//===--- TypeCheckDecl.cpp - Type Checking for Declarations ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements semantic analysis for declarations.
//
//===----------------------------------------------------------------------===//

#include "CodeSynthesis.h"
#include "ConstraintSystem.h"
#include "DerivedConformances.h"
#include "TypeChecker.h"
#include "GenericTypeResolver.h"
#include "MiscDiagnostics.h"
#include "swift/AST/AccessScope.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/ReferencedNameTracker.h"
#include "swift/AST/TypeWalker.h"
#include "swift/Basic/Statistic.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/Parser.h"
#include "swift/Sema/IterativeTypeChecker.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Strings.h"
#include "swift/Basic/Defer.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Compiler.h"

using namespace swift;

#define DEBUG_TYPE "TypeCheckDecl"

namespace {

/// Used during enum raw value checking to identify duplicate raw values.
/// Character, string, float, and integer literals are all keyed by value.
/// Float and integer literals are additionally keyed by numeric equivalence.
struct RawValueKey {
  enum class Kind : uint8_t {
    String, Float, Int, Tombstone, Empty
  } kind;
  
  struct IntValueTy {
    uint64_t v0;
    uint64_t v1;

    IntValueTy(const APInt &bits) {
      APInt bits128 = bits.sextOrTrunc(128);
      assert(bits128.getBitWidth() <= 128);
      const uint64_t *data = bits128.getRawData();
      v0 = data[0];
      v1 = data[1];
    }
  };

  struct FloatValueTy {
    uint64_t v0;
    uint64_t v1;
  };

  // FIXME: doesn't accommodate >64-bit or signed raw integer or float values.
  union {
    StringRef stringValue;
    uint32_t charValue;
    IntValueTy intValue;
    FloatValueTy floatValue;
  };
  
  explicit RawValueKey(LiteralExpr *expr) {
    switch (expr->getKind()) {
    case ExprKind::IntegerLiteral:
      kind = Kind::Int;
      intValue = IntValueTy(cast<IntegerLiteralExpr>(expr)->getValue());
      return;
    case ExprKind::FloatLiteral: {
      APFloat value = cast<FloatLiteralExpr>(expr)->getValue();
      llvm::APSInt asInt(127, /*isUnsigned=*/false);
      bool isExact = false;
      APFloat::opStatus status =
          value.convertToInteger(asInt, APFloat::rmTowardZero, &isExact);
      if (asInt.getBitWidth() <= 128 && status == APFloat::opOK && isExact) {
        kind = Kind::Int;
        intValue = IntValueTy(asInt);
        return;
      }
      APInt bits = value.bitcastToAPInt();
      const uint64_t *data = bits.getRawData();
      if (bits.getBitWidth() == 80) {
        kind = Kind::Float;
        floatValue = FloatValueTy{ data[0], data[1] };
      } else {
        assert(bits.getBitWidth() == 64);
        kind = Kind::Float;
        floatValue = FloatValueTy{ data[0], 0 };
      }
      return;
    }
    case ExprKind::StringLiteral:
      kind = Kind::String;
      stringValue = cast<StringLiteralExpr>(expr)->getValue();
      return;
    default:
      llvm_unreachable("not a valid literal expr for raw value");
    }
  }
  
  explicit RawValueKey(Kind k) : kind(k) {
    assert((k == Kind::Tombstone || k == Kind::Empty)
           && "this ctor is only for creating DenseMap special values");
  }
};
  
/// Used during enum raw value checking to identify the source of a raw value,
/// which may have been derived by auto-incrementing, for diagnostic purposes.
struct RawValueSource {
  /// The decl that has the raw value.
  EnumElementDecl *sourceElt;
  /// If the sourceDecl didn't explicitly name a raw value, this is the most
  /// recent preceding decl with an explicit raw value. This is used to
  /// diagnose 'autoincrementing from' messages.
  EnumElementDecl *lastExplicitValueElt;
};

} // end anonymous namespace

namespace llvm {

template<>
class DenseMapInfo<RawValueKey> {
public:
  static RawValueKey getEmptyKey() {
    return RawValueKey(RawValueKey::Kind::Empty);
  }
  static RawValueKey getTombstoneKey() {
    return RawValueKey(RawValueKey::Kind::Tombstone);
  }
  static unsigned getHashValue(RawValueKey k) {
    switch (k.kind) {
    case RawValueKey::Kind::Float:
      // Hash as bits. We want to treat distinct but IEEE-equal values as not
      // equal.
      return DenseMapInfo<uint64_t>::getHashValue(k.floatValue.v0) ^
             DenseMapInfo<uint64_t>::getHashValue(k.floatValue.v1);
    case RawValueKey::Kind::Int:
      return DenseMapInfo<uint64_t>::getHashValue(k.intValue.v0) &
             DenseMapInfo<uint64_t>::getHashValue(k.intValue.v1);
    case RawValueKey::Kind::String:
      return llvm::HashString(k.stringValue);
    case RawValueKey::Kind::Empty:
    case RawValueKey::Kind::Tombstone:
      return 0;
    }

    llvm_unreachable("Unhandled RawValueKey in switch.");
  }
  static bool isEqual(RawValueKey a, RawValueKey b) {
    if (a.kind != b.kind)
      return false;
    switch (a.kind) {
    case RawValueKey::Kind::Float:
      // Hash as bits. We want to treat distinct but IEEE-equal values as not
      // equal.
      return a.floatValue.v0 == b.floatValue.v0 &&
             a.floatValue.v1 == b.floatValue.v1;
    case RawValueKey::Kind::Int:
      return a.intValue.v0 == b.intValue.v0 &&
             a.intValue.v1 == b.intValue.v1;
    case RawValueKey::Kind::String:
      return a.stringValue.equals(b.stringValue);
    case RawValueKey::Kind::Empty:
    case RawValueKey::Kind::Tombstone:
      return true;
    }

    llvm_unreachable("Unhandled RawValueKey in switch.");
  }
};
  
} // namespace llvm

/// Determine whether the given declaration can inherit a class.
static bool canInheritClass(Decl *decl) {
  // Classes can inherit from a class.
  if (isa<ClassDecl>(decl))
    return true;

  // Generic type parameters can inherit a class.
  if (isa<GenericTypeParamDecl>(decl))
    return true;

  // Associated types can inherit a class.
  if (isa<AssociatedTypeDecl>(decl))
    return true;

  return false;
}

// Add implicit conformances to the given declaration.
static void addImplicitConformances(
              TypeChecker &tc, Decl *decl,
              llvm::SmallSetVector<ProtocolDecl *, 4> &allProtocols) {
  if (auto nominal = dyn_cast<NominalTypeDecl>(decl)) {
    SmallVector<ProtocolDecl *, 2> protocols;
    nominal->getImplicitProtocols(protocols);
    allProtocols.insert(protocols.begin(), protocols.end());
  }
}

/// Check that the declaration attributes are ok.
static void validateAttributes(TypeChecker &TC, Decl *D);

void TypeChecker::resolveSuperclass(ClassDecl *classDecl) {
  IterativeTypeChecker ITC(*this);
  ITC.satisfy(requestTypeCheckSuperclass(classDecl));
}

void TypeChecker::resolveRawType(EnumDecl *enumDecl) {
  IterativeTypeChecker ITC(*this);
  ITC.satisfy(requestTypeCheckRawType(enumDecl));
}

void TypeChecker::validateWhereClauses(ProtocolDecl *protocol,
                                       GenericTypeResolver *resolver) {
  TypeResolutionOptions options;

  if (auto whereClause = protocol->getTrailingWhereClause()) {
    revertGenericRequirements(whereClause->getRequirements());
    validateRequirements(whereClause->getWhereLoc(),
                         whereClause->getRequirements(), protocol,
                         options, resolver);
  }

  for (auto assocType : protocol->getAssociatedTypeMembers()) {
    if (auto whereClause = assocType->getTrailingWhereClause()) {
      revertGenericRequirements(whereClause->getRequirements());
      validateRequirements(whereClause->getWhereLoc(),
                           whereClause->getRequirements(),
                           protocol, options, resolver);
    }
  }
}

void TypeChecker::resolveInheritedProtocols(ProtocolDecl *protocol) {
  IterativeTypeChecker ITC(*this);
  ITC.satisfy(requestInheritedProtocols(protocol));

  ProtocolRequirementTypeResolver resolver;
  validateWhereClauses(protocol, &resolver);
}

void TypeChecker::resolveInheritanceClause(
       llvm::PointerUnion<TypeDecl *, ExtensionDecl *> decl) {
  IterativeTypeChecker ITC(*this);
  unsigned numInherited;
  if (auto ext = decl.dyn_cast<ExtensionDecl *>()) {
    numInherited = ext->getInherited().size();
  } else {
    numInherited = decl.get<TypeDecl *>()->getInherited().size();
  }

  for (unsigned i = 0; i != numInherited; ++i) {
    ITC.satisfy(requestResolveInheritedClauseEntry({ decl, i }));
  }
}

/// check the inheritance clause of a type declaration or extension thereof.
///
/// This routine validates all of the types in the parsed inheritance clause,
/// recording the superclass (if any and if allowed) as well as the protocols
/// to which this type declaration conforms.
void TypeChecker::checkInheritanceClause(Decl *decl,
                                         GenericTypeResolver *resolver) {
  TypeResolutionOptions options;
  DeclContext *DC;
  if (auto nominal = dyn_cast<NominalTypeDecl>(decl)) {
    DC = nominal;
    options |= TypeResolutionFlags::GenericSignature;
    options |= TypeResolutionFlags::InheritanceClause;
    options |= TypeResolutionFlags::AllowUnavailableProtocol;
  } else if (auto ext = dyn_cast<ExtensionDecl>(decl)) {
    DC = ext;
    options |= TypeResolutionFlags::GenericSignature;
    options |= TypeResolutionFlags::InheritanceClause;
    options |= TypeResolutionFlags::AllowUnavailableProtocol;
  } else if (isa<GenericTypeParamDecl>(decl)) {
    // For generic parameters, we want name lookup to look at just the
    // signature of the enclosing entity.
    DC = decl->getDeclContext();
    if (auto nominal = dyn_cast<NominalTypeDecl>(DC)) {
      DC = nominal;
      options |= TypeResolutionFlags::GenericSignature;
    } else if (auto ext = dyn_cast<ExtensionDecl>(DC)) {
      DC = ext;
      options |= TypeResolutionFlags::GenericSignature;
    } else if (auto func = dyn_cast<AbstractFunctionDecl>(DC)) {
      DC = func;
      options |= TypeResolutionFlags::GenericSignature;
    } else if (!DC->isModuleScopeContext()) {
      // Skip the generic parameter's context entirely.
      DC = DC->getParent();
    }
  } else {
    DC = decl->getDeclContext();
  }

  // Establish a default generic type resolver.
  GenericTypeToArchetypeResolver defaultResolver(decl->getInnermostDeclContext());
  if (!resolver)
    resolver = &defaultResolver;

  MutableArrayRef<TypeLoc> inheritedClause;

  // If we already checked the inheritance clause, don't do so again.
  if (auto type = dyn_cast<TypeDecl>(decl)) {
    if (type->checkedInheritanceClause())
      return;

    // This breaks infinite recursion, which will be diagnosed separately.
    type->setCheckedInheritanceClause();
    inheritedClause = type->getInherited();
  } else {
    auto ext = cast<ExtensionDecl>(decl);

    validateExtension(ext);

    if (ext->isInvalid() ||
        ext->checkedInheritanceClause())
      return;

    // This breaks infinite recursion, which will be diagnosed separately.
    ext->setCheckedInheritanceClause();
    inheritedClause = ext->getInherited();

    // Protocol extensions cannot have inheritance clauses.
    if (ext->getExtendedType()->is<ProtocolType>()) {
      if (!inheritedClause.empty()) {
        diagnose(ext->getLoc(), diag::extension_protocol_inheritance,
                 ext->getExtendedType())
          .highlight(SourceRange(inheritedClause.front().getSourceRange().Start,
                                 inheritedClause.back().getSourceRange().End));
        ext->setInherited({ });
        return;
      }
    }
  }

  // Retrieve the location of the start of the inheritance clause.
  auto getStartLocOfInheritanceClause = [&] {
    if (auto genericTypeDecl = dyn_cast<GenericTypeDecl>(decl)) {
      if (auto genericParams = genericTypeDecl->getGenericParams())
        return genericParams->getSourceRange().End;

      return genericTypeDecl->getNameLoc();
    }

    if (auto typeDecl = dyn_cast<TypeDecl>(decl))
      return typeDecl->getNameLoc();

    if (auto ext = dyn_cast<ExtensionDecl>(decl))
      return ext->getSourceRange().End;

    return SourceLoc();
  };

  // Compute the source range to be used when removing something from an
  // inheritance clause.
  auto getRemovalRange = [&](unsigned i) {
    // If there is just one entry, remove the entire inheritance clause.
    if (inheritedClause.size() == 1) {
      SourceLoc start = getStartLocOfInheritanceClause();
      SourceLoc end = inheritedClause[i].getSourceRange().End;
      return SourceRange(Lexer::getLocForEndOfToken(Context.SourceMgr, start),
                         Lexer::getLocForEndOfToken(Context.SourceMgr, end));
    }

    // If we're at the first entry, remove from the start of this entry to the
    // start of the next entry.
    if (i == 0) {
      return SourceRange(inheritedClause[i].getSourceRange().Start,
                         inheritedClause[i+1].getSourceRange().Start);
    }

    // Otherwise, remove from the end of the previous entry to the end of this
    // entry.
    SourceLoc afterPriorLoc =
      Lexer::getLocForEndOfToken(Context.SourceMgr,
                                 inheritedClause[i-1].getSourceRange().End);

    SourceLoc afterMyEndLoc =
      Lexer::getLocForEndOfToken(Context.SourceMgr,
                                 inheritedClause[i].getSourceRange().End);

    return SourceRange(afterPriorLoc, afterMyEndLoc);
  };

  // Check all of the types listed in the inheritance clause.
  Type superclassTy;
  SourceRange superclassRange;
  llvm::SmallSetVector<ProtocolDecl *, 4> allProtocols;
  llvm::SmallDenseMap<CanType, std::pair<unsigned, SourceRange>> inheritedTypes;
  addImplicitConformances(*this, decl, allProtocols);
  for (unsigned i = 0, n = inheritedClause.size(); i != n; ++i) {
    auto &inherited = inheritedClause[i];

    // Validate the type.
    if (validateType(inherited, DC, options, resolver)) {
      inherited.setInvalidType(Context);
      continue;
    }

    auto inheritedTy = inherited.getType();

    // If this is an error type, ignore it.
    if (inheritedTy->hasError())
      continue;

    // Retrieve the interface type for this inherited type.
    //
    // If we have a generic parameter, mapTypeOutOfContext() might not
    // work yet, if we're calling this while building the generic
    // signature. However, we're also not storing inheritedTy back
    // anywhere, so it's OK to leave it as an archetype.
    //
    // FIXME: Ideally, we wouldn't have code paths that take a mix
    // of archetypes and interface types. Other than generic parameters,
    // the only time we get an interface type here is with invalid
    // circular cases. That should be diagnosed elsewhere.
    if (inheritedTy->hasArchetype() && !isa<GenericTypeParamDecl>(decl))
      inheritedTy = inheritedTy->mapTypeOutOfContext();

    // Check whether we inherited from the same type twice.
    CanType inheritedCanTy = inheritedTy->getCanonicalType();
    auto knownType = inheritedTypes.find(inheritedCanTy);
    if (knownType != inheritedTypes.end()) {
      // If the duplicated type is 'AnyObject', check whether the first was
      // written as 'class'. Downgrade the error to a warning in such cases
      // for backward compatibility with Swift <= 4.
      if (!Context.LangOpts.isSwiftVersionAtLeast(5) &&
          inheritedTy->isAnyObject() &&
          (isa<ProtocolDecl>(decl) || isa<AbstractTypeParamDecl>(decl)) &&
          Lexer::getTokenAtLocation(Context.SourceMgr,
                                    knownType->second.second.Start)
            .is(tok::kw_class)) {
        SourceLoc classLoc = knownType->second.second.Start;
        SourceRange removeRange = getRemovalRange(knownType->second.first);

        diagnose(classLoc, diag::duplicate_anyobject_class_inheritance)
          .fixItRemoveChars(removeRange.Start, removeRange.End);
        inherited.setInvalidType(Context);
        continue;
      }

      auto removeRange = getRemovalRange(i);
      diagnose(inherited.getSourceRange().Start,
               diag::duplicate_inheritance, inheritedTy)
        .fixItRemoveChars(removeRange.Start, removeRange.End)
        .highlight(knownType->second.second);
      inherited.setInvalidType(Context);
      continue;
    }
    inheritedTypes[inheritedCanTy] = { i, inherited.getSourceRange() };

    // If this is a protocol or protocol composition type, record the
    // protocols.
    if (inheritedTy->isExistentialType()) {
      auto layout = inheritedTy->getExistentialLayout();

      // Protocols, generic parameters and associated types can inherit
      // from subclass existentials, which are "exploded" into their
      // corresponding requirements.
      if (isa<ProtocolDecl>(decl) ||
          isa<AbstractTypeParamDecl>(decl) ||
          (!layout.hasExplicitAnyObject &&
           !layout.superclass)) {
        for (auto proto : layout.getProtocols()) {
          auto *protoDecl = proto->getDecl();
          allProtocols.insert(protoDecl);
        }
        continue;
      }

      // Classes can inherit from subclass existentials as long as they
      // do not contain an explicit AnyObject member.
      if (isa<ClassDecl>(decl) &&
          !layout.hasExplicitAnyObject) {
        for (auto proto : layout.getProtocols()) {
          auto *protoDecl = proto->getDecl();
          allProtocols.insert(protoDecl);
        }

        // Superclass inheritance is handled below.
        inheritedTy = layout.superclass;
        if (!inheritedTy)
          continue;
      }

      // Swift 3 compatibility -- a class inheriting from AnyObject is a no-op.
      if (Context.LangOpts.isSwiftVersion3() && isa<ClassDecl>(decl) &&
          inheritedTy->isAnyObject()) {
        auto classDecl = cast<ClassDecl>(decl);
        auto removeRange = getRemovalRange(i);
        diagnose(inherited.getSourceRange().Start,
                 diag::class_inherits_anyobject,
                 classDecl->getDeclaredInterfaceType())
          .fixItRemoveChars(removeRange.Start, removeRange.End);
        continue;
      }
    }
    
    // If this is an enum inheritance clause, check for a raw type.
    if (isa<EnumDecl>(decl)) {
      // Check if we already had a raw type.
      if (superclassTy) {
        diagnose(inherited.getSourceRange().Start,
                 diag::multiple_enum_raw_types, superclassTy, inheritedTy)
          .highlight(superclassRange);
        inherited.setInvalidType(Context);
        continue;
      }
      
      // If this is not the first entry in the inheritance clause, complain.
      if (i > 0) {
        auto removeRange = getRemovalRange(i);

        diagnose(inherited.getSourceRange().Start,
                 diag::raw_type_not_first, inheritedTy)
          .fixItRemoveChars(removeRange.Start, removeRange.End)
          .fixItInsert(inheritedClause[0].getSourceRange().Start,
                       inheritedTy.getString() + ", ");

        // Fall through to record the raw type.
      }

      // Record the raw type.
      superclassTy = inheritedTy;
      superclassRange = inherited.getSourceRange();
      
      // Add the RawRepresentable conformance implied by the raw type.
      allProtocols.insert(getProtocol(decl->getLoc(),
                                      KnownProtocolKind::RawRepresentable));
      continue;
    }

    // If this is a class type, it may be the superclass.
    if (inheritedTy->getClassOrBoundGenericClass()) {
      // First, check if we already had a superclass.
      if (superclassTy) {
        // FIXME: Check for shadowed protocol names, i.e., NSObject?

        // Complain about multiple inheritance.
        // Don't emit a Fix-It here. The user has to think harder about this.
        diagnose(inherited.getSourceRange().Start,
                 diag::multiple_inheritance, superclassTy, inheritedTy)
          .highlight(superclassRange);
        inherited.setInvalidType(Context);
        continue;
      }

      // If the declaration we're looking at doesn't allow a superclass,
      // complain.
      if (!canInheritClass(decl)) {
        diagnose(decl->getLoc(),
                 isa<ExtensionDecl>(decl)
                   ? diag::extension_class_inheritance
                   : diag::non_class_inheritance,
                 isa<ExtensionDecl>(decl)
                   ? cast<ExtensionDecl>(decl)->getDeclaredInterfaceType()
                   : cast<TypeDecl>(decl)->getDeclaredInterfaceType(),
                 inheritedTy)
          .highlight(inherited.getSourceRange());
        inherited.setInvalidType(Context);
        continue;
      }

      // If this is not the first entry in the inheritance clause, complain.
      if (i > 0) {
        auto removeRange = getRemovalRange(i);
        diagnose(inherited.getSourceRange().Start,
                 diag::superclass_not_first, inheritedTy)
          .fixItRemoveChars(removeRange.Start, removeRange.End)
          .fixItInsert(inheritedClause[0].getSourceRange().Start,
                       inheritedTy.getString() + ", ");

        // Fall through to record the superclass.
      }

      // Record the superclass.
      superclassTy = inheritedTy;
      superclassRange = inherited.getSourceRange();
      continue;
    }

    // We can't inherit from a non-class, non-protocol type.
    diagnose(decl->getLoc(),
             canInheritClass(decl)
               ? diag::inheritance_from_non_protocol_or_class
               : diag::inheritance_from_non_protocol,
             inheritedTy);
    // FIXME: Note pointing to the declaration 'inheritedTy' references?
    inherited.setInvalidType(Context);
  }

  if (auto proto = dyn_cast<ProtocolDecl>(decl)) {
    // Check for circular inheritance.
    // FIXME: The diagnostics here should be improved.
    bool diagnosedCircularity = false;
    for (unsigned i = 0, n = allProtocols.size(); i != n; /*in loop*/) {
      if (allProtocols[i] == proto || allProtocols[i]->inheritsFrom(proto)) {
        if (!diagnosedCircularity) {
          diagnose(proto, diag::circular_protocol_def, proto->getName().str());
          diagnosedCircularity = true;
        }

        allProtocols.remove(allProtocols[i]);
        --n;
        continue;
      }

      ++i;
    }
  }
  // Set the superclass.
  else if (auto classDecl = dyn_cast<ClassDecl>(decl)) {
    classDecl->setSuperclass(superclassTy);
  } else if (auto enumDecl = dyn_cast<EnumDecl>(decl)) {
    enumDecl->setRawType(superclassTy);
  } else {
    assert(!superclassTy || isa<AbstractTypeParamDecl>(decl));
  }
}

/// Retrieve the set of protocols the given protocol inherits.
static llvm::TinyPtrVector<ProtocolDecl *>
getInheritedForCycleCheck(TypeChecker &tc,
                          ProtocolDecl *proto,
                          ProtocolDecl **scratch) {
  return tc.getDirectConformsTo(proto);
}

/// Retrieve the superclass of the given class.
static ArrayRef<ClassDecl *> getInheritedForCycleCheck(TypeChecker &tc,
                                                       ClassDecl *classDecl,
                                                       ClassDecl **scratch) {
  tc.checkInheritanceClause(classDecl);

  if (classDecl->hasSuperclass()) {
    *scratch = classDecl->getSuperclass()->getClassOrBoundGenericClass();
    return *scratch;
  }
  return { };
}

/// Retrieve the raw type of the given enum.
static ArrayRef<EnumDecl *> getInheritedForCycleCheck(TypeChecker &tc,
                                                      EnumDecl *enumDecl,
                                                      EnumDecl **scratch) {
  tc.checkInheritanceClause(enumDecl);
  
  if (enumDecl->hasRawType()) {
    *scratch = enumDecl->getRawType()->getEnumOrBoundGenericEnum();
    return *scratch ? ArrayRef<EnumDecl*>(*scratch) : ArrayRef<EnumDecl*>{};
  }
  return { };
}

// Break the inheritance cycle for a protocol by removing all inherited
// protocols.
//
// FIXME: Just remove the problematic inheritance?
static void breakInheritanceCycle(ProtocolDecl *proto) {
}

/// Break the inheritance cycle for a class by removing its superclass.
static void breakInheritanceCycle(ClassDecl *classDecl) {
  classDecl->setSuperclass(Type());
}

/// Break the inheritance cycle for an enum by removing its raw type.
static void breakInheritanceCycle(EnumDecl *enumDecl) {
  enumDecl->setRawType(Type());
}

/// Check for circular inheritance.
template<typename T>
static void checkCircularity(TypeChecker &tc, T *decl,
                             Diag<StringRef> circularDiag,
                             Diag<Identifier> declHereDiag,
                             SmallVectorImpl<T *> &path) {
  switch (decl->getCircularityCheck()) {
  case CircularityCheck::Checked:
    return;

  case CircularityCheck::Checking: {
    // We're already checking this type, which means we have a cycle.

    // The beginning of the path might not be part of the cycle, so find
    // where the cycle starts.
    assert(!path.empty());

    auto cycleStart = path.end() - 1;
    while (*cycleStart != decl) {
      assert(cycleStart != path.begin() && "Missing cycle start?");
      --cycleStart;
    }

    // If the path length is 1 the type directly references itself.
    if (path.end() - cycleStart == 1) {
      tc.diagnose(path.back()->getLoc(),
                  circularDiag,
                  path.back()->getName().str());

      decl->setInvalid();
      decl->setInterfaceType(ErrorType::get(tc.Context));
      breakInheritanceCycle(decl);
      break;
    }

    // Form the textual path illustrating the cycle.
    llvm::SmallString<128> pathStr;
    for (auto i = cycleStart, iEnd = path.end(); i != iEnd; ++i) {
      if (!pathStr.empty())
        pathStr += " -> ";
      pathStr += ("'" + (*i)->getName().str() + "'").str();
    }
    pathStr += (" -> '" + decl->getName().str() + "'").str();

    // Diagnose the cycle.
    tc.diagnose(decl->getLoc(), circularDiag, pathStr);
    for (auto i = cycleStart + 1, iEnd = path.end(); i != iEnd; ++i) {
      tc.diagnose(*i, declHereDiag, (*i)->getName());
    }

    // Set this declaration as invalid, then break the cycle somehow.
    decl->setInvalid();
    decl->setInterfaceType(ErrorType::get(tc.Context));
    breakInheritanceCycle(decl);
    break;
  }

  case CircularityCheck::Unchecked: {
    // Walk to the inherited class or protocols.
    path.push_back(decl);
    decl->setCircularityCheck(CircularityCheck::Checking);
    T *scratch = nullptr;
    for (auto inherited : getInheritedForCycleCheck(tc, decl, &scratch)) {
      checkCircularity(tc, inherited, circularDiag, declHereDiag, path);
    }
    decl->setCircularityCheck(CircularityCheck::Checked);
    path.pop_back();
    break;
  }
  }
}

/// Set each bound variable in the pattern to have an error type.
static void setBoundVarsTypeError(Pattern *pattern, ASTContext &ctx) {
  pattern->forEachVariable([&](VarDecl *var) {
    // Don't change the type of a variable that we've been able to
    // compute a type for.
    if (var->hasType() && !var->getType()->hasError())
      return;

    var->markInvalid();
  });
}

/// Expose TypeChecker's handling of GenericParamList to SIL parsing.
GenericEnvironment *
TypeChecker::handleSILGenericParams(GenericParamList *genericParams,
                                    DeclContext *DC) {

  SmallVector<GenericParamList *, 2> nestedList;
  for (; genericParams; genericParams = genericParams->getOuterParameters()) {
    nestedList.push_back(genericParams);
  }

  // Since the innermost GenericParamList is in the beginning of the vector,
  // we process in reverse order to handle the outermost list first.
  GenericSignature *parentSig = nullptr;
  GenericEnvironment *parentEnv = nullptr;

  for (unsigned i = 0, e = nestedList.size(); i < e; i++) {
    auto genericParams = nestedList.rbegin()[i];
    prepareGenericParamList(genericParams, DC);

    parentEnv = checkGenericEnvironment(genericParams, DC, parentSig,
                                        /*allowConcreteGenericParams=*/true,
                                        /*ext=*/nullptr);
    parentSig = parentEnv->getGenericSignature();

    // Compute the final set of archetypes.
    revertGenericParamList(genericParams);
    GenericTypeToArchetypeResolver archetypeResolver(parentEnv);
    checkGenericParamList(nullptr, genericParams, parentSig,
                          &archetypeResolver);
  }

  return parentEnv;
}

/// Build a default initializer for the given type.
static Expr *buildDefaultInitializer(TypeChecker &tc, Type type) {
  // Default-initialize optional types and weak values to 'nil'.
  if (type->getReferenceStorageReferent()->getOptionalObjectType())
    return new (tc.Context) NilLiteralExpr(SourceLoc(), /*Implicit=*/true);

  // Build tuple literals for tuple types.
  if (auto tupleType = type->getAs<TupleType>()) {
    SmallVector<Expr *, 2> inits;
    for (const auto &elt : tupleType->getElements()) {
      if (elt.isVararg())
        return nullptr;

      auto eltInit = buildDefaultInitializer(tc, elt.getType());
      if (!eltInit)
        return nullptr;

      inits.push_back(eltInit);
    }

    return TupleExpr::createImplicit(tc.Context, inits, { });
  }

  // We don't default-initialize anything else.
  return nullptr;
}

/// Check whether \c current is a redeclaration.
static void checkRedeclaration(TypeChecker &tc, ValueDecl *current) {
  // If we've already checked this declaration, don't do it again.
  if (current->alreadyCheckedRedeclaration())
    return;

  // If there's no type yet, come back to it later.
  if (!current->hasInterfaceType())
    return;
  
  // Make sure we don't do this checking again.
  current->setCheckedRedeclaration(true);

  // Ignore invalid and anonymous declarations.
  if (current->isInvalid() || !current->hasName())
    return;

  // If this declaration isn't from a source file, don't check it.
  // FIXME: Should restrict this to the source file we care about.
  DeclContext *currentDC = current->getDeclContext();
  SourceFile *currentFile = currentDC->getParentSourceFile();
  if (!currentFile || currentDC->isLocalContext())
    return;

  ReferencedNameTracker *tracker = currentFile->getReferencedNameTracker();
  bool isCascading = true;
  if (current->hasAccess())
    isCascading = (current->getFormalAccess() > AccessLevel::FilePrivate);

  // Find other potential definitions.
  SmallVector<ValueDecl *, 4> otherDefinitions;
  if (currentDC->isTypeContext()) {
    // Look within a type context.
    if (auto nominal = currentDC->getAsNominalTypeOrNominalTypeExtensionContext()) {
      auto found = nominal->lookupDirect(current->getBaseName());
      otherDefinitions.append(found.begin(), found.end());
      if (tracker)
        tracker->addUsedMember({nominal, current->getBaseName()}, isCascading);
    }
  } else {
    // Look within a module context.
    currentFile->getParentModule()->lookupValue({ }, current->getBaseName(),
                                                NLKind::QualifiedLookup,
                                                otherDefinitions);
    if (tracker)
      tracker->addTopLevelName(current->getBaseName(), isCascading);
  }

  // Compare this signature against the signature of other
  // declarations with the same name.
  OverloadSignature currentSig = current->getOverloadSignature();
  CanType currentSigType = current->getOverloadSignatureType();
  ModuleDecl *currentModule = current->getModuleContext();
  for (auto other : otherDefinitions) {
    // Skip invalid declarations and ourselves.
    if (current == other || other->isInvalid())
      continue;

    // Skip declarations in other modules.
    if (currentModule != other->getModuleContext())
      continue;

    // Don't compare methods vs. non-methods (which only happens with
    // operators).
    if (currentDC->isTypeContext() != other->getDeclContext()->isTypeContext())
      continue;

    // Check whether the overload signatures conflict (ignoring the type for
    // now).
    auto otherSig = other->getOverloadSignature();
    if (!conflicting(currentSig, otherSig))
      continue;

    // Validate the declaration but only if it came from a different context.
    if (other->getDeclContext() != current->getDeclContext())
      tc.validateDecl(other);

    // Skip invalid or not yet seen declarations.
    if (other->isInvalid() || !other->hasInterfaceType())
      continue;

    // Skip declarations in other files.
    // In practice, this means we will warn on a private declaration that
    // shadows a non-private one, but only in the file where the shadowing
    // happens. We will warn on conflicting non-private declarations in both
    // files.
    if (!other->isAccessibleFrom(currentDC))
      continue;

    const auto markInvalid = [&current, &tc]() {
      current->setInvalid();
      if (auto *varDecl = dyn_cast<VarDecl>(current))
        if (varDecl->hasType())
          varDecl->setType(ErrorType::get(tc.Context));
      if (current->hasInterfaceType())
        current->setInterfaceType(ErrorType::get(tc.Context));
    };

    // Thwart attempts to override the same declaration more than once.
    const auto *currentOverride = current->getOverriddenDecl();
    const auto *otherOverride = other->getOverriddenDecl();
    if (currentOverride && currentOverride == otherOverride) {
      tc.diagnose(current, diag::multiple_override, current->getFullName());
      tc.diagnose(other, diag::multiple_override_prev, other->getFullName());
      markInvalid();
      break;
    }

    // Get the overload signature type.
    CanType otherSigType = other->getOverloadSignatureType();

    bool wouldBeSwift5Redeclaration = false;
    auto isRedeclaration = conflicting(tc.Context, currentSig, currentSigType,
                                       otherSig, otherSigType,
                                       &wouldBeSwift5Redeclaration);
    // If there is another conflict, complain.
    if (isRedeclaration || wouldBeSwift5Redeclaration) {
      // If the two declarations occur in the same source file, make sure
      // we get the diagnostic ordering to be sensible.
      if (auto otherFile = other->getDeclContext()->getParentSourceFile()) {
        if (currentFile == otherFile &&
            current->getLoc().isValid() &&
            other->getLoc().isValid() &&
            tc.Context.SourceMgr.isBeforeInBuffer(current->getLoc(),
                                                  other->getLoc())) {
          std::swap(current, other);
        }
      }

      // If we're currently looking at a .sil and the conflicting declaration
      // comes from a .sib, don't error since we won't be considering the sil
      // from the .sib. So it's fine for the .sil to shadow it, since that's the
      // one we want.
      if (currentFile->Kind == SourceFileKind::SIL) {
        auto *otherFile = dyn_cast<SerializedASTFile>(
            other->getDeclContext()->getModuleScopeContext());
        if (otherFile && otherFile->isSIB())
          continue;
      }

      // If the conflicting declarations have non-overlapping availability and,
      // we allow the redeclaration to proceed if...
      //
      // - they are initializers with different failability,
      bool isAcceptableVersionBasedChange = false;
      {
        const auto *currentInit = dyn_cast<ConstructorDecl>(current);
        const auto *otherInit = dyn_cast<ConstructorDecl>(other);
        if (currentInit && otherInit &&
            ((currentInit->getFailability() == OTK_None) !=
             (otherInit->getFailability() == OTK_None))) {
          isAcceptableVersionBasedChange = true;
        }
      }
      // - one throws and the other does not,
      {
        const auto *currentAFD = dyn_cast<AbstractFunctionDecl>(current);
        const auto *otherAFD = dyn_cast<AbstractFunctionDecl>(other);
        if (currentAFD && otherAFD &&
            currentAFD->hasThrows() != otherAFD->hasThrows()) {
          isAcceptableVersionBasedChange = true;
        }
      }
      // - or they are computed properties of different types,
      {
        const auto *currentVD = dyn_cast<VarDecl>(current);
        const auto *otherVD = dyn_cast<VarDecl>(other);
        if (currentVD && otherVD &&
            !currentVD->hasStorage() &&
            !otherVD->hasStorage() &&
            !currentVD->getInterfaceType()->isEqual(
              otherVD->getInterfaceType())) {
          isAcceptableVersionBasedChange = true;
        }
      }

      if (isAcceptableVersionBasedChange) {
        class AvailabilityRange {
          Optional<clang::VersionTuple> introduced;
          Optional<clang::VersionTuple> obsoleted;

        public:
          static AvailabilityRange from(const ValueDecl *VD) {
            AvailabilityRange result;
            for (auto *attr : VD->getAttrs().getAttributes<AvailableAttr>()) {
              if (attr->PlatformAgnostic ==
                    PlatformAgnosticAvailabilityKind::SwiftVersionSpecific) {
                if (attr->Introduced)
                  result.introduced = attr->Introduced;
                if (attr->Obsoleted)
                  result.obsoleted = attr->Obsoleted;
              }
            }
            return result;
          }

          bool fullyPrecedes(const AvailabilityRange &other) const {
            if (!obsoleted.hasValue())
              return false;
            if (!other.introduced.hasValue())
              return false;
            return *obsoleted <= *other.introduced;
          }

          bool overlaps(const AvailabilityRange &other) const {
            return !fullyPrecedes(other) && !other.fullyPrecedes(*this);
          }
        };

        auto currentAvail = AvailabilityRange::from(current);
        auto otherAvail = AvailabilityRange::from(other);
        if (!currentAvail.overlaps(otherAvail))
          continue;
      }

      // If this isn't a redeclaration in the current version of Swift, but
      // would be in Swift 5 mode, emit a warning instead of an error.
      if (wouldBeSwift5Redeclaration) {
        tc.diagnose(current, diag::invalid_redecl_swift5_warning,
                    current->getFullName());
        tc.diagnose(other, diag::invalid_redecl_prev, other->getFullName());
      } else {
        tc.diagnose(current, diag::invalid_redecl, current->getFullName());
        tc.diagnose(other, diag::invalid_redecl_prev, other->getFullName());
        markInvalid();
      }

      // Make sure we don't do this checking again for the same decl. We also
      // set this at the beginning of the function, but we might have swapped
      // the decls for diagnostics; so ensure we also set this for the actual
      // decl we diagnosed on.
      current->setCheckedRedeclaration(true);
      break;
    }
  }
}

/// Does the context allow pattern bindings that don't bind any variables?
static bool contextAllowsPatternBindingWithoutVariables(DeclContext *dc) {
  
  // Property decls in type context must bind variables.
  if (dc->isTypeContext())
    return false;
  
  // Global variable decls must bind variables, except in scripts.
  if (dc->isModuleScopeContext()) {
    if (dc->getParentSourceFile()
        && dc->getParentSourceFile()->isScriptMode())
      return true;
    
    return false;
  }
  
  return true;
}

/// Validate the \c entryNumber'th entry in \c binding.
static void validatePatternBindingEntry(TypeChecker &tc,
                                        PatternBindingDecl *binding,
                                        unsigned entryNumber) {
  // If the pattern already has a type, we're done.
  if (binding->getPattern(entryNumber)->hasType())
    return;

  // Resolve the pattern.
  auto *pattern = tc.resolvePattern(binding->getPattern(entryNumber),
                                    binding->getDeclContext(),
                                    /*isStmtCondition*/true);
  if (!pattern) {
    binding->setInvalid();
    binding->getPattern(entryNumber)->setType(ErrorType::get(tc.Context));
    return;
  }

  binding->setPattern(entryNumber, pattern,
                      binding->getPatternList()[entryNumber].getInitContext());

  // Validate 'static'/'class' on properties in nominal type decls.
  auto StaticSpelling = binding->getStaticSpelling();
  if (StaticSpelling != StaticSpellingKind::None &&
      binding->getDeclContext()->isExtensionContext()) {
    if (auto *NTD = binding->getDeclContext()
        ->getAsNominalTypeOrNominalTypeExtensionContext()) {
      if (!isa<ClassDecl>(NTD)) {
        if (StaticSpelling == StaticSpellingKind::KeywordClass) {
          tc.diagnose(binding, diag::class_var_not_in_class)
            .fixItReplace(binding->getStaticLoc(), "static");
          tc.diagnose(NTD, diag::extended_type_declared_here);
        }
      }
    }
  }

  // Check the pattern. We treat type-checking a PatternBindingDecl like
  // type-checking an expression because that's how the initial binding is
  // checked, and they have the same effect on the file's dependencies.
  //
  // In particular, it's /not/ correct to check the PBD's DeclContext because
  // top-level variables in a script file are accessible from other files,
  // even though the PBD is inside a TopLevelCodeDecl.
  TypeResolutionOptions options = TypeResolutionFlags::InExpression;

  options |= TypeResolutionFlags::AllowIUO;
  if (binding->getInit(entryNumber)) {
    // If we have an initializer, we can also have unknown types.
    options |= TypeResolutionFlags::AllowUnspecifiedTypes;
    options |= TypeResolutionFlags::AllowUnboundGenerics;
  }
  if (tc.typeCheckPattern(pattern, binding->getDeclContext(), options)) {
    setBoundVarsTypeError(pattern, tc.Context);
    binding->setInvalid();
    pattern->setType(ErrorType::get(tc.Context));
    return;
  }

  // If the pattern didn't get a type or if it contains an unbound generic type,
  // we'll need to check the initializer.
  if (!pattern->hasType() || pattern->getType()->hasUnboundGenericType()) {
    bool skipApplyingSolution = false;
    if (auto var = binding->getSingleVar())
      skipApplyingSolution = var->getAttrs().hasAttribute<LazyAttr>();

    if (tc.typeCheckPatternBinding(binding, entryNumber, skipApplyingSolution))
      return;
  }

  // If the pattern binding appears in a type or library file context, then
  // it must bind at least one variable.
  if (!contextAllowsPatternBindingWithoutVariables(binding->getDeclContext())) {
    llvm::SmallVector<VarDecl*, 2> vars;
    binding->getPattern(entryNumber)->collectVariables(vars);
    if (vars.empty()) {
      // Selector for error message.
      enum : unsigned {
        Property,
        GlobalVariable,
      };
      tc.diagnose(binding->getPattern(entryNumber)->getLoc(),
                  diag::pattern_binds_no_variables,
                  binding->getDeclContext()->isTypeContext()
                                                   ? Property : GlobalVariable);
    }
  }

  // If we have any type-adjusting attributes, apply them here.
  if (binding->getPattern(entryNumber)->hasType())
    if (auto var = binding->getSingleVar())
      tc.checkTypeModifyingDeclAttributes(var);
}

/// Validate the entries in the given pattern binding declaration.
static void validatePatternBindingEntries(TypeChecker &tc,
                                          PatternBindingDecl *binding) {
  if (binding->hasValidationStarted())
    return;

  binding->setIsBeingValidated();
  SWIFT_DEFER { binding->setIsBeingValidated(false); };

  for (unsigned i = 0, e = binding->getNumPatternEntries(); i != e; ++i)
    validatePatternBindingEntry(tc, binding, i);
}

void swift::makeFinal(ASTContext &ctx, ValueDecl *D) {
  if (D && !D->isFinal()) {
    assert(isa<ClassDecl>(D) || D->isPotentiallyOverridable());
    D->getAttrs().add(new (ctx) FinalAttr(/*IsImplicit=*/true));
  }
}

void swift::makeDynamic(ASTContext &ctx, ValueDecl *D) {
  if (D && !D->isDynamic()) {
    D->getAttrs().add(new (ctx) DynamicAttr(/*IsImplicit=*/true));
  }
}

namespace {
// The raw values of this enum must be kept in sync with
// diag::implicitly_final_cannot_be_open.
enum class ImplicitlyFinalReason : unsigned {
  /// A property was declared with 'let'.
  Let,
  /// The containing class is final.
  FinalClass,
  /// A member was declared as 'static'.
  Static
};
}

static void inferFinalAndDiagnoseIfNeeded(TypeChecker &TC, ValueDecl *D,
                                          StaticSpellingKind staticSpelling) {
  auto cls = D->getDeclContext()->getAsClassOrClassExtensionContext();
  if (!cls)
    return;

  // Are there any reasons to infer 'final'? Prefer 'static' over the class
  // being final for the purposes of diagnostics.
  Optional<ImplicitlyFinalReason> reason;
  if (staticSpelling == StaticSpellingKind::KeywordStatic) {
    reason = ImplicitlyFinalReason::Static;

    if (auto finalAttr = D->getAttrs().getAttribute<FinalAttr>()) {
      auto finalRange = finalAttr->getRange();
      if (finalRange.isValid()) {
        TC.diagnose(finalRange.Start, diag::static_decl_already_final)
        .fixItRemove(finalRange);
      }
    }
  } else if (cls->isFinal()) {
    reason = ImplicitlyFinalReason::FinalClass;
  }

  if (!reason)
    return;

  if (D->getFormalAccess() == AccessLevel::Open) {
    auto diagID = diag::implicitly_final_cannot_be_open;
    if (!TC.Context.isSwiftVersionAtLeast(5))
      diagID = diag::implicitly_final_cannot_be_open_swift4;
    auto inFlightDiag = TC.diagnose(D, diagID,
                                    static_cast<unsigned>(reason.getValue()));
    fixItAccess(inFlightDiag, D, AccessLevel::Public);
  }

  makeFinal(TC.Context, D);
}

/// Configure the implicit 'self' parameter of a function, setting its type,
/// pattern, etc.
///
/// \param func The function whose 'self' is being configured.
static void configureImplicitSelf(TypeChecker &tc,
                                  AbstractFunctionDecl *func) {
  auto selfDecl = func->getImplicitSelfDecl();

  // Compute the type of self.
  auto selfParam = computeSelfParam(func, /*isInitializingCtor*/true,
                                    /*wantDynamicSelf*/true);
  assert(selfDecl && selfParam.getPlainType() && "Not a method");

  // 'self' is 'let' for reference types (i.e., classes) or when 'self' is
  // neither inout.
  auto specifier = selfParam.getParameterFlags().isInOut()
                       ? VarDecl::Specifier::InOut
                       : VarDecl::Specifier::Default;
  selfDecl->setSpecifier(specifier);

  selfDecl->setInterfaceType(selfParam.getPlainType());
}

/// Record the context type of 'self' after the generic environment of
/// the function has been determined.
static void recordSelfContextType(AbstractFunctionDecl *func) {
  auto selfDecl = func->getImplicitSelfDecl();
  auto selfParam = computeSelfParam(func, /*isInitializingCtor*/true,
                                    /*wantDynamicSelf*/true);

  auto selfTy = func->mapTypeIntoContext(selfParam.getType());
  if (selfParam.getParameterFlags().isInOut()) {
    selfDecl->setSpecifier(VarDecl::Specifier::InOut);
  }
  selfDecl->setType(selfTy->getInOutObjectType());
}

namespace {

class AccessScopeChecker {
  const SourceFile *File;
  TypeChecker::TypeAccessScopeCacheMap &Cache;

protected:
  ASTContext &Context;
  Optional<AccessScope> Scope = AccessScope::getPublic();

  AccessScopeChecker(const DeclContext *useDC,
                     decltype(TypeChecker::TypeAccessScopeCache) &caches)
      : File(useDC->getParentSourceFile()),
        Cache(caches[File]),
        Context(File->getASTContext()) {}

  bool visitDecl(ValueDecl *VD) {
    if (!VD || isa<GenericTypeParamDecl>(VD))
      return true;

    // FIXME: Figure out why AssociatedTypeDecls don't always have an access
    // level here.
    if (!VD->hasAccess()) {
      if (isa<AssociatedTypeDecl>(VD))
        return true;
    }

    auto cached = Cache.find(VD);
    if (cached != Cache.end()) {
      Scope = Scope->intersectWith(cached->second);
      return Scope.hasValue();
    }

    auto AS = VD->getFormalAccessScope(File);
    auto result = Cache.insert(std::make_pair(VD, AS));
    assert(result.second);
    (void) result;

    Scope = Scope->intersectWith(AS);
    return Scope.hasValue();
  }
};

class TypeReprAccessScopeChecker : private ASTWalker, AccessScopeChecker {
  TypeReprAccessScopeChecker(const DeclContext *useDC,
                             decltype(TypeChecker::TypeAccessScopeCache) &caches)
      : AccessScopeChecker(useDC, caches) {
  }

  bool walkToTypeReprPre(TypeRepr *TR) override {
    if (auto CITR = dyn_cast<ComponentIdentTypeRepr>(TR))
      return visitDecl(CITR->getBoundDecl());
    return true;
  }

  bool walkToTypeReprPost(TypeRepr *TR) override {
    return Scope.hasValue();
  }

public:
  static Optional<AccessScope>
  getAccessScope(TypeRepr *TR, const DeclContext *useDC,
                 decltype(TypeChecker::TypeAccessScopeCache) &caches) {
    TypeReprAccessScopeChecker checker(useDC, caches);
    TR->walk(checker);
    return checker.Scope;
  }
};

class TypeAccessScopeChecker : private TypeWalker, AccessScopeChecker {
  bool CanonicalizeParentTypes;

  TypeAccessScopeChecker(const DeclContext *useDC,
                         decltype(TypeChecker::TypeAccessScopeCache) &caches,
                         bool canonicalizeParentTypes)
      : AccessScopeChecker(useDC, caches),
        CanonicalizeParentTypes(canonicalizeParentTypes) {}

  Action walkToTypePre(Type T) override {
    ValueDecl *VD;
    if (auto *BNAD = dyn_cast<NameAliasType>(T.getPointer())) {
      if (CanonicalizeParentTypes &&
          BNAD->getDecl()->getUnderlyingTypeLoc().getType()->hasTypeParameter())
        VD = nullptr;
      else
        VD = BNAD->getDecl();
    }
    else if (auto *NTD = T->getAnyNominal())
      VD = NTD;
    else
      VD = nullptr;

    if (!visitDecl(VD))
      return Action::Stop;

    if (!CanonicalizeParentTypes) {
      return isa<NameAliasType>(T.getPointer()) ? Action::SkipChildren
                                                     : Action::Continue;
    }
    
    Type nominalParentTy;
    if (auto nominalTy = dyn_cast<NominalType>(T.getPointer())) {
      nominalParentTy = nominalTy->getParent();
    } else if (auto genericTy = dyn_cast<BoundGenericType>(T.getPointer())) {
      nominalParentTy = genericTy->getParent();
      for (auto genericArg : genericTy->getGenericArgs())
        genericArg.walk(*this);
    } else if (auto NameAliasTy =
                 dyn_cast<NameAliasType>(T.getPointer())) {
      // The parent type would have been lost previously, so look right through
      // this type.
      if (NameAliasTy->getDecl()->getUnderlyingTypeLoc().getType()
            ->hasTypeParameter())
        Type(NameAliasTy->getSinglyDesugaredType()).walk(*this);
    } else {
      return Action::Continue;
    }

    if (nominalParentTy)
      nominalParentTy->getCanonicalType().walk(*this);
    return Action::SkipChildren;
  }

public:
  static Optional<AccessScope>
  getAccessScope(Type T, const DeclContext *useDC,
                 decltype(TypeChecker::TypeAccessScopeCache) &caches,
                 bool canonicalizeParentTypes = false) {
    TypeAccessScopeChecker checker(useDC, caches, canonicalizeParentTypes);
    T.walk(checker);
    return checker.Scope;
  }
};

} // end anonymous namespace


void TypeChecker::computeDefaultAccessLevel(ExtensionDecl *ED) {
  if (ED->hasDefaultAccessLevel())
    return;

  validateExtension(ED);

  if (ED->hasDefaultAccessLevel())
    return;

  AccessLevel maxAccess = AccessLevel::Public;

  if (!ED->getExtendedType().isNull() &&
      !ED->getExtendedType()->hasError()) {
    if (NominalTypeDecl *nominal = ED->getExtendedType()->getAnyNominal()) {
      validateDeclForNameLookup(nominal);
      if (ED->hasDefaultAccessLevel())
        return;
      maxAccess = std::max(nominal->getFormalAccess(),
                           AccessLevel::FilePrivate);
    }
  }

  if (const GenericParamList *genericParams = ED->getGenericParams()) {
    auto getTypeAccess = [this, ED](const TypeLoc &TL) -> AccessLevel {
      if (!TL.getType())
        return AccessLevel::Public;
      auto accessScope =
          TypeReprAccessScopeChecker::getAccessScope(TL.getTypeRepr(),
                                                     ED->getDeclContext(),
                                                     TypeAccessScopeCache);
      // This is an error case and will be diagnosed elsewhere.
      if (!accessScope.hasValue())
        return AccessLevel::Public;

      if (accessScope->isPublic())
        return AccessLevel::Public;
      if (isa<ModuleDecl>(accessScope->getDeclContext()))
        return AccessLevel::Internal;
      // Because extensions are always at top-level, they should never
      // reference declarations not at the top level. (And any such references
      // should be diagnosed elsewhere.) This code should not crash if that
      // occurs, though.
      return AccessLevel::FilePrivate;
    };

    // Only check the trailing 'where' requirements. Other requirements come
    // from the extended type and have already been checked.
    for (const RequirementRepr &req : genericParams->getTrailingRequirements()){
      switch (req.getKind()) {
      case RequirementReprKind::TypeConstraint:
        maxAccess = std::min(getTypeAccess(req.getSubjectLoc()), maxAccess);
        maxAccess = std::min(getTypeAccess(req.getConstraintLoc()), maxAccess);
        break;
      case RequirementReprKind::LayoutConstraint:
        maxAccess = std::min(getTypeAccess(req.getSubjectLoc()), maxAccess);
        break;
      case RequirementReprKind::SameType:
        maxAccess = std::min(getTypeAccess(req.getFirstTypeLoc()), maxAccess);
        maxAccess = std::min(getTypeAccess(req.getSecondTypeLoc()), maxAccess);
        break;
      }
    }
  }

  AccessLevel defaultAccess;
  if (auto *AA = ED->getAttrs().getAttribute<AccessControlAttr>())
    defaultAccess = std::max(AA->getAccess(), AccessLevel::FilePrivate);
  else
    defaultAccess = AccessLevel::Internal;

  // Don't set the max or default access level to 'open'.  This should
  // be diagnosed as invalid anyway.
  defaultAccess = std::min(defaultAccess, AccessLevel::Public);
  maxAccess = std::min(maxAccess, AccessLevel::Public);

  // Normally putting a public member in an internal extension is harmless,
  // because that member can never be used elsewhere. But if some of the types
  // in the signature are public, it could actually end up getting picked in
  // overload resolution. Therefore, we only enforce the maximum access if the
  // extension has a 'where' clause.
  if (ED->getTrailingWhereClause())
    defaultAccess = std::min(defaultAccess, maxAccess);
  else
    maxAccess = AccessLevel::Public;

  ED->setDefaultAndMaxAccess(defaultAccess, maxAccess);
}

void TypeChecker::computeAccessLevel(ValueDecl *D) {
  if (D->hasAccess())
    return;

  // Check if the decl has an explicit access control attribute.
  if (auto *AA = D->getAttrs().getAttribute<AccessControlAttr>()) {
    D->setAccess(AA->getAccess());

  } else if (auto accessor = dyn_cast<AccessorDecl>(D)) {
    // Special case for accessors, which inherit the access of their storage.
    // decl. A setter attribute can also override this.
    AbstractStorageDecl *storage = accessor->getStorage();
    if (storage->hasAccess()) {
      switch (accessor->getAccessorKind()) {
      case AccessorKind::IsGetter:
      case AccessorKind::IsAddressor:
        accessor->setAccess(storage->getFormalAccess());
        break;
      case AccessorKind::IsSetter:
      case AccessorKind::IsMutableAddressor:
      case AccessorKind::IsMaterializeForSet:
        accessor->setAccess(storage->getSetterFormalAccess());
        break;
      case AccessorKind::IsWillSet:
      case AccessorKind::IsDidSet:
        // These are only needed to synthesize the setter.
        accessor->setAccess(AccessLevel::Private);
        break;
      }
    } else {
      computeAccessLevel(storage);
      assert(accessor->hasAccess() &&
             "if the accessor isn't just the getter/setter this isn't enough");
    }
  }

  if (!D->hasAccess()) {
    DeclContext *DC = D->getDeclContext();
    switch (DC->getContextKind()) {
    case DeclContextKind::TopLevelCodeDecl:
      // Variables declared in a top-level 'guard' statement can be accessed in
      // later top-level code.
      D->setAccess(AccessLevel::FilePrivate);
      break;
    case DeclContextKind::AbstractClosureExpr:
      if (isa<ParamDecl>(D)) {
        // Closure parameters may need to be accessible to the enclosing
        // context, for single-expression closures.
        D->setAccess(AccessLevel::FilePrivate);
      } else {
        D->setAccess(AccessLevel::Private);
      }
      break;
    case DeclContextKind::SerializedLocal:
    case DeclContextKind::Initializer:
    case DeclContextKind::AbstractFunctionDecl:
    case DeclContextKind::SubscriptDecl:
      D->setAccess(AccessLevel::Private);
      break;
    case DeclContextKind::Module:
    case DeclContextKind::FileUnit:
      D->setAccess(AccessLevel::Internal);
      break;
    case DeclContextKind::GenericTypeDecl: {
      auto generic = cast<GenericTypeDecl>(DC);
      validateAccessControl(generic);
      AccessLevel access = AccessLevel::Internal;
      if (isa<ProtocolDecl>(generic))
        access = std::max(AccessLevel::FilePrivate,
                          generic->getFormalAccess());
      D->setAccess(access);
      break;
    }
    case DeclContextKind::ExtensionDecl: {
      auto extension = cast<ExtensionDecl>(DC);
      computeDefaultAccessLevel(extension);
      if (!D->hasAccess()) {
        auto access = extension->getDefaultAccessLevel();
        D->setAccess(access);
      }
    }
    }
  }

  if (auto ASD = dyn_cast<AbstractStorageDecl>(D)) {
    if (auto *AA = D->getAttrs().getAttribute<SetterAccessAttr>())
      ASD->setSetterAccess(AA->getAccess());
    else
      ASD->setSetterAccess(ASD->getFormalAccess());

    if (auto getter = ASD->getGetter())
      computeAccessLevel(getter);
    if (auto setter = ASD->getSetter())
      computeAccessLevel(setter);
  }
}

namespace {

class TypeAccessScopeDiagnoser : private ASTWalker {
  AccessScope accessScope;
  const DeclContext *useDC;
  const ComponentIdentTypeRepr *offendingType = nullptr;

  bool walkToTypeReprPre(TypeRepr *TR) override {
    // Exit early if we've already found a problem type.
    if (offendingType)
      return false;

    auto CITR = dyn_cast<ComponentIdentTypeRepr>(TR);
    if (!CITR)
      return true;

    const ValueDecl *VD = CITR->getBoundDecl();
    if (!VD)
      return true;

    if (VD->getFormalAccessScope(useDC) != accessScope)
      return true;

    offendingType = CITR;
    return false;
  }

  bool walkToTypeReprPost(TypeRepr *T) override {
    // Exit early if we've already found a problem type.
    return offendingType != nullptr;
  }

  explicit TypeAccessScopeDiagnoser(AccessScope accessScope,
                                    const DeclContext *useDC)
    : accessScope(accessScope), useDC(useDC) {}

public:
  static const TypeRepr *findTypeWithScope(TypeRepr *TR,
                                           AccessScope accessScope,
                                           const DeclContext *useDC) {
    assert(!accessScope.isPublic() &&
           "why would we need to find a public access scope?");
    if (TR == nullptr)
      return nullptr;
    TypeAccessScopeDiagnoser diagnoser(accessScope, useDC);
    TR->walk(diagnoser);
    return diagnoser.offendingType;
  }
};

/// A uniquely-typed boolean to reduce the chances of accidentally inverting
/// a check.
///
/// \see checkTypeAccess
enum class DowngradeToWarning: bool {
  No,
  Yes
};

/// \see checkTypeAccess
using CheckTypeAccessCallback =
    void(AccessScope, const TypeRepr *, DowngradeToWarning);

} // end anonymous namespace

/// Checks if the access scope of the type described by \p TL contains
/// \p contextAccessScope. If it isn't, calls \p diagnose with a TypeRepr
/// representing the offending part of \p TL.
///
/// If \p contextAccessScope is null, checks that \p TL is only made up of
/// public types.
///
/// The TypeRepr passed to \p diagnose may be null, in which case a particular
/// part of the type that caused the problem could not be found. The DeclContext
/// is never null.
static void checkTypeAccessImpl(
    TypeChecker &TC, TypeLoc TL, AccessScope contextAccessScope,
    const DeclContext *useDC,
    llvm::function_ref<CheckTypeAccessCallback> diagnose) {
  if (!TC.getLangOpts().EnableAccessControl)
    return;
  if (!TL.getType())
    return;
  // Don't spend time checking local declarations; this is always valid by the
  // time we get to this point.
  if (!contextAccessScope.isPublic() &&
      contextAccessScope.getDeclContext()->isLocalContext())
    return;

  // TypeRepr checking is more accurate, but we must also look at TypeLocs
  // without a TypeRepr, for example for 'var' declarations with an inferred
  // type.
  auto typeAccessScope =
    (TL.getTypeRepr()
     ? TypeReprAccessScopeChecker::getAccessScope(TL.getTypeRepr(), useDC,
                                                  TC.TypeAccessScopeCache)
     : TypeAccessScopeChecker::getAccessScope(TL.getType(), useDC,
                                              TC.TypeAccessScopeCache));

  // Note: This means that the type itself is invalid for this particular
  // context, because it references declarations from two incompatible scopes.
  // In this case we should have diagnosed the bad reference already.
  if (!typeAccessScope.hasValue())
    return;

  auto shouldComplainAboutAccessScope =
      [contextAccessScope](AccessScope scope) -> bool {
    if (scope.isPublic())
      return false;
    if (scope.hasEqualDeclContextWith(contextAccessScope))
      return false;
    if (contextAccessScope.isChildOf(scope))
      return false;
    return true;
  };

  if (!shouldComplainAboutAccessScope(typeAccessScope.getValue()))
    return;

  // Swift 3.0 wasn't nearly as strict as checking types because it didn't
  // look at the TypeRepr at all except to highlight a particular part of the
  // type in diagnostics, and looked through typealiases in other cases.
  // Approximate this behavior by running our non-TypeRepr-based check again
  // and downgrading to a warning when the checks disagree.
  auto downgradeToWarning = DowngradeToWarning::No;
  if (TC.getLangOpts().isSwiftVersion3()) {
    auto typeOnlyAccessScope =
       TypeAccessScopeChecker::getAccessScope(TL.getType(), useDC,
                                              TC.TypeAccessScopeCache,
                                              /*canonicalizeParents*/true);
    if (typeOnlyAccessScope.hasValue()) {
      // If Swift 4 would have complained about a private type, but Swift 4
      // would only diagnose an internal type, complain about the Swift 3
      // offense first to avoid confusing users.
      if (shouldComplainAboutAccessScope(typeOnlyAccessScope.getValue()))
        typeAccessScope = typeOnlyAccessScope;
      else
        downgradeToWarning = DowngradeToWarning::Yes;
    }
  }

  const TypeRepr *complainRepr =
        TypeAccessScopeDiagnoser::findTypeWithScope(
            TL.getTypeRepr(),
            *typeAccessScope,
            useDC);
  diagnose(*typeAccessScope, complainRepr, downgradeToWarning);
}

/// Checks if the access scope of the type described by \p TL is valid for the
/// type to be the type of \p context. If it isn't, calls \p diagnose with a
/// TypeRepr representing the offending part of \p TL.
///
/// The TypeRepr passed to \p diagnose may be null, in which case a particular
/// part of the type that caused the problem could not be found. The DeclContext
/// is never null. The DowngradeToWarning parameter is a hack to deal with
/// early versions of Swift 3 not diagnosing certain access violations.
static void checkTypeAccess(
    TypeChecker &TC, TypeLoc TL, const ValueDecl *context,
    llvm::function_ref<CheckTypeAccessCallback> diagnose) {
  const DeclContext *DC = context->getDeclContext();
  if (isa<ParamDecl>(context)) {
    context = dyn_cast<AbstractFunctionDecl>(DC);
    if (!context)
      context = dyn_cast<SubscriptDecl>(DC);
    if (!context)
      context = cast<EnumDecl>(DC);
    DC = context->getDeclContext();
  }

  AccessScope contextAccessScope = context->getFormalAccessScope();
  checkTypeAccessImpl(TC, TL, contextAccessScope, DC,
                      [=, &TC](AccessScope requiredAccessScope,
                               const TypeRepr *offendingTR,
                               DowngradeToWarning downgradeToWarning) {
    if (!contextAccessScope.isPublic() &&
        !isa<ModuleDecl>(contextAccessScope.getDeclContext()) &&
        TC.getLangOpts().isSwiftVersion3()) {
      // Swift 3.0.0 mistakenly didn't diagnose any issues when the context
      // access scope represented a private or fileprivate level.
      downgradeToWarning = DowngradeToWarning::Yes;
    }
    diagnose(requiredAccessScope, offendingTR, downgradeToWarning);
  });
}

/// Highlights the given TypeRepr, and adds a note pointing to the type's
/// declaration if possible.
///
/// Just flushes \p diag as is if \p complainRepr is null.
static void highlightOffendingType(TypeChecker &TC, InFlightDiagnostic &diag,
                                   const TypeRepr *complainRepr) {
  if (!complainRepr) {
    diag.flush();
    return;
  }

  diag.highlight(complainRepr->getSourceRange());
  diag.flush();

  if (auto CITR = dyn_cast<ComponentIdentTypeRepr>(complainRepr)) {
    const ValueDecl *VD = CITR->getBoundDecl();
    TC.diagnose(VD, diag::type_declared_here);
  }
}

static void checkGenericParamAccess(TypeChecker &TC,
                                    const GenericParamList *params,
                                    const Decl *owner,
                                    AccessScope accessScope,
                                    AccessLevel contextAccess) {
  if (!params)
    return;

  // This must stay in sync with diag::generic_param_access.
  enum {
    ACEK_Parameter = 0,
    ACEK_Requirement
  } accessControlErrorKind;
  auto minAccessScope = AccessScope::getPublic();
  const TypeRepr *complainRepr = nullptr;
  auto downgradeToWarning = DowngradeToWarning::Yes;

  for (auto param : *params) {
    if (param->getInherited().empty())
      continue;
    assert(param->getInherited().size() == 1);
    checkTypeAccessImpl(TC, param->getInherited().front(), accessScope,
                        owner->getDeclContext(),
                        [&](AccessScope typeAccessScope,
                            const TypeRepr *thisComplainRepr,
                            DowngradeToWarning thisDowngrade) {
      if (typeAccessScope.isChildOf(minAccessScope) ||
          (thisDowngrade == DowngradeToWarning::No &&
           downgradeToWarning == DowngradeToWarning::Yes) ||
          (!complainRepr &&
           typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
        minAccessScope = typeAccessScope;
        complainRepr = thisComplainRepr;
        accessControlErrorKind = ACEK_Parameter;
        downgradeToWarning = thisDowngrade;
      }
    });
  }

  for (auto &requirement : params->getRequirements()) {
    auto callback = [&](AccessScope typeAccessScope,
                        const TypeRepr *thisComplainRepr,
                        DowngradeToWarning thisDowngrade) {
      if (typeAccessScope.isChildOf(minAccessScope) ||
          (thisDowngrade == DowngradeToWarning::No &&
           downgradeToWarning == DowngradeToWarning::Yes) ||
          (!complainRepr &&
           typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
        minAccessScope = typeAccessScope;
        complainRepr = thisComplainRepr;
        accessControlErrorKind = ACEK_Requirement;
        downgradeToWarning = thisDowngrade;
      }
    };
    switch (requirement.getKind()) {
    case RequirementReprKind::TypeConstraint:
      checkTypeAccessImpl(TC, requirement.getSubjectLoc(),
                          accessScope, owner->getDeclContext(),
                          callback);
      checkTypeAccessImpl(TC, requirement.getConstraintLoc(),
                          accessScope, owner->getDeclContext(),
                          callback);
      break;
    case RequirementReprKind::LayoutConstraint:
      checkTypeAccessImpl(TC, requirement.getSubjectLoc(),
                          accessScope, owner->getDeclContext(),
                          callback);
      break;
    case RequirementReprKind::SameType:
      checkTypeAccessImpl(TC, requirement.getFirstTypeLoc(),
                          accessScope, owner->getDeclContext(),
                          callback);
      checkTypeAccessImpl(TC, requirement.getSecondTypeLoc(),
                          accessScope, owner->getDeclContext(),
                          callback);
      break;
    }
  }

  if (minAccessScope.isPublic())
    return;

  // Swift 3.0.0 mistakenly didn't diagnose any issues when the context access
  // scope represented a private or fileprivate level.
  if (downgradeToWarning == DowngradeToWarning::No) {
    if (!accessScope.isPublic() &&
        !isa<ModuleDecl>(accessScope.getDeclContext()) &&
        TC.getLangOpts().isSwiftVersion3()) {
      downgradeToWarning = DowngradeToWarning::Yes;
    }
  }

  auto minAccess = minAccessScope.accessLevelForDiagnostics();

  bool isExplicit =
    owner->getAttrs().hasAttribute<AccessControlAttr>() ||
    owner->getDeclContext()->getAsProtocolOrProtocolExtensionContext();
  auto diagID = diag::generic_param_access;
  if (downgradeToWarning == DowngradeToWarning::Yes)
    diagID = diag::generic_param_access_warn;
  auto diag = TC.diagnose(owner, diagID,
                          owner->getDescriptiveKind(), isExplicit,
                          contextAccess, minAccess,
                          isa<FileUnit>(owner->getDeclContext()),
                          accessControlErrorKind);
  highlightOffendingType(TC, diag, complainRepr);
}

static void checkGenericParamAccess(TypeChecker &TC,
                                    const GenericParamList *params,
                                    const ValueDecl *owner) {
  checkGenericParamAccess(TC, params, owner, owner->getFormalAccessScope(),
                          owner->getFormalAccess());
}

/// Checks the given declaration's access to make sure it is valid given the way
/// it is defined.
///
/// \p D must be a ValueDecl or a Decl that can appear in a type context.
static void checkAccessControl(TypeChecker &TC, const Decl *D) {
  if (D->isInvalid() || D->isImplicit())
    return;

  switch (D->getKind()) {
  case DeclKind::Import:
  case DeclKind::Extension:
  case DeclKind::TopLevelCode:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
  case DeclKind::PrecedenceGroup:
  case DeclKind::Module:
    llvm_unreachable("cannot appear in a type context");

  case DeclKind::Param:
  case DeclKind::GenericTypeParam:
  case DeclKind::MissingMember:
    llvm_unreachable("does not have access control");

  case DeclKind::IfConfig:
  case DeclKind::PoundDiagnostic:
    // Does not have access control.
  case DeclKind::EnumCase:
    // Handled at the EnumElement level.
  case DeclKind::Var:
    // Handled at the PatternBindingDecl level.
  case DeclKind::Destructor:
    // Always correct.
    return;

  case DeclKind::PatternBinding: {
    auto PBD = cast<PatternBindingDecl>(D);
    bool isTypeContext = PBD->getDeclContext()->isTypeContext();

    llvm::DenseSet<const VarDecl *> seenVars;
    for (auto entry : PBD->getPatternList())
    entry.getPattern()->forEachNode([&](const Pattern *P) {
      if (auto *NP = dyn_cast<NamedPattern>(P)) {
        // Only check individual variables if we didn't check an enclosing
        // TypedPattern.
        const VarDecl *theVar = NP->getDecl();
        if (seenVars.count(theVar) || theVar->isInvalid())
          return;

        checkTypeAccess(TC, TypeLoc::withoutLoc(theVar->getType()),
                        theVar,
                        [&](AccessScope typeAccessScope,
                            const TypeRepr *complainRepr,
                            DowngradeToWarning downgradeToWarning) {
          auto typeAccess = typeAccessScope.accessLevelForDiagnostics();
          bool isExplicit =
            theVar->getAttrs().hasAttribute<AccessControlAttr>();
          auto theVarAccess = isExplicit
            ? theVar->getFormalAccess()
            : typeAccessScope.requiredAccessForDiagnostics();
          auto diagID = diag::pattern_type_access_inferred;
          if (downgradeToWarning == DowngradeToWarning::Yes)
            diagID = diag::pattern_type_access_inferred_warn;
          auto diag = TC.diagnose(P->getLoc(), diagID,
                                  theVar->isLet(),
                                  isTypeContext,
                                  isExplicit,
                                  theVarAccess,
                                  isa<FileUnit>(theVar->getDeclContext()),
                                  typeAccess,
                                  theVar->getType());
        });
        return;
      }

      auto *TP = dyn_cast<TypedPattern>(P);
      if (!TP)
        return;

      // FIXME: We need an access level to check against, so we pull one out of
      // some random VarDecl in the pattern. They're all going to be the same,
      // but still, ick.
      const VarDecl *anyVar = nullptr;
      TP->forEachVariable([&](VarDecl *V) {
        seenVars.insert(V);
        anyVar = V;
      });
      if (!anyVar)
        return;

      checkTypeAccess(TC, TP->getTypeLoc(), anyVar,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *complainRepr,
                          DowngradeToWarning downgradeToWarning) {
        auto typeAccess = typeAccessScope.accessLevelForDiagnostics();
        bool isExplicit =
          anyVar->getAttrs().hasAttribute<AccessControlAttr>() ||
          anyVar->getDeclContext()->getAsProtocolOrProtocolExtensionContext();
        auto diagID = diag::pattern_type_access;
        if (downgradeToWarning == DowngradeToWarning::Yes)
          diagID = diag::pattern_type_access_warn;
        auto anyVarAccess = isExplicit
          ? anyVar->getFormalAccess()
          : typeAccessScope.requiredAccessForDiagnostics();
        auto diag = TC.diagnose(P->getLoc(), diagID,
                                anyVar->isLet(),
                                isTypeContext,
                                isExplicit,
                                anyVarAccess,
                                isa<FileUnit>(anyVar->getDeclContext()),
                                typeAccess);
        highlightOffendingType(TC, diag, complainRepr);
      });
    });
    return;
  }

  case DeclKind::TypeAlias: {
    auto TAD = cast<TypeAliasDecl>(D);

    checkTypeAccess(TC, TAD->getUnderlyingTypeLoc(), TAD,
                    [&](AccessScope typeAccessScope,
                        const TypeRepr *complainRepr,
                        DowngradeToWarning downgradeToWarning) {
      auto typeAccess = typeAccessScope.accessLevelForDiagnostics();
      bool isExplicit = TAD->getAttrs().hasAttribute<AccessControlAttr>();
      auto diagID = diag::type_alias_underlying_type_access;
      if (downgradeToWarning == DowngradeToWarning::Yes)
        diagID = diag::type_alias_underlying_type_access_warn;
      auto diag = TC.diagnose(TAD, diagID,
                              isExplicit, TAD->getFormalAccess(),
                              typeAccess, isa<FileUnit>(TAD->getDeclContext()));
      highlightOffendingType(TC, diag, complainRepr);
    });

    return;
  }

  case DeclKind::AssociatedType: {
    auto assocType = cast<AssociatedTypeDecl>(D);

    // This must stay in sync with diag::associated_type_access.
    enum {
      ACEK_DefaultDefinition = 0,
      ACEK_Requirement
    } accessControlErrorKind;
    auto minAccessScope = AccessScope::getPublic();
    const TypeRepr *complainRepr = nullptr;
    auto downgradeToWarning = DowngradeToWarning::No;

    std::for_each(assocType->getInherited().begin(),
                  assocType->getInherited().end(),
                  [&](TypeLoc requirement) {
      checkTypeAccess(TC, requirement, assocType,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *thisComplainRepr,
                          DowngradeToWarning downgradeDiag) {
        if (typeAccessScope.isChildOf(minAccessScope) ||
            (!complainRepr &&
             typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
          minAccessScope = typeAccessScope;
          complainRepr = thisComplainRepr;
          accessControlErrorKind = ACEK_Requirement;
          downgradeToWarning = downgradeDiag;
        }
      });
    });
    checkTypeAccess(TC, assocType->getDefaultDefinitionLoc(), assocType,
                    [&](AccessScope typeAccessScope,
                        const TypeRepr *thisComplainRepr,
                        DowngradeToWarning downgradeDiag) {
      if (typeAccessScope.isChildOf(minAccessScope) ||
          (!complainRepr &&
           typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
        minAccessScope = typeAccessScope;
        complainRepr = thisComplainRepr;
        accessControlErrorKind = ACEK_DefaultDefinition;
        downgradeToWarning = downgradeDiag;
      }
    });

    if (!minAccessScope.isPublic()) {
      auto minAccess = minAccessScope.accessLevelForDiagnostics();
      auto diagID = diag::associated_type_access;
      if (downgradeToWarning == DowngradeToWarning::Yes)
        diagID = diag::associated_type_access_warn;
      auto diag = TC.diagnose(assocType, diagID,
                              assocType->getFormalAccess(),
                              minAccess, accessControlErrorKind);
      highlightOffendingType(TC, diag, complainRepr);
    }
    return;
  }

  case DeclKind::Enum: {
    auto ED = cast<EnumDecl>(D);

    checkGenericParamAccess(TC, ED->getGenericParams(), ED);

    if (ED->hasRawType()) {
      Type rawType = ED->getRawType();
      auto rawTypeLocIter = std::find_if(ED->getInherited().begin(),
                                         ED->getInherited().end(),
                                         [&](TypeLoc inherited) {
        if (!inherited.wasValidated())
          return false;
        return inherited.getType().getPointer() == rawType.getPointer();
      });
      if (rawTypeLocIter == ED->getInherited().end())
        return;
      checkTypeAccess(TC, *rawTypeLocIter, ED,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *complainRepr,
                          DowngradeToWarning downgradeToWarning) {
        auto typeAccess = typeAccessScope.accessLevelForDiagnostics();
        bool isExplicit = ED->getAttrs().hasAttribute<AccessControlAttr>();
        auto diagID = diag::enum_raw_type_access;
        if (downgradeToWarning == DowngradeToWarning::Yes)
          diagID = diag::enum_raw_type_access_warn;
        auto diag = TC.diagnose(ED, diagID, isExplicit,
                                ED->getFormalAccess(), typeAccess,
                                isa<FileUnit>(ED->getDeclContext()));
        highlightOffendingType(TC, diag, complainRepr);
      });
    }

    return;
  }

  case DeclKind::Struct: {
    auto SD = cast<StructDecl>(D);
    checkGenericParamAccess(TC, SD->getGenericParams(), SD);
    return;
  }

  case DeclKind::Class: {
    auto CD = cast<ClassDecl>(D);

    checkGenericParamAccess(TC, CD->getGenericParams(), CD);

    if (CD->hasSuperclass()) {
      const NominalTypeDecl *superclassDecl =
          CD->getSuperclass()->getAnyNominal();
      // Be slightly defensive here in the presence of badly-ordered
      // inheritance clauses.
      auto superclassLocIter = std::find_if(CD->getInherited().begin(),
                                            CD->getInherited().end(),
                                            [&](TypeLoc inherited) {
        if (!inherited.wasValidated())
          return false;
        Type ty = inherited.getType();
        if (ty->is<ProtocolCompositionType>())
          ty = ty->getExistentialLayout().superclass;
        return ty->getAnyNominal() == superclassDecl;
      });
      // Sanity check: we couldn't find the superclass for whatever reason
      // (possibly because it's synthetic or something), so don't bother
      // checking it.
      if (superclassLocIter == CD->getInherited().end())
        return;

      auto outerDowngradeToWarning = DowngradeToWarning::No;
      if (superclassDecl->isGenericContext() &&
          !TC.getLangOpts().isSwiftVersionAtLeast(5)) {
        // Swift 4 failed to properly check this if the superclass was generic,
        // because the above loop was too strict.
        outerDowngradeToWarning = DowngradeToWarning::Yes;
      }

      checkTypeAccess(TC, *superclassLocIter, CD,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *complainRepr,
                          DowngradeToWarning downgradeToWarning) {
        auto typeAccess = typeAccessScope.accessLevelForDiagnostics();
        bool isExplicit = CD->getAttrs().hasAttribute<AccessControlAttr>();
        auto diagID = diag::class_super_access;
        if (downgradeToWarning == DowngradeToWarning::Yes ||
            outerDowngradeToWarning == DowngradeToWarning::Yes) {
          diagID = diag::class_super_access_warn;
        }
        auto diag = TC.diagnose(CD, diagID, isExplicit, CD->getFormalAccess(),
                                typeAccess,
                                isa<FileUnit>(CD->getDeclContext()));
        highlightOffendingType(TC, diag, complainRepr);
      });
    }

    return;
  }

  case DeclKind::Protocol: {
    auto proto = cast<ProtocolDecl>(D);

    auto minAccessScope = AccessScope::getPublic();
    const TypeRepr *complainRepr = nullptr;
    auto downgradeToWarning = DowngradeToWarning::No;

    std::for_each(proto->getInherited().begin(),
                  proto->getInherited().end(),
                  [&](TypeLoc requirement) {
      checkTypeAccess(TC, requirement, proto,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *thisComplainRepr,
                          DowngradeToWarning downgradeDiag) {
        if (typeAccessScope.isChildOf(minAccessScope) ||
            (!complainRepr &&
             typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
          minAccessScope = typeAccessScope;
          complainRepr = thisComplainRepr;
          downgradeToWarning = downgradeDiag;
        }
      });
    });

    if (!minAccessScope.isPublic()) {
      auto minAccess = minAccessScope.accessLevelForDiagnostics();
      bool isExplicit = proto->getAttrs().hasAttribute<AccessControlAttr>();
      auto diagID = diag::protocol_refine_access;
      if (downgradeToWarning == DowngradeToWarning::Yes)
        diagID = diag::protocol_refine_access_warn;
      auto diag = TC.diagnose(proto, diagID,
                              isExplicit, proto->getFormalAccess(), minAccess,
                              isa<FileUnit>(proto->getDeclContext()));
      highlightOffendingType(TC, diag, complainRepr);
    }
    return;
  }

  case DeclKind::Subscript: {
    auto SD = cast<SubscriptDecl>(D);

    auto minAccessScope = AccessScope::getPublic();
    const TypeRepr *complainRepr = nullptr;
    auto downgradeToWarning = DowngradeToWarning::No;
    bool problemIsElement = false;

    for (auto &P : *SD->getIndices()) {
      checkTypeAccess(TC, P->getTypeLoc(), P,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *thisComplainRepr,
                          DowngradeToWarning downgradeDiag) {
        if (typeAccessScope.isChildOf(minAccessScope) ||
            (!complainRepr &&
             typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
          minAccessScope = typeAccessScope;
          complainRepr = thisComplainRepr;
          downgradeToWarning = downgradeDiag;
        }
      });
    }

    checkTypeAccess(TC, SD->getElementTypeLoc(), SD,
                    [&](AccessScope typeAccessScope,
                        const TypeRepr *thisComplainRepr,
                        DowngradeToWarning downgradeDiag) {
      if (typeAccessScope.isChildOf(minAccessScope) ||
          (!complainRepr &&
           typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
        minAccessScope = typeAccessScope;
        complainRepr = thisComplainRepr;
        downgradeToWarning = downgradeDiag;
        problemIsElement = true;
      }
    });

    if (!minAccessScope.isPublic()) {
      auto minAccess = minAccessScope.accessLevelForDiagnostics();
      bool isExplicit =
        SD->getAttrs().hasAttribute<AccessControlAttr>() ||
        SD->getDeclContext()->getAsProtocolOrProtocolExtensionContext();
      auto diagID = diag::subscript_type_access;
      if (downgradeToWarning == DowngradeToWarning::Yes)
        diagID = diag::subscript_type_access_warn;
      auto subscriptDeclAccess = isExplicit
        ? SD->getFormalAccess()
        : minAccessScope.requiredAccessForDiagnostics();
      auto diag = TC.diagnose(SD, diagID,
                              isExplicit,
                              subscriptDeclAccess,
                              minAccess,
                              problemIsElement);
      highlightOffendingType(TC, diag, complainRepr);
    }
    return;
  }

  case DeclKind::Accessor:
    return;

  case DeclKind::Func:
  case DeclKind::Constructor: {
    auto fn = cast<AbstractFunctionDecl>(D);
    bool isTypeContext = fn->getDeclContext()->isTypeContext();

    checkGenericParamAccess(TC, fn->getGenericParams(), fn);

    // This must stay in sync with diag::function_type_access.
    enum {
      FK_Function = 0,
      FK_Method,
      FK_Initializer
    };

    auto minAccessScope = AccessScope::getPublic();
    const TypeRepr *complainRepr = nullptr;
    auto downgradeToWarning = DowngradeToWarning::No;

    for (auto *PL : fn->getParameterLists().slice(isTypeContext)) {
      for (auto &P : *PL) {
        checkTypeAccess(TC, P->getTypeLoc(), P,
                        [&](AccessScope typeAccessScope,
                            const TypeRepr *thisComplainRepr,
                            DowngradeToWarning downgradeDiag) {
          if (typeAccessScope.isChildOf(minAccessScope) ||
              (!complainRepr &&
               typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
            minAccessScope = typeAccessScope;
            complainRepr = thisComplainRepr;
            downgradeToWarning = downgradeDiag;
          }
        });
      }
    }

    bool problemIsResult = false;
    if (auto FD = dyn_cast<FuncDecl>(fn)) {
      checkTypeAccess(TC, FD->getBodyResultTypeLoc(), FD,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *thisComplainRepr,
                          DowngradeToWarning downgradeDiag) {
        if (typeAccessScope.isChildOf(minAccessScope) ||
            (!complainRepr &&
             typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
          minAccessScope = typeAccessScope;
          complainRepr = thisComplainRepr;
          downgradeToWarning = downgradeDiag;
          problemIsResult = true;
        }
      });
    }

    if (!minAccessScope.isPublic()) {
      auto minAccess = minAccessScope.accessLevelForDiagnostics();
      auto functionKind = isa<ConstructorDecl>(fn)
        ? FK_Initializer
        : isTypeContext ? FK_Method : FK_Function;
      bool isExplicit =
        fn->getAttrs().hasAttribute<AccessControlAttr>() ||
        fn->getDeclContext()->getAsProtocolOrProtocolExtensionContext();
      auto diagID = diag::function_type_access;
      if (downgradeToWarning == DowngradeToWarning::Yes)
        diagID = diag::function_type_access_warn;
      auto fnAccess = isExplicit
        ? fn->getFormalAccess()
        : minAccessScope.requiredAccessForDiagnostics();
      auto diag = TC.diagnose(fn, diagID,
                              isExplicit,
                              fnAccess,
                              isa<FileUnit>(fn->getDeclContext()),
                              minAccess,
                              functionKind,
                              problemIsResult);
      highlightOffendingType(TC, diag, complainRepr);
    }
    return;
  }

  case DeclKind::EnumElement: {
    auto EED = cast<EnumElementDecl>(D);

    if (!EED->hasAssociatedValues())
      return;
    for (auto &P : *EED->getParameterList()) {
      checkTypeAccess(TC, P->getTypeLoc(), P,
                             [&](AccessScope typeAccessScope,
                                 const TypeRepr *complainRepr,
                                 DowngradeToWarning downgradeToWarning) {
        auto typeAccess = typeAccessScope.accessLevelForDiagnostics();
        auto diagID = diag::enum_case_access;
        if (downgradeToWarning == DowngradeToWarning::Yes)
          diagID = diag::enum_case_access_warn;
        auto diag = TC.diagnose(EED, diagID,
                                EED->getFormalAccess(), typeAccess);
        highlightOffendingType(TC, diag, complainRepr);
      });
    }

    return;
  }
  }
}

/// Whether this declaration is a member of a class extension marked @objc.
static bool isMemberOfObjCClassExtension(const ValueDecl *VD) {
  auto ext = dyn_cast<ExtensionDecl>(VD->getDeclContext());
  if (!ext) return false;

  return ext->getAsClassOrClassExtensionContext() &&
    ext->getAttrs().hasAttribute<ObjCAttr>();
}

/// Whether this declaration is a member of a class with the `@objcMembers`
/// attribute.
static bool isMemberOfObjCMembersClass(const ValueDecl *VD) {
  auto classDecl = VD->getDeclContext()->getAsClassOrClassExtensionContext();
  if (!classDecl) return false;

  return classDecl->getAttrs().hasAttribute<ObjCMembersAttr>();
}
/// Figure out if a declaration should be exported to Objective-C.
static Optional<ObjCReason> shouldMarkAsObjC(TypeChecker &TC,
                                             const ValueDecl *VD,
                                             bool allowImplicit = false){
  assert(!isa<ClassDecl>(VD));

  ProtocolDecl *protocolContext =
      dyn_cast<ProtocolDecl>(VD->getDeclContext());
  bool isMemberOfObjCProtocol =
      protocolContext && protocolContext->isObjC();

  // Local function to determine whether we can implicitly infer @objc.
  auto canInferImplicitObjC = [&] {
    if (VD->isInvalid())
      return false;
    if (VD->isOperator())
      return false;

    // Implicitly generated declarations are not @objc, except for constructors.
    if (!allowImplicit && VD->isImplicit())
      return false;

    if (VD->getFormalAccess() <= AccessLevel::FilePrivate)
      return false;

    return true;
  };

  // explicitly declared @objc.
  if (VD->getAttrs().hasAttribute<ObjCAttr>())
    return ObjCReason::ExplicitlyObjC;
  // @IBOutlet, @IBAction, @NSManaged, and @GKInspectable imply @objc.
  //
  // @IBInspectable and @GKInspectable imply @objc quietly in Swift 3
  // (where they warn on failure) and loudly in Swift 4 (error on failure).
  if (VD->getAttrs().hasAttribute<IBOutletAttr>())
    return ObjCReason::ExplicitlyIBOutlet;
  if (VD->getAttrs().hasAttribute<IBActionAttr>())
    return ObjCReason::ExplicitlyIBAction;
  if (VD->getAttrs().hasAttribute<IBInspectableAttr>())
    return ObjCReason::ExplicitlyIBInspectable;
  if (VD->getAttrs().hasAttribute<GKInspectableAttr>())
    return ObjCReason::ExplicitlyGKInspectable;
  if (VD->getAttrs().hasAttribute<NSManagedAttr>())
    return ObjCReason::ExplicitlyNSManaged;
  // A member of an @objc protocol is implicitly @objc.
  if (isMemberOfObjCProtocol)
    return ObjCReason::MemberOfObjCProtocol;
  // A @nonobjc is not @objc, even if it is an override of an @objc, so check
  // for @nonobjc first.
  if (VD->getAttrs().hasAttribute<NonObjCAttr>() ||
      (isa<ExtensionDecl>(VD->getDeclContext()) &&
       cast<ExtensionDecl>(VD->getDeclContext())->getAttrs()
        .hasAttribute<NonObjCAttr>()))
    return None;
  if (isMemberOfObjCClassExtension(VD))
    return ObjCReason::MemberOfObjCExtension;
  if (isMemberOfObjCMembersClass(VD) && canInferImplicitObjC())
    return ObjCReason::MemberOfObjCMembersClass;
  // An override of an @objc declaration is implicitly @objc.
  if (VD->getOverriddenDecl() && VD->getOverriddenDecl()->isObjC())
    return ObjCReason::OverridesObjC;
  // A witness to an @objc protocol requirement is implicitly @objc.
  if (VD->getDeclContext()->getAsClassOrClassExtensionContext() &&
      !TC.findWitnessedObjCRequirements(VD,
                                        /*anySingleRequirement=*/true).empty())
    return ObjCReason::WitnessToObjC;

  // Infer '@objc' for 'dynamic' members.
  if (auto attr = VD->getAttrs().getAttribute<DynamicAttr>()) {
    // For implicit 'dynamic', just infer '@objc' implicitly.
    if (attr->isImplicit())
      return ObjCReason::ImplicitlyObjC;

    bool isGetterOrSetter =
      isa<AccessorDecl>(VD) && cast<AccessorDecl>(VD)->isGetterOrSetter();

    // Under Swift 3's @objc inference rules, 'dynamic' infers '@objc'.
    if (TC.Context.LangOpts.EnableSwift3ObjCInference) {
      // If we've been asked to warn about deprecated @objc inference, do so
      // now.
      if (TC.Context.LangOpts.WarnSwift3ObjCInference !=
            Swift3ObjCInferenceWarnings::None &&
          !isGetterOrSetter) {
        TC.diagnose(VD, diag::objc_inference_swift3_dynamic)
          .highlight(attr->getLocation())
          .fixItInsert(VD->getAttributeInsertionLoc(/*forModifier=*/false),
                      "@objc ");
      }

      return ObjCReason::ExplicitlyDynamic;
    }

    // Complain that 'dynamic' requires '@objc', but (quietly) infer @objc
    // anyway for better recovery.
    TC.diagnose(VD, diag::dynamic_requires_objc,
                VD->getDescriptiveKind(), VD->getFullName())
      .highlight(attr->getRange())
      .fixItInsert(VD->getAttributeInsertionLoc(/*forModifier=*/false),
                   "@objc ");

    return ObjCReason::ImplicitlyObjC;
  }

  // If we aren't provided Swift 3's @objc inference rules, we're done.
  if (!TC.Context.LangOpts.EnableSwift3ObjCInference)
    return None;

  // Infer '@objc' for valid, non-implicit, non-operator, members of classes
  // (and extensions thereof) whose class hierarchies originate in Objective-C,
  // e.g., which derive from NSObject, so long as the members have internal
  // access or greater.
  if (!canInferImplicitObjC())
    return None;

  // If this declaration is part of a class with implicitly @objc members,
  // make it implicitly @objc. However, if the declaration cannot be represented
  // as @objc, don't diagnose.
  if (auto classDecl = VD->getDeclContext()
          ->getAsClassOrClassExtensionContext()) {
    // One cannot define @objc members of any foreign classes.
    if (classDecl->isForeign())
      return None;

    if (classDecl->checkObjCAncestry() != ObjCClassKind::NonObjC) {
      return VD->isImplicit() ? ObjCReason::ImplicitlyObjC
                              : ObjCReason::MemberOfObjCSubclass;
    }
  }

  return None;
}

/// If we need to infer 'dynamic', do so now.
///
/// This occurs when
/// - it is implied by an attribute like @NSManaged
/// - when we have an override of an imported method
/// - we need to dynamically dispatch to a method in an extension
///
/// FIXME: The latter reason is a hack. We should figure out how to safely
/// put extension methods into the class vtable.
static void inferDynamic(ASTContext &ctx, ValueDecl *D) {
  // If we can't infer dynamic here, don't.
  if (!DeclAttribute::canAttributeAppearOnDecl(DAK_Dynamic, D))
    return;

  // The presence of 'dynamic' blocks the inference of 'dynamic'.
  if (D->isDynamic())
    return;

  // Only 'objc' declarations use 'dynamic'.
  if (!D->isObjC() || D->hasClangNode())
    return;

  bool overridesImportedMethod =
    (D->getOverriddenDecl() &&
     D->getOverriddenDecl()->hasClangNode());

  bool isNSManaged = D->getAttrs().hasAttribute<NSManagedAttr>();

  bool isExtension = isa<ExtensionDecl>(D->getDeclContext());

  // We only infer 'dynamic' in these three cases.
  if (!isExtension && !isNSManaged && !overridesImportedMethod)
    return;

  // The presence of 'final' blocks the inference of 'dynamic'.
  if (D->isFinal() && !isNSManaged)
    return;

  // Accessors should not infer 'dynamic' on their own; they can get it from
  // their storage decls.
  if (isa<AccessorDecl>(D))
    return;

  // Only classes can use 'dynamic'.
  auto classDecl = D->getDeclContext()->getAsClassOrClassExtensionContext();
  if (!classDecl)
    return;

  // Add the 'dynamic' attribute.
  D->getAttrs().add(new (ctx) DynamicAttr(/*IsImplicit=*/true));
}

/// Check runtime functions responsible for implicit bridging of Objective-C
/// types.
static void checkObjCBridgingFunctions(TypeChecker &TC,
                                       ModuleDecl *mod,
                                       StringRef bridgedTypeName,
                                       StringRef forwardConversion,
                                       StringRef reverseConversion) {
  assert(mod);
  ModuleDecl::AccessPathTy unscopedAccess = {};
  SmallVector<ValueDecl *, 4> results;
  
  auto &Ctx = TC.Context;
  mod->lookupValue(unscopedAccess, Ctx.getIdentifier(bridgedTypeName),
                   NLKind::QualifiedLookup, results);
  mod->lookupValue(unscopedAccess, Ctx.getIdentifier(forwardConversion),
                   NLKind::QualifiedLookup, results);
  mod->lookupValue(unscopedAccess, Ctx.getIdentifier(reverseConversion),
                   NLKind::QualifiedLookup, results);
  
  for (auto D : results)
    TC.validateDecl(D);
}

static void checkBridgedFunctions(TypeChecker &TC) {
  if (TC.HasCheckedBridgeFunctions)
    return;
  
  TC.HasCheckedBridgeFunctions = true;
  
  #define BRIDGE_TYPE(BRIDGED_MOD, BRIDGED_TYPE, _, NATIVE_TYPE, OPT) \
  Identifier ID_##BRIDGED_MOD = TC.Context.getIdentifier(#BRIDGED_MOD);\
  if (ModuleDecl *module = TC.Context.getLoadedModule(ID_##BRIDGED_MOD)) {\
    checkObjCBridgingFunctions(TC, module, #BRIDGED_TYPE, \
    "_convert" #BRIDGED_TYPE "To" #NATIVE_TYPE, \
    "_convert" #NATIVE_TYPE "To" #BRIDGED_TYPE); \
  }
  #include "swift/SIL/BridgedTypes.def"
  
  if (ModuleDecl *module = TC.Context.getLoadedModule(TC.Context.Id_Foundation)) {
    checkObjCBridgingFunctions(TC, module,
                               TC.Context.getSwiftName(
                                 KnownFoundationEntity::NSError),
                               "_convertNSErrorToError",
                               "_convertErrorToNSError");
  }
}

/// Infer the Objective-C name for a given declaration.
static void inferObjCName(TypeChecker &tc, ValueDecl *decl) {
  if (isa<DestructorDecl>(decl))
    return;

  auto attr = decl->getAttrs().getAttribute<ObjCAttr>();
  assert(attr && "should only be called on decls already marked @objc");

  // If this declaration overrides an @objc declaration, use its name.
  if (auto overridden = decl->getOverriddenDecl()) {
    if (overridden->isObjC()) {
      // Handle methods first.
      if (auto overriddenFunc = dyn_cast<AbstractFunctionDecl>(overridden)) {
        // Determine the selector of the overridden method.
        ObjCSelector overriddenSelector = overriddenFunc->getObjCSelector();

        // Determine whether there is a name conflict.
        bool shouldFixName = !attr->hasName();
        if (attr->hasName() && *attr->getName() != overriddenSelector) {
          // If the user explicitly wrote the incorrect name, complain.
          if (!attr->isNameImplicit()) {
            {
              auto diag = tc.diagnose(
                            attr->AtLoc,
                            diag::objc_override_method_selector_mismatch,
                            *attr->getName(), overriddenSelector);
              fixDeclarationObjCName(diag, decl, overriddenSelector);
            }

            tc.diagnose(overriddenFunc, diag::overridden_here);
          }

          shouldFixName = true;
        }

        // If we have to set the name, do so.
        if (shouldFixName) {
          // Override the name on the attribute.
          const_cast<ObjCAttr *>(attr)->setName(overriddenSelector,
                                                /*implicit=*/true);
        }
        return;
      }

      // Handle properties.
      if (auto overriddenProp = dyn_cast<VarDecl>(overridden)) {
        Identifier overriddenName = overriddenProp->getObjCPropertyName();
        ObjCSelector overriddenNameAsSel(tc.Context, 0, overriddenName);

        // Determine whether there is a name conflict.
        bool shouldFixName = !attr->hasName();
        if (attr->hasName() && *attr->getName() != overriddenNameAsSel) {
          // If the user explicitly wrote the wrong name, complain.
          if (!attr->isNameImplicit()) {
            tc.diagnose(attr->AtLoc,
                        diag::objc_override_property_name_mismatch,
                        attr->getName()->getSelectorPieces()[0],
                        overriddenName)
              .fixItReplaceChars(attr->getNameLocs().front(),
                                 attr->getRParenLoc(),
                                 overriddenName.str());
            tc.diagnose(overridden, diag::overridden_here);
          }

          shouldFixName = true;
        }

        // Fix the name, if needed.
        if (shouldFixName) {
          const_cast<ObjCAttr *>(attr)->setName(overriddenNameAsSel,
                                                /*implicit=*/true);
        }
        return;
      }
    }
  }

  // If the decl already has a name, do nothing; the protocol conformance
  // checker will handle any mismatches.
  if (attr->hasName()) return;

  // When no override determined the Objective-C name, look for
  // requirements for which this declaration is a witness.
  Optional<ObjCSelector> requirementObjCName;
  ValueDecl *firstReq = nullptr;
  for (auto req : tc.findWitnessedObjCRequirements(decl)) {
    // If this is the first requirement, take its name.
    if (!requirementObjCName) {
      requirementObjCName = req->getObjCRuntimeName();
      firstReq = req;
      continue;
    }

    // If this requirement has a different name from one we've seen,
    // note the ambiguity.
    if (*requirementObjCName != *req->getObjCRuntimeName()) {
      tc.diagnose(decl, diag::objc_ambiguous_inference,
                  decl->getDescriptiveKind(), decl->getFullName(),
                  *requirementObjCName, *req->getObjCRuntimeName());
      
      // Note the candidates and what Objective-C names they provide.
      auto diagnoseCandidate = [&](ValueDecl *req) {
        auto proto = cast<ProtocolDecl>(req->getDeclContext());
        auto diag = tc.diagnose(decl,
                                diag::objc_ambiguous_inference_candidate,
                                req->getFullName(),
                                proto->getFullName(),
                                *req->getObjCRuntimeName());
        fixDeclarationObjCName(diag, decl, req->getObjCRuntimeName());
      };
      diagnoseCandidate(firstReq);
      diagnoseCandidate(req);

      // Suggest '@nonobjc' to suppress this error, and not try to
      // infer @objc for anything.
      tc.diagnose(decl, diag::req_near_match_nonobjc, true)
        .fixItInsert(decl->getAttributeInsertionLoc(false), "@nonobjc ");
      break;
    }
  }

  // If we have a name, install it via an @objc attribute.
  if (requirementObjCName) {
    const_cast<ObjCAttr *>(attr)->setName(*requirementObjCName,
                                          /*implicit=*/true);
  }
}

/// Mark the given declaration as being Objective-C compatible (or
/// not) as appropriate.
///
/// If the declaration has a @nonobjc attribute, diagnose an error
/// using the given Reason, if present.
void swift::markAsObjC(TypeChecker &TC, ValueDecl *D,
                       Optional<ObjCReason> isObjC,
                       Optional<ForeignErrorConvention> errorConvention) {
  D->setIsObjC(isObjC.hasValue());

  if (!isObjC) {
    // FIXME: For now, only @objc declarations can be dynamic.
    if (auto attr = D->getAttrs().getAttribute<DynamicAttr>())
      attr->setInvalid();
    return;
  }

  // By now, the caller will have handled the case where an implicit @objc
  // could be overridden by @nonobjc. If we see a @nonobjc and we are trying
  // to add an @objc for whatever reason, diagnose an error.
  if (auto *attr = D->getAttrs().getAttribute<NonObjCAttr>()) {
    if (!shouldDiagnoseObjCReason(*isObjC, TC.Context))
      isObjC = ObjCReason::ImplicitlyObjC;

    TC.diagnose(D->getStartLoc(), diag::nonobjc_not_allowed,
                getObjCDiagnosticAttrKind(*isObjC));

    attr->setInvalid();
  }

  // Make sure we have the appropriate bridging operations.
  if (!isa<DestructorDecl>(D))
    checkBridgedFunctions(TC);
  TC.useObjectiveCBridgeableConformances(D->getInnermostDeclContext(),
                                         D->getInterfaceType());

  // Record the name of this Objective-C method in its class.
  if (auto classDecl
        = D->getDeclContext()->getAsClassOrClassExtensionContext()) {
    if (auto method = dyn_cast<AbstractFunctionDecl>(D)) {
      // Determine the foreign error convention.
      if (auto baseMethod = method->getOverriddenDecl()) {
        // If the overridden method has a foreign error convention,
        // adopt it.  Set the foreign error convention for a throwing
        // method.  Note that the foreign error convention affects the
        // selector, so we perform this before inferring a selector.
        if (method->hasThrows()) {
          if (auto baseErrorConvention
                = baseMethod->getForeignErrorConvention()) {
            errorConvention = baseErrorConvention;
          }

          assert(errorConvention && "Missing error convention");
          method->setForeignErrorConvention(*errorConvention);
        }
      } else if (method->hasThrows()) {
        // Attach the foreign error convention.
        assert(errorConvention && "Missing error convention");
        method->setForeignErrorConvention(*errorConvention);
      }

      // Infer the Objective-C name for this method.
      inferObjCName(TC, method);

      // ... then record it.
      classDecl->recordObjCMethod(method);

      // Swift does not permit class methods with Objective-C selectors 'load',
      // 'alloc', or 'allocWithZone:'.
      if (!method->isInstanceMember()) {
        auto isForbiddenSelector = [&TC](ObjCSelector sel)
        -> Optional<Diag<unsigned, DeclName, ObjCSelector>> {
          switch (sel.getNumArgs()) {
          case 0:
            if (sel.getSelectorPieces().front() == TC.Context.Id_load ||
                sel.getSelectorPieces().front() == TC.Context.Id_alloc)
              return diag::objc_class_method_not_permitted;
            // Swift 3 and earlier allowed you to override `initialize`, but
            // Swift's semantics do not guarantee that it will be called at
            // the point you expect. It is disallowed in Swift 4 and later.
            if (sel.getSelectorPieces().front() == TC.Context.Id_initialize) {
              if (TC.getLangOpts().isSwiftVersion3())
                return
                  diag::objc_class_method_not_permitted_swift3_compat_warning;
              else
                return diag::objc_class_method_not_permitted;
            }
            return None;
          case 1:
            if (sel.getSelectorPieces().front() == TC.Context.Id_allocWithZone)
              return diag::objc_class_method_not_permitted;
            return None;
          default:
            return None;
          }
        };
        auto sel = method->getObjCSelector();
        if (auto diagID = isForbiddenSelector(sel)) {
          auto diagInfo = getObjCMethodDiagInfo(method);
          TC.diagnose(method, *diagID,
                      diagInfo.first, diagInfo.second, sel);
        }
      }
    } else if (isa<VarDecl>(D)) {
      // Infer the Objective-C name for this property.
      inferObjCName(TC, D);
    }
  } else if (auto method = dyn_cast<AbstractFunctionDecl>(D)) {
    if (method->hasThrows()) {
      // Attach the foreign error convention.
      assert(errorConvention && "Missing error convention");
      method->setForeignErrorConvention(*errorConvention);
    }
  }

  // Record this method in the source-file-specific Objective-C method
  // table.
  if (auto method = dyn_cast<AbstractFunctionDecl>(D)) {
    if (auto sourceFile = method->getParentSourceFile()) {
      sourceFile->ObjCMethods[method->getObjCSelector()].push_back(method);
    }
  }

  // Special handling for Swift 3 @objc inference rules that are no longer
  // present in later versions of Swift.
  if (*isObjC == ObjCReason::MemberOfObjCSubclass) {
    // If we've been asked to unconditionally warn about these deprecated
    // @objc inference rules, do so now. However, we don't warn about
    // accessors---just the main storage declarations.
    if (TC.Context.LangOpts.WarnSwift3ObjCInference ==
          Swift3ObjCInferenceWarnings::Complete &&
        !(isa<AccessorDecl>(D) && cast<AccessorDecl>(D)->isGetterOrSetter())) {
      TC.diagnose(D, diag::objc_inference_swift3_objc_derived);
      TC.diagnose(D, diag::objc_inference_swift3_addobjc)
        .fixItInsert(D->getAttributeInsertionLoc(/*forModifier=*/false),
                     "@objc ");
      TC.diagnose(D, diag::objc_inference_swift3_addnonobjc)
        .fixItInsert(D->getAttributeInsertionLoc(/*forModifier=*/false),
                     "@nonobjc ");
    }

    // Mark the attribute as having used Swift 3 inference, or create an
    // implicit @objc for that purpose.
    auto attr = D->getAttrs().getAttribute<ObjCAttr>();
    attr->setSwift3Inferred();
  }
}

namespace {
  /// How to generate the raw value for each element of an enum that doesn't
  /// have one explicitly specified.
  enum class AutomaticEnumValueKind {
    /// Raw values cannot be automatically generated.
    None,
    /// The raw value is the enum element's name.
    String,
    /// The raw value is the previous element's raw value, incremented.
    ///
    /// For the first element in the enum, the raw value is 0.
    Integer,
  };
} // end anonymous namespace

/// Given the raw value literal expression for an enum case, produces the
/// auto-incremented raw value for the subsequent case, or returns null if
/// the value is not auto-incrementable.
static LiteralExpr *getAutomaticRawValueExpr(TypeChecker &TC,
                                             AutomaticEnumValueKind valueKind,
                                             EnumElementDecl *forElt,
                                             LiteralExpr *prevValue) {
  switch (valueKind) {
  case AutomaticEnumValueKind::None:
    TC.diagnose(forElt->getLoc(),
                diag::enum_non_integer_convertible_raw_type_no_value);
    return nullptr;

  case AutomaticEnumValueKind::String:
    return new (TC.Context) StringLiteralExpr(forElt->getNameStr(), SourceLoc(),
                                              /*Implicit=*/true);

  case AutomaticEnumValueKind::Integer:
    // If there was no previous value, start from zero.
    if (!prevValue) {
      return new (TC.Context) IntegerLiteralExpr("0", SourceLoc(),
                                                 /*Implicit=*/true);
    }
    
    if (auto intLit = dyn_cast<IntegerLiteralExpr>(prevValue)) {
      APInt nextVal = intLit->getValue() + 1;
      bool negative = nextVal.slt(0);
      if (negative)
        nextVal = -nextVal;

      llvm::SmallString<10> nextValStr;
      nextVal.toStringSigned(nextValStr);
      auto expr = new (TC.Context)
        IntegerLiteralExpr(TC.Context.AllocateCopy(StringRef(nextValStr)),
                           forElt->getLoc(), /*Implicit=*/true);
      if (negative)
        expr->setNegative(forElt->getLoc());

      return expr;
    }

    TC.diagnose(forElt->getLoc(),
                diag::enum_non_integer_raw_value_auto_increment);
    return nullptr;
  }

  llvm_unreachable("Unhandled AutomaticEnumValueKind in switch.");
}

static void checkEnumRawValues(TypeChecker &TC, EnumDecl *ED) {
  Type rawTy = ED->getRawType();

  if (!rawTy) {
    // @objc enums must have a raw type.
    if (ED->isObjC())
      TC.diagnose(ED->getNameLoc(), diag::objc_enum_no_raw_type);
    return;
  }

  if (ED->getGenericEnvironmentOfContext() != nullptr)
    rawTy = ED->mapTypeIntoContext(rawTy);
  if (rawTy->hasError())
    return;

  AutomaticEnumValueKind valueKind;

  if (ED->isObjC()) {
    // @objc enums must have a raw type that's an ObjC-representable
    // integer type.
    if (!TC.isCIntegerType(ED, rawTy)) {
      TC.diagnose(ED->getInherited().front().getSourceRange().Start,
                  diag::objc_enum_raw_type_not_integer,
                  rawTy);
      ED->getInherited().front().setInvalidType(TC.Context);
      return;
    }
    valueKind = AutomaticEnumValueKind::Integer;
  } else {
    // Swift enums require that the raw type is convertible from one of the
    // primitive literal protocols.
    auto conformsToProtocol = [&](KnownProtocolKind protoKind) {
        ProtocolDecl *proto = TC.getProtocol(ED->getLoc(), protoKind);
        return TC.conformsToProtocol(rawTy, proto, ED->getDeclContext(), None);
    };

    static auto otherLiteralProtocolKinds = {
      KnownProtocolKind::ExpressibleByFloatLiteral,
      KnownProtocolKind::ExpressibleByUnicodeScalarLiteral,
      KnownProtocolKind::ExpressibleByExtendedGraphemeClusterLiteral,
    };

    if (conformsToProtocol(KnownProtocolKind::ExpressibleByIntegerLiteral)) {
      valueKind = AutomaticEnumValueKind::Integer;
    } else if (conformsToProtocol(KnownProtocolKind::ExpressibleByStringLiteral)){
      valueKind = AutomaticEnumValueKind::String;
    } else if (std::any_of(otherLiteralProtocolKinds.begin(),
                           otherLiteralProtocolKinds.end(),
                           conformsToProtocol)) {
      valueKind = AutomaticEnumValueKind::None;
    } else {
      TC.diagnose(ED->getInherited().front().getSourceRange().Start,
                  diag::raw_type_not_literal_convertible,
                  rawTy);
      ED->getInherited().front().setInvalidType(TC.Context);
      return;
    }
  }

  // We need at least one case to have a raw value.
  if (ED->getAllElements().empty()) {
    TC.diagnose(ED->getInherited().front().getSourceRange().Start,
                diag::empty_enum_raw_type);
    return;
  }

  // Check the raw values of the cases.
  LiteralExpr *prevValue = nullptr;
  EnumElementDecl *lastExplicitValueElt = nullptr;

  // Keep a map we can use to check for duplicate case values.
  llvm::SmallDenseMap<RawValueKey, RawValueSource, 8> uniqueRawValues;

  for (auto elt : ED->getAllElements()) {
    // Skip if the raw value expr has already been checked.
    if (elt->getTypeCheckedRawValueExpr())
      continue;

    // Make sure the element is checked out before we poke at it.
    TC.validateDecl(elt);
    
    if (elt->isInvalid())
      continue;

    // We don't yet support raw values on payload cases.
    if (elt->hasAssociatedValues()) {
      TC.diagnose(elt->getLoc(),
                  diag::enum_with_raw_type_case_with_argument);
      TC.diagnose(ED->getInherited().front().getSourceRange().Start,
                  diag::enum_raw_type_here, rawTy);
      elt->setInvalid();
      continue;
    }
    
    // Check the raw value expr, if we have one.
    if (auto *rawValue = elt->getRawValueExpr()) {
      Expr *typeCheckedExpr = rawValue;
      auto resultTy = TC.typeCheckExpression(typeCheckedExpr, ED,
                                             TypeLoc::withoutLoc(rawTy),
                                             CTP_EnumCaseRawValue);
      if (resultTy) {
        elt->setTypeCheckedRawValueExpr(typeCheckedExpr);
      }
      lastExplicitValueElt = elt;
    } else {
      // If the enum element has no explicit raw value, try to
      // autoincrement from the previous value, or start from zero if this
      // is the first element.
      auto nextValue = getAutomaticRawValueExpr(TC, valueKind, elt, prevValue);
      if (!nextValue) {
        elt->setInvalid();
        break;
      }
      elt->setRawValueExpr(nextValue);
      Expr *typeChecked = nextValue;
      auto resultTy = TC.typeCheckExpression(
          typeChecked, ED, TypeLoc::withoutLoc(rawTy), CTP_EnumCaseRawValue);
      if (resultTy)
        elt->setTypeCheckedRawValueExpr(typeChecked);
    }
    prevValue = elt->getRawValueExpr();
    assert(prevValue && "continued without setting raw value of enum case");

    // If we didn't find a valid initializer (maybe the initial value was
    // incompatible with the raw value type) mark the entry as being erroneous.
    if (!elt->getTypeCheckedRawValueExpr()) {
      elt->setInvalid();
      continue;
    }

    TC.checkEnumElementErrorHandling(elt);

    // Find the type checked version of the LiteralExpr used for the raw value.
    // this is unfortunate, but is needed because we're digging into the
    // literals to get information about them, instead of accepting general
    // expressions.
    LiteralExpr *rawValue = elt->getRawValueExpr();
    if (!rawValue->getType()) {
      elt->getTypeCheckedRawValueExpr()->forEachChildExpr([&](Expr *E)->Expr* {
        if (E->getKind() == rawValue->getKind())
          rawValue = cast<LiteralExpr>(E);
        return E;
      });
      elt->setRawValueExpr(rawValue);
    }

    prevValue = rawValue;
    assert(prevValue && "continued without setting raw value of enum case");

    // Check that the raw value is unique.
    RawValueKey key(rawValue);
    RawValueSource source{elt, lastExplicitValueElt};

    auto insertIterPair = uniqueRawValues.insert({key, source});
    if (insertIterPair.second)
      continue;

    // Diagnose the duplicate value.
    SourceLoc diagLoc = elt->getRawValueExpr()->isImplicit()
        ? elt->getLoc() : elt->getRawValueExpr()->getLoc();
    TC.diagnose(diagLoc, diag::enum_raw_value_not_unique);
    assert(lastExplicitValueElt &&
           "should not be able to have non-unique raw values when "
           "relying on autoincrement");
    if (lastExplicitValueElt != elt &&
        valueKind == AutomaticEnumValueKind::Integer) {
      TC.diagnose(lastExplicitValueElt->getRawValueExpr()->getLoc(),
                  diag::enum_raw_value_incrementing_from_here);
    }

    RawValueSource prevSource = insertIterPair.first->second;
    auto foundElt = prevSource.sourceElt;
    diagLoc = foundElt->getRawValueExpr()->isImplicit()
        ? foundElt->getLoc() : foundElt->getRawValueExpr()->getLoc();
    TC.diagnose(diagLoc, diag::enum_raw_value_used_here);
    if (foundElt != prevSource.lastExplicitValueElt &&
        valueKind == AutomaticEnumValueKind::Integer) {
      if (prevSource.lastExplicitValueElt)
        TC.diagnose(prevSource.lastExplicitValueElt
                      ->getRawValueExpr()->getLoc(),
                    diag::enum_raw_value_incrementing_from_here);
      else
        TC.diagnose(ED->getAllElements().front()->getLoc(),
                    diag::enum_raw_value_incrementing_from_zero);
    }
  }
}

/// Walks up the override chain for \p CD until it finds an initializer that is
/// required and non-implicit. If no such initializer exists, returns the
/// declaration where \c required was introduced (i.e. closest to the root
/// class).
static const ConstructorDecl *
findNonImplicitRequiredInit(const ConstructorDecl *CD) {
  while (CD->isImplicit()) {
    auto *overridden = CD->getOverriddenDecl();
    if (!overridden || !overridden->isRequired())
      break;
    CD = overridden;
  }
  return CD;
}

static void checkVarBehavior(VarDecl *decl, TypeChecker &TC) {
  // No behavior, no problems.
  if (!decl->hasBehavior())
    return;
  
  // Don't try to check the behavior if we already encountered an error.
  if (decl->getType()->hasError())
    return;
  
  auto behavior = decl->getMutableBehavior() ;
  // We should have set up the conformance during validation.
  assert(behavior->Conformance.hasValue());
  // If the behavior couldn't be resolved during validation, we can't really
  // do much more.
  auto *conformance = behavior->Conformance.getValue();
  if (!conformance)
    return;
  
  assert(behavior->ValueDecl);
  
  auto dc = decl->getDeclContext();
  auto behaviorSelf = conformance->getType();
  auto behaviorInterfaceSelf = behaviorSelf->mapTypeOutOfContext();
  auto behaviorProto = conformance->getProtocol();
  auto behaviorProtoTy = behaviorProto->getDeclaredType();
  
  // Treat any inherited protocols as constraints on `Self`, and gather
  // conformances from the containing type.
  //
  // It'd probably be cleaner to model as `where Self: ...` constraints when
  // we have those.
  //
  // TODO: Handle non-protocol requirements ('class', base class, etc.)
  for (auto refinedProto : behaviorProto->getInheritedProtocols()) {
    // A behavior in non-type or static context is never going to be able to
    // satisfy Self constraints (until we give structural types that ability).
    // Give a tailored error message for this case.
    if (!dc->isTypeContext() || decl->isStatic()) {
      TC.diagnose(behavior->getLoc(),
                  diag::property_behavior_with_self_requirement_not_in_type,
                  behaviorProto->getName());
      break;
    }
    
    // Blame conformance failures on the containing type.
    SourceLoc blameLoc;
    if (auto nomTy = dyn_cast<NominalTypeDecl>(dc)) {
      blameLoc = nomTy->getLoc();
    } else if (auto ext = dyn_cast<ExtensionDecl>(dc)) {
      blameLoc = ext->getLoc();
    } else {
      llvm_unreachable("unknown type context type?!");
    }

    auto inherited = TC.conformsToProtocol(behaviorSelf, refinedProto, dc,
                                           ConformanceCheckFlags::Used,
                                           blameLoc);
    if (!inherited || !inherited->isConcrete()) {
      // Add some notes that the conformance is behavior-driven.
      TC.diagnose(behavior->getLoc(),
                  diag::self_conformance_required_by_property_behavior,
                  refinedProto->getName(),
                  behaviorProto->getName());
      conformance->setInvalid();
    }
  }
  
  // Try to satisfy the protocol requirements from the property's traits.
  
  auto unknownRequirement = [&](ValueDecl *requirement) {
    // Diagnose requirements that can't be satisfied from the behavior decl.
    TC.diagnose(behavior->getLoc(),
                diag::property_behavior_unknown_requirement,
                behaviorProto->getName(),
                requirement->getBaseName());
    TC.diagnose(requirement->getLoc(),
                diag::property_behavior_unknown_requirement_here);
    conformance->setInvalid();
  };
  
  conformance->setState(ProtocolConformanceState::CheckingTypeWitnesses);
  
  // First, satisfy any associated type requirements.
  Substitution valueSub;
  AssociatedTypeDecl *valueReqt = nullptr;
  for (auto assocTy : behaviorProto->getAssociatedTypeMembers()) {
  
    // Match a Value associated type requirement to the property type.
    if (assocTy->getName() != TC.Context.Id_Value) {
      unknownRequirement(assocTy);
      continue;
    }
    
    valueReqt = assocTy;
    
    // Check for required protocol conformances.
    // TODO: Handle secondary 'where' constraints on the associated types.
    // TODO: Handle non-protocol constraints ('class', base class)
    auto propTy = decl->getType();
    SmallVector<ProtocolConformanceRef, 4> valueConformances;
    for (auto proto : assocTy->getConformingProtocols()) {
      auto valueConformance = TC.conformsToProtocol(propTy, proto, dc,
                                                    ConformanceCheckFlags::Used,
                                                    decl->getLoc());
      if (!valueConformance) {
        conformance->setInvalid();
        TC.diagnose(behavior->getLoc(),
                    diag::value_conformance_required_by_property_behavior,
                    proto->getName(),
                    behaviorProto->getName());
        goto next_requirement;
      }
      valueConformances.push_back(*valueConformance);
    }
    
    {
      auto conformancesCopy = TC.Context.AllocateCopy(valueConformances);
      valueSub = Substitution(propTy, conformancesCopy);
      // FIXME: Maybe we should synthesize an implicit TypeAliasDecl? We
      // really don't want the behavior conformances to show up in the
      // enclosing namespace though.
      conformance->setTypeWitness(assocTy, propTy, /*typeDecl*/ nullptr);
    }
  next_requirement:;
  }
  
  // Bail out if we didn't resolve type witnesses.
  if (conformance->isInvalid()) {
    decl->markInvalid();
    return;
  }

  // Build a Substitution vector from the conformance.
  auto conformanceMem =
    TC.Context.AllocateUninitialized<ProtocolConformanceRef>(1);
  auto selfConformance = new ((void*)conformanceMem.data())
    ProtocolConformanceRef(conformance);
  Substitution allInterfaceSubs[] = {
    Substitution(behaviorInterfaceSelf, *selfConformance),
    Substitution(decl->getInterfaceType(), valueSub.getConformances()),
  };
  Substitution allContextSubs[] = {
    Substitution(behaviorSelf, *selfConformance),
    Substitution(decl->getType(), valueSub.getConformances()),
  };

  SubstitutionList interfaceSubs = allInterfaceSubs;
  if (interfaceSubs.back().getConformances().empty())
    interfaceSubs = interfaceSubs.drop_back();

  SubstitutionList contextSubs = allContextSubs;
  if (contextSubs.back().getConformances().empty())
    contextSubs = contextSubs.drop_back();

  // Now that type witnesses are done, satisfy property and method requirements.
  conformance->setState(ProtocolConformanceState::Checking);

  bool requiresParameter = false;
  for (auto requirementDecl : behaviorProto->getMembers()) {
    auto requirement = dyn_cast<ValueDecl>(requirementDecl);
    if (!requirement)
      continue;
    if (isa<AssociatedTypeDecl>(requirement))
      continue;
    
    if (auto varReqt = dyn_cast<VarDecl>(requirement)) {
      // Match a storage requirement.
      if (varReqt->getName() == TC.Context.Id_storage) {
        TC.validateDecl(varReqt);

        auto storageTy = varReqt->getInterfaceType();
        // We need an initStorage extension method to initialize this storage.
        // Should have the signature:
        //   static func initStorage() -> Storage
        // for default initialization, or:
        //   static func initStorage(_: Value) -> Storage
        // for parameterized initialization.
        auto expectedDefaultInitStorageTy =
          FunctionType::get(TC.Context.TheEmptyTupleType, storageTy);
        Type valueTy = DependentMemberType::get(
                                          behaviorProto->getSelfInterfaceType(),
                                          valueReqt);
        
        auto expectedParameterizedInitStorageTy =
          FunctionType::get(valueTy, storageTy);

        auto lookup = TC.lookupMember(dc, behaviorProtoTy,
                                      TC.Context.Id_initStorage);
        FuncDecl *defaultInitStorageDecl = nullptr;
        FuncDecl *parameterizedInitStorageDecl = nullptr;
        for (auto found : lookup) {
          if (auto foundFunc = dyn_cast<FuncDecl>(found.getValueDecl())) {
            if (!foundFunc->isStatic())
              continue;
            auto methodTy = foundFunc->getInterfaceType()
              ->castTo<AnyFunctionType>()
              ->getResult();
            if (methodTy->isEqual(expectedDefaultInitStorageTy))
              defaultInitStorageDecl = foundFunc;
            else if (methodTy->isEqual(expectedParameterizedInitStorageTy))
              parameterizedInitStorageDecl = foundFunc;
          }
        }
        
        if (defaultInitStorageDecl && parameterizedInitStorageDecl) {
          TC.diagnose(behavior->getLoc(),
                      diag::property_behavior_protocol_reqt_ambiguous,
                      TC.Context.Id_initStorage);
          TC.diagnose(defaultInitStorageDecl->getLoc(),
                      diag::property_behavior_protocol_reqt_here,
                      TC.Context.Id_initStorage);
          TC.diagnose(parameterizedInitStorageDecl->getLoc(),
                      diag::property_behavior_protocol_reqt_here,
                      TC.Context.Id_initStorage);
          conformance->setInvalid();
          continue;
        }
        
        if (!defaultInitStorageDecl && !parameterizedInitStorageDecl) {
          TC.diagnose(behavior->getLoc(),
                      diag::property_behavior_protocol_no_initStorage,
                      expectedDefaultInitStorageTy,
                      expectedParameterizedInitStorageTy);
          for (auto found : lookup)
            TC.diagnose(found.getValueDecl()->getLoc(),
                        diag::found_candidate);
          conformance->setInvalid();
          continue;
        }
        
        // TODO: Support storage-backed behaviors in non-type contexts.
        if (!dc->isTypeContext()) {
          TC.diagnose(behavior->getLoc(),
                      diag::property_behavior_with_feature_not_supported,
                      behaviorProto->getName(), "storage");
          conformance->setInvalid();
          continue;
        }
        
        // TODO: Support destructured initializers such as
        // `var (a, b) = tuple __behavior blah`. This ought to be supportable
        // if the behavior allows for DI-like initialization.
        if (parameterizedInitStorageDecl
            && !isa<NamedPattern>(decl->getParentPattern()
                                      ->getSemanticsProvidingPattern())) {
          TC.diagnose(decl->getLoc(),
                      diag::property_behavior_unsupported_initializer);
          auto PBD = decl->getParentPatternBinding();
          unsigned entryIndex = PBD->getPatternEntryIndexForVarDecl(decl);
          PBD->setInit(entryIndex, nullptr);
          PBD->setInitializerChecked(entryIndex);
          continue;
        }
        
        // Instantiate the storage next to us in the enclosing scope.
        TC.completePropertyBehaviorStorage(decl, varReqt,
                                           defaultInitStorageDecl,
                                           parameterizedInitStorageDecl,
                                           behaviorSelf,
                                           storageTy,
                                           conformance,
                                           interfaceSubs,
                                           contextSubs);
        continue;
      }
    } else if (auto func = dyn_cast<FuncDecl>(requirement)) {
      // Handle accessors as part of their property.
      if (isa<AccessorDecl>(func))
        continue;
      
      // Handle a parameter block requirement.
      if (func->getName() == TC.Context.Id_parameter) {
        requiresParameter = true;
        
        TC.validateDecl(func);
        
        // The requirement should be for a nongeneric, nonmutating instance
        // method.
        if (func->isStatic() || func->isGeneric() || func->isMutating()) {
          TC.diagnose(behavior->getLoc(),
                      diag::property_behavior_invalid_parameter_reqt,
                      behaviorProto->getName());
          TC.diagnose(varReqt->getLoc(),
                      diag::property_behavior_protocol_reqt_here,
                      TC.Context.Id_parameter);
          conformance->setInvalid();
          continue;
        }
        
        // The declaration must have a parameter.
        if (!decl->getBehavior()->Param) {
          TC.diagnose(behavior->getLoc(),
                      diag::property_behavior_requires_parameter,
                      behaviorProto->getName(),
                      decl->getName());
          conformance->setInvalid();
          continue;
        }
        
        // TODO: Support parameter requirements in non-type contexts.
        if (!dc->isTypeContext()) {
          TC.diagnose(behavior->getLoc(),
                      diag::property_behavior_with_feature_not_supported,
                      behaviorProto->getName(), "parameter requirement");
          conformance->setInvalid();
          continue;
        }
        
        // Build the parameter witness method.
        TC.completePropertyBehaviorParameter(decl, func,
                                             conformance,
                                             interfaceSubs,
                                             contextSubs);
        continue;
      }
    }

    unknownRequirement(requirement);
  }
  
  // If the property was declared with an initializer, but the behavior
  // didn't use it, complain.
  if (decl->getParentInitializer()) {
    TC.diagnose(decl->getParentInitializer()->getLoc(),
                diag::property_behavior_invalid_initializer,
                behaviorProto->getName());
  }
  
  // If the property was declared with a parameter, but the behavior didn't
  // use it, complain.
  // TODO: The initializer could eventually be consumed by DI-style
  // initialization.
  if (!requiresParameter && decl->getBehavior()->Param) {
    TC.diagnose(decl->getBehavior()->Param->getLoc(),
                diag::property_behavior_invalid_parameter,
                behaviorProto->getName());
  }
  
  // Bail out if we didn't resolve method witnesses.
  if (conformance->isInvalid()) {
    decl->markInvalid();
    return;
  }

  conformance->setState(ProtocolConformanceState::Complete);

  // Check that the 'value' property from the protocol matches the
  // declared property type in context.
  auto sig = behaviorProto->getGenericSignatureOfContext();
  auto map = sig->getSubstitutionMap(interfaceSubs);
  auto substValueTy = behavior->ValueDecl->getInterfaceType().subst(map);
  
  if (!substValueTy->isEqual(decl->getInterfaceType())) {
    TC.diagnose(behavior->getLoc(),
                diag::property_behavior_value_type_doesnt_match,
                behaviorProto->getName(),
                substValueTy,
                decl->getName(),
                decl->getInterfaceType());
    TC.diagnose(behavior->ValueDecl->getLoc(),
                diag::property_behavior_value_decl_here);
    decl->markInvalid();
    return;
  }

  // Synthesize the bodies of the property's accessors now, forwarding to the
  // 'value' implementation.
  TC.completePropertyBehaviorAccessors(decl, behavior->ValueDecl,
                                       decl->getType(),
                                       interfaceSubs, contextSubs);
  
  return;
}

/// For building the higher-than component of the diagnostic path,
/// we use the visited set, which we've embellished with information
/// about how we reached a particular node.  This is reasonable because
/// we need to maintain the set anyway.
static void buildHigherThanPath(PrecedenceGroupDecl *last,
                          const llvm::DenseMap<PrecedenceGroupDecl *,
                                        PrecedenceGroupDecl *> &visitedFrom,
                          raw_ostream &out) {
  auto it = visitedFrom.find(last);
  assert(it != visitedFrom.end());
  auto from = it->second;
  if (from) {
    buildHigherThanPath(from, visitedFrom, out);
  }
  out << last->getName() << " -> ";
}

/// For building the lower-than component of the diagnostic path,
/// we just do a depth-first search to find a path.
static bool buildLowerThanPath(PrecedenceGroupDecl *start,
                               PrecedenceGroupDecl *target,
                               raw_ostream &out) {
  if (start == target) {
    out << start->getName();
    return true;
  }

  if (start->isInvalid())
    return false;

  for (auto &rel : start->getLowerThan()) {
    if (rel.Group && buildLowerThanPath(rel.Group, target, out)) {
      out << " -> " << start->getName();
      return true;
    }
  }

  return false;
}

static void checkPrecedenceCircularity(TypeChecker &TC,
                                       PrecedenceGroupDecl *PGD) {
  // Don't diagnose if this group is already marked invalid.
  if (PGD->isInvalid()) return;

  // The cycle doesn't necessarily go through this specific group,
  // so we need a proper visited set to avoid infinite loops.  We
  // also record a back-reference so that we can easily reconstruct
  // the cycle.
  llvm::DenseMap<PrecedenceGroupDecl*, PrecedenceGroupDecl*> visitedFrom;
  SmallVector<PrecedenceGroupDecl*, 4> stack;

  // Fill out the targets set.
  llvm::SmallPtrSet<PrecedenceGroupDecl*, 4> targets;
  stack.push_back(PGD);
  do {
    auto cur = stack.pop_back_val();

    // If we reach an invalid node, just bail out.
    if (cur->isInvalid()) {
      PGD->setInvalid();
      return;
    }

    targets.insert(cur);

    for (auto &rel : cur->getLowerThan()) {
      if (!rel.Group) continue;

      // We can't have cycles in the lower-than relationship
      // because it has to point outside of the module.

      stack.push_back(rel.Group);
    }
  } while (!stack.empty());

  // Make sure that the PGD is its own source.
  visitedFrom.insert({PGD, nullptr});

  stack.push_back(PGD);
  do {
    auto cur = stack.pop_back_val();

    // If we reach an invalid node, just bail out.
    if (cur->isInvalid()) {
      PGD->setInvalid();
      return;
    }

    for (auto &rel : cur->getHigherThan()) {
      if (!rel.Group) continue;

      // Check whether we've reached a target declaration.
      if (!targets.count(rel.Group)) {
        // If not, check whether we've visited this group before.
        if (visitedFrom.insert({rel.Group, cur}).second) {
          // If not, add it to the queue.
          stack.push_back(rel.Group);
        }

        // Note that we'll silently ignore cycles that don't go through PGD.
        // We should eventually process the groups that are involved.
        continue;
      }

      // Otherwise, we have something to report.
      SmallString<128> path;
      {
        llvm::raw_svector_ostream str(path);

        // Build the higherThan portion of the path (PGD -> cur).
        buildHigherThanPath(cur, visitedFrom, str);

        // Build the lowerThan portion of the path (rel.Group -> PGD).
        buildLowerThanPath(PGD, rel.Group, str);
      }
      
      TC.diagnose(PGD->getHigherThanLoc(), diag::precedence_group_cycle, path);
      PGD->setInvalid();
      return;
    }
  } while (!stack.empty());
}

/// Do a primitive lookup for the given precedence group.  This does
/// not validate the precedence group or diagnose if the lookup fails
/// (other than via ambiguity); for that, use
/// TypeChecker::lookupPrecedenceGroup.
///
/// Pass an invalid source location to suppress diagnostics.
static PrecedenceGroupDecl *
lookupPrecedenceGroupPrimitive(DeclContext *dc, Identifier name,
                               SourceLoc nameLoc) {
  if (auto sf = dc->getParentSourceFile()) {
    bool cascading = dc->isCascadingContextForLookup(false);
    return sf->lookupPrecedenceGroup(name, cascading, nameLoc);
  } else {
    return dc->getParentModule()->lookupPrecedenceGroup(name, nameLoc);
  }
}

void TypeChecker::validateDecl(PrecedenceGroupDecl *PGD) {
  checkDeclAttributesEarly(PGD);
  checkDeclAttributes(PGD);

  if (PGD->isInvalid() || PGD->hasValidationStarted())
    return;
  PGD->setIsBeingValidated();
  SWIFT_DEFER { PGD->setIsBeingValidated(false); };

  bool isInvalid = false;

  // Validate the higherThan relationships.
  bool addedHigherThan = false;
  for (auto &rel : PGD->getMutableHigherThan()) {
    if (rel.Group) continue;

    auto group = lookupPrecedenceGroupPrimitive(PGD->getDeclContext(),
                                                rel.Name, rel.NameLoc);
    if (group) {
      rel.Group = group;
      validateDecl(group);
      addedHigherThan = true;
    } else if (!PGD->isInvalid()) {
      diagnose(rel.NameLoc, diag::unknown_precedence_group, rel.Name);
      isInvalid = true;
    }
  }

  // Validate the lowerThan relationships.
  for (auto &rel : PGD->getMutableLowerThan()) {
    if (rel.Group) continue;

    auto dc = PGD->getDeclContext();
    auto group = lookupPrecedenceGroupPrimitive(dc, rel.Name, rel.NameLoc);
    if (group) {
      if (group->getDeclContext()->getParentModule()
            == dc->getParentModule()) {
        if (!PGD->isInvalid()) {
          diagnose(rel.NameLoc, diag::precedence_group_lower_within_module);
          diagnose(group->getNameLoc(), diag::precedence_group_declared_here);
          isInvalid = true;
        }
      } else {
        rel.Group = group;
        validateDecl(group);
      }
    } else if (!PGD->isInvalid()) {
      diagnose(rel.NameLoc, diag::unknown_precedence_group, rel.Name);
      isInvalid = true;
    }
  }

  // Check for circularity.
  if (addedHigherThan) {
    checkPrecedenceCircularity(*this, PGD);
  }

  if (isInvalid) PGD->setInvalid();
}

PrecedenceGroupDecl *TypeChecker::lookupPrecedenceGroup(DeclContext *dc,
                                                        Identifier name,
                                                        SourceLoc nameLoc) {
  auto group = lookupPrecedenceGroupPrimitive(dc, name, nameLoc);
  if (group) {
    validateDecl(group);
  } else if (nameLoc.isValid()) {
    // FIXME: avoid diagnosing this multiple times per source file.
    diagnose(nameLoc, diag::unknown_precedence_group, name);
  }
  return group;
}

/// Validate the given operator declaration.
///
/// This establishes key invariants, such as an InfixOperatorDecl's
/// reference to its precedence group and the transitive validity of that
/// group.
void TypeChecker::validateDecl(OperatorDecl *OD) {
  checkDeclAttributesEarly(OD);
  checkDeclAttributes(OD);

  if (auto IOD = dyn_cast<InfixOperatorDecl>(OD)) {
    if (!IOD->getPrecedenceGroup()) {
      PrecedenceGroupDecl *group = nullptr;

      // If a name was given, try to look it up.
      Identifier name = IOD->getPrecedenceGroupName();
      if (!name.empty()) {
        auto loc = IOD->getPrecedenceGroupNameLoc();
        group = lookupPrecedenceGroupPrimitive(OD->getDeclContext(), name, loc);
        if (!group && !IOD->isInvalid()) {
          diagnose(loc, diag::unknown_precedence_group, name);
          IOD->setInvalid();
        }
      }

      // If that fails, or if a name was not given, use the default
      // precedence group.
      if (!group) {
        group = lookupPrecedenceGroupPrimitive(OD->getDeclContext(),
                                               Context.Id_DefaultPrecedence,
                                               SourceLoc());
        if (!group && name.empty() && !IOD->isInvalid()) {
          diagnose(OD->getLoc(), diag::missing_builtin_precedence_group,
                   Context.Id_DefaultPrecedence);
        }
      }

      // Validate the precedence group.
      if (group) {
        validateDecl(group);
        IOD->setPrecedenceGroup(group);
      }
    }
  }
}

static bool doesContextHaveValueSemantics(DeclContext *dc) {
  if (Type contextTy = dc->getDeclaredInterfaceType())
    return !contextTy->hasReferenceSemantics();
  return false;
}

static void validateSelfAccessKind(TypeChecker &TC, FuncDecl *FD) {
  // Validate the mutating attribute if present, and install it into the bit
  // on funcdecl (instead of just being a DeclAttribute).
  if (FD->getAttrs().hasAttribute<MutatingAttr>())
    FD->setSelfAccessKind(SelfAccessKind::Mutating);
  else if (FD->getAttrs().hasAttribute<NonMutatingAttr>())
    FD->setSelfAccessKind(SelfAccessKind::NonMutating);
  else if (FD->getAttrs().hasAttribute<ConsumingAttr>())
    FD->setSelfAccessKind(SelfAccessKind::__Consuming);

  if (FD->isMutating()) {
    if (!FD->isInstanceMember() ||
        !doesContextHaveValueSemantics(FD->getDeclContext()))
      FD->setSelfAccessKind(SelfAccessKind::NonMutating);
  }
}

static bool validateAccessorIsMutating(TypeChecker &TC, FuncDecl *accessor) {
  assert(accessor && "accessor not present!");
  validateSelfAccessKind(TC, accessor);
  return accessor->isMutating();
}

static bool computeIsGetterMutating(TypeChecker &TC,
                                    AbstractStorageDecl *storage) {
  switch (storage->getStorageKind()) {
  case AbstractStorageDecl::Stored:
    return false;

  case AbstractStorageDecl::StoredWithObservers:
  case AbstractStorageDecl::StoredWithTrivialAccessors:
  case AbstractStorageDecl::InheritedWithObservers:
  case AbstractStorageDecl::ComputedWithMutableAddress:
  case AbstractStorageDecl::Computed:
  case AbstractStorageDecl::AddressedWithTrivialAccessors:
  case AbstractStorageDecl::AddressedWithObservers:
    return validateAccessorIsMutating(TC, storage->getGetter());

  case AbstractStorageDecl::Addressed:
    return validateAccessorIsMutating(TC, storage->getAddressor());
  }

  llvm_unreachable("bad storage kind");
}

static bool computeIsSetterMutating(TypeChecker &TC,
                                    AbstractStorageDecl *storage) {
  switch (storage->getStorageKind()) {
  case AbstractStorageDecl::Stored:
  case AbstractStorageDecl::StoredWithTrivialAccessors:
    // Instance member setters are mutating; static property setters and
    // top-level setters are not.
    return storage->isInstanceMember() &&
           doesContextHaveValueSemantics(storage->getDeclContext());

  case AbstractStorageDecl::StoredWithObservers:
  case AbstractStorageDecl::InheritedWithObservers:
  case AbstractStorageDecl::Computed:
    if (auto setter = storage->getSetter())
      return validateAccessorIsMutating(TC, setter);
    return false;

  case AbstractStorageDecl::Addressed:
  case AbstractStorageDecl::AddressedWithTrivialAccessors:
  case AbstractStorageDecl::AddressedWithObservers:
  case AbstractStorageDecl::ComputedWithMutableAddress:
    if (auto addressor = storage->getMutableAddressor())
      return validateAccessorIsMutating(TC, addressor);
    return false;
  }
  llvm_unreachable("bad storage kind");
}

static void validateAbstractStorageDecl(TypeChecker &TC,
                                        AbstractStorageDecl *storage) {
  // isGetterMutating and isSetterMutating are part of the signature
  // of a storage declaration and need to be validated immediately.
  storage->setIsGetterMutating(computeIsGetterMutating(TC, storage));
  storage->setIsSetterMutating(computeIsSetterMutating(TC, storage));

  // We can't delay validation of getters and setters on @objc properties,
  // because if they never get validated at all then conformance checkers
  // will complain about selector mismatches.
  if (storage->isObjC()) {
    if (auto *getter = storage->getGetter())
      TC.validateDecl(getter);
    if (auto *setter = storage->getSetter())
      TC.validateDecl(setter);
  }

  // Create a materializeForSet function if necessary.  This needs to
  // happen immediately so that subclass materializeForSet functions
  // will be properly marked as overriding it.
  if (storage->hasAccessorFunctions())
    maybeAddMaterializeForSet(storage, TC);
  if (storage->isFinal())
    makeFinal(TC.Context, storage->getMaterializeForSetFunc());

  // Everything else about the accessors can wait until finalization.
  TC.DeclsToFinalize.insert(storage);
}

static void finalizeAbstractStorageDecl(TypeChecker &TC,
                                        AbstractStorageDecl *storage) {
  if (auto getter = storage->getGetter())
    TC.validateDecl(getter);
  if (auto setter = storage->getSetter())
    TC.validateDecl(setter);
  if (auto materializeForSet = storage->getMaterializeForSetFunc())
    TC.validateDecl(materializeForSet);
  if (storage->hasAddressors()) {
    if (auto addressor = storage->getAddressor())
      TC.validateDecl(addressor);
    if (auto addressor = storage->getMutableAddressor())
      TC.validateDecl(addressor);
  }
}

namespace {
class DeclChecker : public DeclVisitor<DeclChecker> {
public:
  TypeChecker &TC;

  explicit DeclChecker(TypeChecker &TC) : TC(TC) {}

  void visit(Decl *decl) {
    FrontendStatsTracer StatsTracer(TC.Context.Stats, "typecheck-decl", decl);
    PrettyStackTraceDecl StackTrace("type-checking", decl);
    
    DeclVisitor<DeclChecker>::visit(decl);

    TC.checkUnsupportedProtocolType(decl);

    if (auto VD = dyn_cast<ValueDecl>(decl)) {
      checkRedeclaration(TC, VD);
      
      // If this is a member of a nominal type, don't allow it to have a name of
      // "Type" or "Protocol" since we reserve the X.Type and X.Protocol
      // expressions to mean something builtin to the language.  We *do* allow
      // these if they are escaped with backticks though.
      auto &Context = TC.Context;
      if (VD->getDeclContext()->isTypeContext() &&
          (VD->getFullName().isSimpleName(Context.Id_Type) ||
           VD->getFullName().isSimpleName(Context.Id_Protocol)) &&
          VD->getNameLoc().isValid() &&
          Context.SourceMgr.extractText({VD->getNameLoc(), 1}) != "`") {
        TC.diagnose(VD->getNameLoc(), diag::reserved_member_name,
                    VD->getFullName(), VD->getBaseName().getIdentifier().str());
        TC.diagnose(VD->getNameLoc(), diag::backticks_to_escape)
            .fixItReplace(VD->getNameLoc(),
                          "`" + VD->getBaseName().userFacingName().str() + "`");
      }
    }
  }

  //===--------------------------------------------------------------------===//
  // Visit Methods.
  //===--------------------------------------------------------------------===//

  void visitGenericTypeParamDecl(GenericTypeParamDecl *D) {
    llvm_unreachable("cannot reach here");
  }
  
  void visitImportDecl(ImportDecl *ID) {
    TC.checkDeclAttributesEarly(ID);
    TC.checkDeclAttributes(ID);
  }

  void visitOperatorDecl(OperatorDecl *OD) {
    TC.validateDecl(OD);
  }

  void visitPrecedenceGroupDecl(PrecedenceGroupDecl *PGD) {
    TC.validateDecl(PGD);
  }

  void visitMissingMemberDecl(MissingMemberDecl *MMD) {
    llvm_unreachable("should always be type-checked already");
  }

  void visitBoundVariable(VarDecl *VD) {
    TC.validateDecl(VD);

    // Check the behavior.
    checkVarBehavior(VD, TC);

    // WARNING: Anything you put in this function will only be run when the
    // VarDecl is fully type-checked within its own file. It will NOT be run
    // when the VarDecl is merely used from another file.

    // Reject cases where this is a variable that has storage but it isn't
    // allowed.
    if (VD->hasStorage()) {
      // Stored properties in protocols are diagnosed in
      // maybeAddAccessorsToVariable(), to ensure they run when a
      // protocol requirement is validated but not type checked.

      // Enums and extensions cannot have stored instance properties.
      // Static stored properties are allowed, with restrictions
      // enforced below.
      if (isa<EnumDecl>(VD->getDeclContext()) &&
          !VD->isStatic()) {
        // Enums can only have computed properties.
        TC.diagnose(VD->getLoc(), diag::enum_stored_property);
        VD->markInvalid();
      } else if (isa<ExtensionDecl>(VD->getDeclContext()) &&
                 !VD->isStatic()) {
        TC.diagnose(VD->getLoc(), diag::extension_stored_property);
        VD->markInvalid();
      }
      
      // We haven't implemented type-level storage in some contexts.
      if (VD->isStatic()) {
        auto PBD = VD->getParentPatternBinding();
        // Selector for unimplemented_static_var message.
        enum : unsigned {
          Misc,
          GenericTypes,
          Classes,
          ProtocolExtensions
        };
        auto unimplementedStatic = [&](unsigned diagSel) {
          auto staticLoc = PBD->getStaticLoc();
          TC.diagnose(VD->getLoc(), diag::unimplemented_static_var,
                      diagSel, PBD->getStaticSpelling(),
                      diagSel == Classes)
            .highlight(staticLoc);
        };

        auto DC = VD->getDeclContext();

        // Non-stored properties are fine.
        if (!PBD->hasStorage()) {
          // do nothing

        // Stored type variables in a generic context need to logically
        // occur once per instantiation, which we don't yet handle.
        } else if (DC->getAsProtocolExtensionContext()) {
            unimplementedStatic(ProtocolExtensions);
        } else if (DC->isGenericContext()
               && !DC->getGenericSignatureOfContext()->areAllParamsConcrete()) {
          unimplementedStatic(GenericTypes);
        } else if (DC->getAsClassOrClassExtensionContext()) {
          auto StaticSpelling = PBD->getStaticSpelling();
          if (StaticSpelling != StaticSpellingKind::KeywordStatic)
            unimplementedStatic(Classes);
        }
      }
    }

    // Synthesize accessors for lazy, all checking already been performed.
    if (VD->getAttrs().hasAttribute<LazyAttr>() && !VD->isStatic() &&
        !VD->getGetter()->hasBody())
      TC.completeLazyVarImplementation(VD);

    // If this is a willSet/didSet property, synthesize the getter and setter
    // decl.
    if (VD->hasObservers() && !VD->getGetter()->getBody())
      synthesizeObservingAccessors(VD, TC);

    // If this is a get+mutableAddress property, synthesize the setter body.
    if (VD->getStorageKind() == VarDecl::ComputedWithMutableAddress &&
        !VD->getSetter()->getBody()) {
      synthesizeSetterForMutableAddressedStorage(VD, TC);
    }

    // Typecheck any accessors that were previously synthesized
    // (that were previously only validated at point of synthesis)
    if (auto getter = VD->getGetter()) {
      if (getter->hasBody()) {
        TC.typeCheckDecl(getter);
      }
    }
    if (auto setter = VD->getSetter()) {
      if (setter->hasBody()) {
        TC.typeCheckDecl(setter);
      }
    }

    TC.checkDeclAttributes(VD);
  }


  void visitBoundVars(Pattern *P) {
    P->forEachVariable([&] (VarDecl *VD) { this->visitBoundVariable(VD); });
  }

  void visitPatternBindingDecl(PatternBindingDecl *PBD) {
    if (PBD->isBeingValidated())
      return;

    // Check all the pattern/init pairs in the PBD.
    validatePatternBindingEntries(TC, PBD);

    TC.checkDeclAttributesEarly(PBD);

    for (unsigned i = 0, e = PBD->getNumPatternEntries(); i != e; ++i) {
      // Type check each VarDecl that this PatternBinding handles.
      visitBoundVars(PBD->getPattern(i));

      // If we have a type but no initializer, check whether the type is
      // default-initializable. If so, do it.
      if (PBD->getPattern(i)->hasType() &&
          !PBD->getInit(i) &&
          PBD->getPattern(i)->hasStorage() &&
          !PBD->getPattern(i)->getType()->hasError()) {

        // If we have a type-adjusting attribute (like ownership), apply it now.
        if (auto var = PBD->getSingleVar())
          TC.checkTypeModifyingDeclAttributes(var);

        // Decide whether we should suppress default initialization.
        //
        // Note: Swift 4 had a bug where properties with a desugared optional
        // type like Optional<Int> had a half-way behavior where sometimes
        // they behave like they are default initialized, and sometimes not.
        //
        // In Swift 5 mode, use the right condition here, and only default
        // initialize properties with a sugared Optional type.
        //
        // (The restriction to sugared types only comes because we don't have
        // the iterative declaration checker yet; so in general, we cannot
        // look at the type of a property at all, and can only look at the
        // TypeRepr, because we haven't validated the property yet.)
        if (TC.Context.isSwiftVersionAtLeast(5)) {
          if (!PBD->isDefaultInitializable(i))
            continue;
        } else {
          if (PBD->getPattern(i)->isNeverDefaultInitializable())
            continue;
        }

        auto type = PBD->getPattern(i)->getType();
        if (auto defaultInit = buildDefaultInitializer(TC, type)) {
          // If we got a default initializer, install it and re-type-check it
          // to make sure it is properly coerced to the pattern type.
          PBD->setInit(i, defaultInit);
          TC.typeCheckPatternBinding(PBD, i, /*skipApplyingSolution*/false);
        }
      }
    }

    bool isInSILMode = false;
    if (auto sourceFile = PBD->getDeclContext()->getParentSourceFile())
      isInSILMode = sourceFile->Kind == SourceFileKind::SIL;
    bool isTypeContext = PBD->getDeclContext()->isTypeContext();

    // If this is a declaration without an initializer, reject code if
    // uninitialized vars are not allowed.
    for (unsigned i = 0, e = PBD->getNumPatternEntries(); i != e; ++i) {
      auto entry = PBD->getPatternList()[i];
    
      if (entry.getInit() || isInSILMode) continue;
      
      entry.getPattern()->forEachVariable([&](VarDecl *var) {
        // If the variable has no storage, it never needs an initializer.
        if (!var->hasStorage())
          return;

        auto *varDC = var->getDeclContext();

        // Non-member observing properties need an initializer.
        if (var->getStorageKind() == VarDecl::StoredWithObservers &&
            !isTypeContext && !var->isInvalid() && !PBD->isInvalid()) {
          TC.diagnose(var->getLoc(), diag::observingprop_requires_initializer);
          PBD->setInvalid();
          var->setInvalid();
          if (!var->hasType()) {
            var->markInvalid();
          }
          return;
        }

        // Static/class declarations require an initializer unless in a
        // protocol.
        if (var->isStatic() && !isa<ProtocolDecl>(varDC) &&
            !var->isInvalid() && !PBD->isInvalid()) {
          TC.diagnose(var->getLoc(), diag::static_requires_initializer,
                      var->getCorrectStaticSpelling());
          PBD->setInvalid();
          var->setInvalid();
          if (!var->hasType()) {
            var->markInvalid();
          }
          return;
        }

        // Global variables require an initializer (except in top level code).
        if (varDC->isModuleScopeContext() &&
            !varDC->getParentSourceFile()->isScriptMode() &&
            !var->isInvalid() && !PBD->isInvalid()) {
          TC.diagnose(var->getLoc(),
                      diag::global_requires_initializer, var->isLet());
          PBD->setInvalid();
          var->setInvalid();
          if (!var->hasType()) {
            var->markInvalid();
          }
          return;
        }
      });
    }

    TC.checkDeclAttributes(PBD);
    checkAccessControl(TC, PBD);

    // If the initializers in the PBD aren't checked yet, do so now.
    for (unsigned i = 0, e = PBD->getNumPatternEntries(); i != e; ++i) {
      if (!PBD->isInitializerChecked(i) && PBD->getInit(i))
        TC.typeCheckPatternBinding(PBD, i, /*skipApplyingSolution*/false);
    }
  }

  void visitSubscriptDecl(SubscriptDecl *SD) {
    TC.validateDecl(SD);
    TC.checkDeclAttributes(SD);
    checkAccessControl(TC, SD);
  }

  void visitTypeAliasDecl(TypeAliasDecl *TAD) {
    TC.checkDeclAttributesEarly(TAD);
    TC.computeAccessLevel(TAD);

    TC.validateDecl(TAD);
    TC.checkDeclAttributes(TAD);
    checkAccessControl(TC, TAD);
  }
  
  void visitAssociatedTypeDecl(AssociatedTypeDecl *AT) {
    TC.validateDecl(AT);

    auto *proto = AT->getProtocol();
    if (proto->isObjC()) {
      TC.diagnose(AT->getLoc(),
                  diag::associated_type_objc,
                  AT->getName(),
                  proto->getName());
    }

    checkAccessControl(TC, AT);
  }

  void checkUnsupportedNestedType(NominalTypeDecl *NTD) {
    TC.diagnoseInlinableLocalType(NTD);

    // We don't support protocols outside the top level of a file.
    if (isa<ProtocolDecl>(NTD) &&
        !NTD->getParent()->isModuleScopeContext()) {
      TC.diagnose(NTD->getLoc(),
                  diag::unsupported_nested_protocol,
                  NTD->getName());
      return;
    }

    // We don't support nested types in generics yet.
    if (NTD->isGenericContext()) {
      auto DC = NTD->getDeclContext();
      if (auto proto = DC->getAsProtocolOrProtocolExtensionContext()) {
        if (DC->getAsProtocolExtensionContext()) {
          TC.diagnose(NTD->getLoc(),
                      diag::unsupported_type_nested_in_protocol_extension,
                      NTD->getName(),
                      proto->getName());
        } else {
          TC.diagnose(NTD->getLoc(),
                      diag::unsupported_type_nested_in_protocol,
                      NTD->getName(),
                      proto->getName());
        }
      }

      if (DC->isLocalContext() && DC->isGenericContext()) {
        // A local generic context is a generic function.
        if (auto AFD = dyn_cast<AbstractFunctionDecl>(DC)) {
          TC.diagnose(NTD->getLoc(),
                      diag::unsupported_type_nested_in_generic_function,
                      NTD->getName(),
                      AFD->getFullName());
        } else {
          TC.diagnose(NTD->getLoc(),
                      diag::unsupported_type_nested_in_generic_closure,
                      NTD->getName());
        }
      }
    }
  }

  void visitEnumDecl(EnumDecl *ED) {
    TC.checkDeclAttributesEarly(ED);
    TC.computeAccessLevel(ED);

    checkUnsupportedNestedType(ED);

    TC.validateDecl(ED);
    TC.DeclsToFinalize.remove(ED);
    ED->setHasValidatedLayout();

    {
      // Check for circular inheritance of the raw type.
      SmallVector<EnumDecl *, 8> path;
      path.push_back(ED);
      checkCircularity(TC, ED, diag::circular_enum_inheritance,
                       diag::enum_here, path);
    }

    for (Decl *member : ED->getMembers())
      visit(member);

    TC.checkDeclAttributes(ED);
    checkAccessControl(TC, ED);

    if (ED->hasRawType() && !ED->isObjC()) {
      // ObjC enums have already had their raw values checked, but pure Swift
      // enums haven't.
      checkEnumRawValues(TC, ED);
    }

    TC.checkDeclCircularity(ED);
    TC.ConformanceContexts.push_back(ED);
  }

  void visitStructDecl(StructDecl *SD) {
    TC.checkDeclAttributesEarly(SD);
    TC.computeAccessLevel(SD);

    checkUnsupportedNestedType(SD);

    TC.validateDecl(SD);
    TC.DeclsToFinalize.remove(SD);
    SD->setHasValidatedLayout();

    TC.addImplicitConstructors(SD);

    for (Decl *Member : SD->getMembers())
      visit(Member);

    TC.checkDeclAttributes(SD);
    checkAccessControl(TC, SD);

    TC.checkDeclCircularity(SD);
    TC.ConformanceContexts.push_back(SD);
  }

  /// Check whether the given properties can be @NSManaged in this class.
  static bool propertiesCanBeNSManaged(ClassDecl *classDecl,
                                       ArrayRef<VarDecl *> vars) {
    // Check whether we have an Objective-C-defined class in our
    // inheritance chain.
    while (classDecl) {
      // If we found an Objective-C-defined class, continue checking.
      if (classDecl->hasClangNode())
        break;

      // If we ran out of superclasses, we're done.
      if (!classDecl->hasSuperclass())
        return false;

      classDecl = classDecl->getSuperclass()->getClassOrBoundGenericClass();
    }

    // If all of the variables are @objc, we can use @NSManaged.
    for (auto var : vars) {
      if (!var->isObjC())
        return false;
    }

    // Okay, we can use @NSManaged.
    return true;
  }

  /// Check that all stored properties have in-class initializers.
  void checkRequiredInClassInits(ClassDecl *cd) {
    ClassDecl *source = nullptr;
    for (auto member : cd->getMembers()) {
      auto pbd = dyn_cast<PatternBindingDecl>(member);
      if (!pbd)
        continue;

      if (pbd->isStatic() || !pbd->hasStorage() || 
          pbd->isDefaultInitializable() || pbd->isInvalid())
        continue;

      // The variables in this pattern have not been
      // initialized. Diagnose the lack of initial value.
      pbd->setInvalid();
      SmallVector<VarDecl *, 4> vars;
      for (auto entry : pbd->getPatternList())
        entry.getPattern()->collectVariables(vars);
      bool suggestNSManaged = propertiesCanBeNSManaged(cd, vars);
      switch (vars.size()) {
      case 0:
        llvm_unreachable("should have been marked invalid");

      case 1:
        TC.diagnose(pbd->getLoc(), diag::missing_in_class_init_1,
                    vars[0]->getName(), suggestNSManaged);
        break;

      case 2:
        TC.diagnose(pbd->getLoc(), diag::missing_in_class_init_2,
                    vars[0]->getName(), vars[1]->getName(), suggestNSManaged);
        break;

      case 3:
        TC.diagnose(pbd->getLoc(), diag::missing_in_class_init_3plus,
                    vars[0]->getName(), vars[1]->getName(), vars[2]->getName(),
                    false, suggestNSManaged);
        break;

      default:
        TC.diagnose(pbd->getLoc(), diag::missing_in_class_init_3plus,
                    vars[0]->getName(), vars[1]->getName(), vars[2]->getName(),
                    true, suggestNSManaged);
        break;
      }

      // Figure out where this requirement came from.
      if (!source) {
        source = cd;
        while (true) {
          // If this class had the 'requires_stored_property_inits'
          // attribute, diagnose here.
          if (source->getAttrs().
                hasAttribute<RequiresStoredPropertyInitsAttr>())
            break;

          // If the superclass doesn't require in-class initial
          // values, the requirement was introduced at this point, so
          // stop here.
          auto superclass = cast<ClassDecl>(
                              source->getSuperclass()->getAnyNominal());
          if (!superclass->requiresStoredPropertyInits())
            break;

          // Keep looking.
          source = superclass;
        }
      }

      // Add a note describing why we need an initializer.
      TC.diagnose(source, diag::requires_stored_property_inits_here,
                  source->getDeclaredType(), cd == source, suggestNSManaged);
    }
  }


  void visitClassDecl(ClassDecl *CD) {
    TC.checkDeclAttributesEarly(CD);
    TC.computeAccessLevel(CD);

    checkUnsupportedNestedType(CD);

    TC.validateDecl(CD);
    TC.requestSuperclassLayout(CD);
    TC.DeclsToFinalize.remove(CD);
    CD->setHasValidatedLayout();

    {
      // Check for circular inheritance.
      SmallVector<ClassDecl *, 8> path;
      path.push_back(CD);
      checkCircularity(TC, CD, diag::circular_class_inheritance,
                       diag::class_here, path);
    }

    for (Decl *Member : CD->getMembers())
      visit(Member);

    // If this class requires all of its stored properties to have
    // in-class initializers, diagnose this now.
    if (CD->requiresStoredPropertyInits())
      checkRequiredInClassInits(CD);

    TC.addImplicitConstructors(CD);
    CD->addImplicitDestructor();

    if (auto superclassTy = CD->getSuperclass()) {
      ClassDecl *Super = superclassTy->getClassOrBoundGenericClass();

      if (auto *SF = CD->getParentSourceFile()) {
        if (auto *tracker = SF->getReferencedNameTracker()) {
          bool isPrivate =
              CD->getFormalAccess() <= AccessLevel::FilePrivate;
          tracker->addUsedMember({Super, Identifier()}, !isPrivate);
        }
      }

      bool isInvalidSuperclass = false;

      if (Super->isFinal()) {
        TC.diagnose(CD, diag::inheritance_from_final_class,
                    Super->getName());
        // FIXME: should this really be skipping the rest of decl-checking?
        return;
      }

      if (Super->hasClangNode() && Super->getGenericParams()
          && superclassTy->hasTypeParameter()) {
        TC.diagnose(CD,
                    diag::inheritance_from_unspecialized_objc_generic_class,
                    Super->getName());
      }

      switch (Super->getForeignClassKind()) {
      case ClassDecl::ForeignKind::Normal:
        break;
      case ClassDecl::ForeignKind::CFType:
        TC.diagnose(CD, diag::inheritance_from_cf_class,
                    Super->getName());
        isInvalidSuperclass = true;
        break;
      case ClassDecl::ForeignKind::RuntimeOnly:
        TC.diagnose(CD, diag::inheritance_from_objc_runtime_visible_class,
                    Super->getName());
        isInvalidSuperclass = true;
        break;
      }

      if (!isInvalidSuperclass && Super->hasMissingVTableEntries()) {
        auto *superFile = Super->getModuleScopeContext();
        if (auto *serialized = dyn_cast<SerializedASTFile>(superFile)) {
          if (serialized->getLanguageVersionBuiltWith() !=
              TC.getLangOpts().EffectiveLanguageVersion) {
            TC.diagnose(CD,
                        diag::inheritance_from_class_with_missing_vtable_entries_versioned,
                        Super->getName(),
                        serialized->getLanguageVersionBuiltWith(),
                        TC.getLangOpts().EffectiveLanguageVersion);
            isInvalidSuperclass = true;
          }
        }
        if (!isInvalidSuperclass) {
          TC.diagnose(
              CD, diag::inheritance_from_class_with_missing_vtable_entries,
              Super->getName());
          isInvalidSuperclass = true;
        }
      }

      // Require the superclass to be open if this is outside its
      // defining module.  But don't emit another diagnostic if we
      // already complained about the class being inherently
      // un-subclassable.
      if (!isInvalidSuperclass &&
          Super->getFormalAccess(CD->getDeclContext())
            < AccessLevel::Open &&
          Super->getModuleContext() != CD->getModuleContext()) {
        TC.diagnose(CD, diag::superclass_not_open, superclassTy);
        isInvalidSuperclass = true;
      }

      // Require superclasses to be open if the subclass is open.
      // This is a restriction we can consider lifting in the future,
      // e.g. to enable a "sealed" superclass whose subclasses are all
      // of one of several alternatives.
      if (!isInvalidSuperclass &&
          CD->getFormalAccess() == AccessLevel::Open &&
          Super->getFormalAccess() != AccessLevel::Open) {
        TC.diagnose(CD, diag::superclass_of_open_not_open, superclassTy);
        TC.diagnose(Super, diag::superclass_here);
      }

    }

    TC.checkDeclAttributes(CD);
    checkAccessControl(TC, CD);

    TC.checkDeclCircularity(CD);
    TC.ConformanceContexts.push_back(CD);
  }

  void visitProtocolDecl(ProtocolDecl *PD) {
    TC.checkDeclAttributesEarly(PD);
    TC.computeAccessLevel(PD);

    checkUnsupportedNestedType(PD);

    TC.validateDecl(PD);
    if (!PD->hasValidSignature())
      return;

    {
      // Check for circular inheritance within the protocol.
      SmallVector<ProtocolDecl *, 8> path;
      path.push_back(PD);
      checkCircularity(TC, PD, diag::circular_protocol_def,
                       diag::protocol_here, path);

      // Make sure the parent protocols have been fully validated.
      for (auto inherited : PD->getLocalProtocols()) {
        TC.validateDecl(inherited);
        for (auto *member : inherited->getMembers())
          if (auto *requirement = dyn_cast<ValueDecl>(member))
            TC.validateDecl(requirement);
      }

      if (auto *SF = PD->getParentSourceFile()) {
        if (auto *tracker = SF->getReferencedNameTracker()) {
          bool isNonPrivate =
              (PD->getFormalAccess() > AccessLevel::FilePrivate);
          for (auto *parentProto : PD->getInheritedProtocols())
            tracker->addUsedMember({parentProto, Identifier()}, isNonPrivate);
        }
      }
    }

    // Check the members.
    for (auto Member : PD->getMembers())
      visit(Member);

    TC.checkDeclAttributes(PD);

    checkAccessControl(TC, PD);
    TC.checkInheritanceClause(PD);

    GenericTypeToArchetypeResolver resolver(PD);
    TC.validateWhereClauses(PD, &resolver);

    TC.checkDeclCircularity(PD);
    if (PD->isResilient())
      TC.inferDefaultWitnesses(PD);

    if (TC.Context.LangOpts.DebugGenericSignatures) {
      auto requirementsSig =
        GenericSignature::get({PD->getProtocolSelfType()},
                              PD->getRequirementSignature());

      llvm::errs() << "Protocol requirement signature:\n";
      PD->dumpRef(llvm::errs());
      llvm::errs() << "\n";
      llvm::errs() << "Requirement signature: ";
      requirementsSig->print(llvm::errs());
      llvm::errs() << "\n";

      // Note: One cannot canonicalize a requirement signature, because
      // requirement signatures are necessarily missing requirements.
      llvm::errs() << "Canonical requirement signature: ";
      auto canRequirementSig =
        GenericSignature::getCanonical(requirementsSig->getGenericParams(),
                                       requirementsSig->getRequirements(),
                                       /*skipValidation=*/true);
      canRequirementSig->print(llvm::errs());
      llvm::errs() << "\n";
    }
  }

  void visitVarDecl(VarDecl *VD) {
    // Delay type-checking on VarDecls until we see the corresponding
    // PatternBindingDecl.
  }

  /// Determine whether the given declaration requires a definition.
  ///
  /// Only valid for declarations that can have definitions, i.e.,
  /// functions, initializers, etc.
  static bool requiresDefinition(Decl *decl) {
    // Invalid, implicit, and Clang-imported declarations never
    // require a definition.
    if (decl->isInvalid() || decl->isImplicit() || decl->hasClangNode())
      return false;

    // Protocol requirements do not require definitions.
    if (isa<ProtocolDecl>(decl->getDeclContext()))
      return false;

    // Functions can have _silgen_name, semantics, and NSManaged attributes.
    if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
      if (func->getAttrs().hasAttribute<SILGenNameAttr>() ||
          func->getAttrs().hasAttribute<SemanticsAttr>() ||
          func->getAttrs().hasAttribute<NSManagedAttr>())
        return false;
    }

    // Declarations in SIL don't require definitions.
    if (auto sourceFile = decl->getDeclContext()->getParentSourceFile()) {
      if (sourceFile->Kind == SourceFileKind::SIL)
        return false;
    }

    // Everything else requires a definition.
    return true;
  }

  void visitFuncDecl(FuncDecl *FD) {
    TC.validateDecl(FD);
    checkAccessControl(TC, FD);

    if (FD->hasBody()) {
      // Record the body.
      TC.definedFunctions.push_back(FD);
    } else if (requiresDefinition(FD)) {
      // Complain if we should have a body.
      TC.diagnose(FD->getLoc(), diag::func_decl_without_brace);
    }
  }

  void visitModuleDecl(ModuleDecl *) { }

  /// Perform basic checking to determine whether a declaration can override a
  /// declaration in a superclass.
  static bool areOverrideCompatibleSimple(ValueDecl *decl,
                                          ValueDecl *parentDecl) {
    // If the number of argument labels does not match, these overrides cannot
    // be compatible.
    if (decl->getFullName().getArgumentNames().size() !=
          parentDecl->getFullName().getArgumentNames().size())
      return false;

    if (auto func = dyn_cast<FuncDecl>(decl)) {
      // Specific checking for methods.
      auto parentFunc = cast<FuncDecl>(parentDecl);
      if (func->isStatic() != parentFunc->isStatic())
        return false;
      if (func->isGeneric() != parentFunc->isGeneric())
        return false;
    } else if (auto ctor = dyn_cast<ConstructorDecl>(decl)) {
      auto parentCtor = cast<ConstructorDecl>(parentDecl);
      if (ctor->isGeneric() != parentCtor->isGeneric())
        return false;
    } else if (auto var = dyn_cast<VarDecl>(decl)) {
      auto parentVar = cast<VarDecl>(parentDecl);
      if (var->isStatic() != parentVar->isStatic())
        return false;
    } else if (auto subscript = dyn_cast<SubscriptDecl>(decl)) {
      auto parentSubscript = cast<SubscriptDecl>(parentDecl);
      if (subscript->isGeneric() != parentSubscript->isGeneric())
        return false;
    }

    return true;
  }

  /// Drop the optionality of the result type of the given function type.
  static Type dropResultOptionality(Type type, unsigned uncurryLevel) {
    // We've hit the result type.
    if (uncurryLevel == 0) {
      if (auto objectTy = type->getOptionalObjectType())
        return objectTy;

      return type;
    }

    // Determine the input and result types of this function.
    auto fnType = type->castTo<AnyFunctionType>();
    Type inputType = fnType->getInput();
    Type resultType = dropResultOptionality(fnType->getResult(),
                                            uncurryLevel - 1);

    // Produce the resulting function type.
    if (auto genericFn = dyn_cast<GenericFunctionType>(fnType)) {
      return GenericFunctionType::get(genericFn->getGenericSignature(),
                                      inputType, resultType,
                                      fnType->getExtInfo());
    }

    return FunctionType::get(inputType, resultType, fnType->getExtInfo());
  }

  static bool
  diagnoseMismatchedOptionals(TypeChecker &TC, const ValueDecl *member,
                              const ParameterList *params, TypeLoc resultTL,
                              const ValueDecl *parentMember,
                              const ParameterList *parentParams, Type owningTy,
                              bool treatIUOResultAsError) {
    bool emittedError = false;
    Type plainParentTy = owningTy->adjustSuperclassMemberDeclType(
        parentMember, member, parentMember->getInterfaceType());
    const auto *parentTy = plainParentTy->castTo<FunctionType>();
    if (isa<AbstractFunctionDecl>(parentMember))
      parentTy = parentTy->getResult()->castTo<FunctionType>();

    // Check the parameter types.
    auto checkParam = [&](const ParamDecl *decl, const ParamDecl *parentDecl) {
      Type paramTy = decl->getType();
      Type parentParamTy = parentDecl->getType();

      if (!paramTy || !parentParamTy)
        return;

      TypeLoc TL = decl->getTypeLoc();
      if (!TL.getTypeRepr())
        return;

      bool paramIsOptional =  (bool) paramTy->getOptionalObjectType();
      bool parentIsOptional = (bool) parentParamTy->getOptionalObjectType();

      if (paramIsOptional == parentIsOptional)
        return;

      if (!paramIsOptional) {
        if (parentDecl->getAttrs()
                .hasAttribute<ImplicitlyUnwrappedOptionalAttr>())
          if (!treatIUOResultAsError)
            return;

        emittedError = true;
        auto diag = TC.diagnose(decl->getStartLoc(),
                                diag::override_optional_mismatch,
                                member->getDescriptiveKind(),
                                isa<SubscriptDecl>(member),
                                parentParamTy, paramTy);
        if (TL.getTypeRepr()->isSimple()) {
          diag.fixItInsertAfter(TL.getSourceRange().End, "?");
        } else {
          diag.fixItInsert(TL.getSourceRange().Start, "(");
          diag.fixItInsertAfter(TL.getSourceRange().End, ")?");
        }
        return;
      }

      if (!decl->getAttrs().hasAttribute<ImplicitlyUnwrappedOptionalAttr>())
        return;

      // Allow silencing this warning using parens.
      if (TL.getType()->hasParenSugar())
        return;

      TC.diagnose(decl->getStartLoc(), diag::override_unnecessary_IUO,
                  member->getDescriptiveKind(), parentParamTy, paramTy)
        .highlight(TL.getSourceRange());

      auto sugaredForm =
        dyn_cast<ImplicitlyUnwrappedOptionalTypeRepr>(TL.getTypeRepr());
      if (sugaredForm) {
        TC.diagnose(sugaredForm->getExclamationLoc(),
                    diag::override_unnecessary_IUO_remove)
          .fixItRemove(sugaredForm->getExclamationLoc());
      }

      TC.diagnose(TL.getSourceRange().Start,
                  diag::override_unnecessary_IUO_silence)
        .fixItInsert(TL.getSourceRange().Start, "(")
        .fixItInsertAfter(TL.getSourceRange().End, ")");
    };

    // FIXME: If we ever allow argument reordering, this is incorrect.
    ArrayRef<ParamDecl *> sharedParams = params->getArray();
    ArrayRef<ParamDecl *> sharedParentParams = parentParams->getArray();
    assert(sharedParams.size() == sharedParentParams.size());
    for_each(sharedParams, sharedParentParams, checkParam);

    if (!resultTL.getTypeRepr())
      return emittedError;

    auto checkResult = [&](TypeLoc resultTL, Type parentResultTy) {
      Type resultTy = resultTL.getType();
      if (!resultTy || !parentResultTy)
        return;

      if (!resultTy->getOptionalObjectType())
        return;

      TypeRepr *TR = resultTL.getTypeRepr();

      bool resultIsPlainOptional = true;
      if (member->getAttrs().hasAttribute<ImplicitlyUnwrappedOptionalAttr>())
        resultIsPlainOptional = false;

      if (resultIsPlainOptional || treatIUOResultAsError) {
        if (parentResultTy->getOptionalObjectType())
          return;
        emittedError = true;
        auto diag = TC.diagnose(resultTL.getSourceRange().Start,
                                diag::override_optional_result_mismatch,
                                member->getDescriptiveKind(),
                                isa<SubscriptDecl>(member),
                                parentResultTy, resultTy);
        if (auto optForm = dyn_cast<OptionalTypeRepr>(TR)) {
          diag.fixItRemove(optForm->getQuestionLoc());
        } else if (auto iuoForm =
            dyn_cast<ImplicitlyUnwrappedOptionalTypeRepr>(TR)) {
          diag.fixItRemove(iuoForm->getExclamationLoc());
        }
        return;
      }

      if (!parentResultTy->getOptionalObjectType())
        return;

      // Allow silencing this warning using parens.
      if (resultTy->hasParenSugar())
        return;

      TC.diagnose(resultTL.getSourceRange().Start,
                  diag::override_unnecessary_result_IUO,
                  member->getDescriptiveKind(), parentResultTy, resultTy)
        .highlight(resultTL.getSourceRange());

      auto sugaredForm = dyn_cast<ImplicitlyUnwrappedOptionalTypeRepr>(TR);
      if (sugaredForm) {
        TC.diagnose(sugaredForm->getExclamationLoc(),
                    diag::override_unnecessary_IUO_use_strict)
          .fixItReplace(sugaredForm->getExclamationLoc(), "?");
      }

      TC.diagnose(resultTL.getSourceRange().Start,
                  diag::override_unnecessary_IUO_silence)
        .fixItInsert(resultTL.getSourceRange().Start, "(")
        .fixItInsertAfter(resultTL.getSourceRange().End, ")");
    };

    checkResult(resultTL, parentTy->getResult());
    return emittedError;
  }

  /// Make sure that there is an invalid 'override' attribute on the
  /// given declaration.
  static void makeInvalidOverrideAttr(TypeChecker &TC, ValueDecl *decl) {
    if (auto overrideAttr = decl->getAttrs().getAttribute<OverrideAttr>()) {
      overrideAttr->setInvalid();
    } else {
      auto attr = new (TC.Context) OverrideAttr(true);
      decl->getAttrs().add(attr);
      attr->setInvalid();
    }

    if (auto storage = dyn_cast<AbstractStorageDecl>(decl)) {
      if (auto getter = storage->getGetter())
        makeInvalidOverrideAttr(TC, getter);
      if (auto setter = storage->getSetter())
        makeInvalidOverrideAttr(TC, setter);
    }
  }

  static void adjustFunctionTypeForOverride(Type &type) {
    // Drop 'throws'.
    // FIXME: Do we want to allow overriding a function returning a value
    // with one returning Never?
    auto fnType = type->castTo<AnyFunctionType>();
    auto extInfo = fnType->getExtInfo();
    extInfo = extInfo.withThrows(false);
    if (fnType->getExtInfo() != extInfo)
      type = fnType->withExtInfo(extInfo);
  }

  /// If the difference between the types of \p decl and \p base is something
  /// we feel confident about fixing (even partially), emit a note with fix-its
  /// attached. Otherwise, no note will be emitted.
  ///
  /// \returns true iff a diagnostic was emitted.
  static bool noteFixableMismatchedTypes(TypeChecker &TC, ValueDecl *decl,
                                         const ValueDecl *base) {
    DiagnosticTransaction tentativeDiags(TC.Diags);

    {
      Type baseTy = base->getInterfaceType();
      if (baseTy->hasError())
        return false;

      Optional<InFlightDiagnostic> activeDiag;
      if (auto *baseInit = dyn_cast<ConstructorDecl>(base)) {
        // Special-case initializers, whose "type" isn't useful besides the
        // input arguments.
        baseTy = baseTy->getAs<AnyFunctionType>()->getResult();
        Type argTy = baseTy->getAs<AnyFunctionType>()->getInput();
        auto diagKind = diag::override_type_mismatch_with_fixits_init;
        unsigned numArgs = baseInit->getParameters()->size();
        activeDiag.emplace(TC.diagnose(decl, diagKind,
                                       /*plural*/std::min(numArgs, 2U),
                                       argTy));
      } else {
        if (isa<AbstractFunctionDecl>(base))
          baseTy = baseTy->getAs<AnyFunctionType>()->getResult();

        activeDiag.emplace(TC.diagnose(decl,
                                       diag::override_type_mismatch_with_fixits,
                                       base->getDescriptiveKind(), baseTy));
      }

      if (fixItOverrideDeclarationTypes(*activeDiag, decl, base))
        return true;
    }

    // There weren't any fixes we knew how to make. Drop this diagnostic.
    tentativeDiags.abort();
    return false;
  }

  enum class OverrideCheckingAttempt {
    PerfectMatch,
    MismatchedOptional,
    MismatchedTypes,
    BaseName,
    BaseNameWithMismatchedOptional,
    Final
  };

  friend OverrideCheckingAttempt &operator++(OverrideCheckingAttempt &attempt) {
    assert(attempt != OverrideCheckingAttempt::Final);
    attempt = static_cast<OverrideCheckingAttempt>(1+static_cast<int>(attempt));
    return attempt;
  }

  struct OverrideMatch {
    ValueDecl *Decl;
    bool IsExact;
    Type SubstType;
  };

  static void diagnoseGeneralOverrideFailure(TypeChecker &TC,
                                             ValueDecl *decl,
                                             ArrayRef<OverrideMatch> matches,
                                             OverrideCheckingAttempt attempt) {
    switch (attempt) {
    case OverrideCheckingAttempt::PerfectMatch:
      TC.diagnose(decl, diag::override_multiple_decls_base,
                  decl->getFullName());
      break;
    case OverrideCheckingAttempt::BaseName:
      TC.diagnose(decl, diag::override_multiple_decls_arg_mismatch,
                  decl->getFullName());
      break;
    case OverrideCheckingAttempt::MismatchedOptional:
    case OverrideCheckingAttempt::MismatchedTypes:
    case OverrideCheckingAttempt::BaseNameWithMismatchedOptional:
      if (isa<ConstructorDecl>(decl))
        TC.diagnose(decl, diag::initializer_does_not_override);
      else if (isa<SubscriptDecl>(decl))
        TC.diagnose(decl, diag::subscript_does_not_override);
      else if (isa<VarDecl>(decl))
        TC.diagnose(decl, diag::property_does_not_override);
      else
        TC.diagnose(decl, diag::method_does_not_override);
      break;
    case OverrideCheckingAttempt::Final:
      llvm_unreachable("should have exited already");
    }

    for (auto match : matches) {
      auto matchDecl = match.Decl;
      if (attempt == OverrideCheckingAttempt::PerfectMatch) {
        TC.diagnose(matchDecl, diag::overridden_here);
        continue;
      }

      auto diag = TC.diagnose(matchDecl, diag::overridden_near_match_here,
                              matchDecl->getDescriptiveKind(),
                              matchDecl->getFullName());
      if (attempt == OverrideCheckingAttempt::BaseName) {
        fixDeclarationName(diag, cast<AbstractFunctionDecl>(decl),
                           matchDecl->getFullName());
      }
    }
  }

  static bool parameterTypesMatch(const ValueDecl *derivedDecl,
                                  const ValueDecl *baseDecl,
                                  TypeMatchOptions matchMode) {
    const ParameterList *derivedParams;
    const ParameterList *baseParams;
    if (auto *derived = dyn_cast<AbstractFunctionDecl>(derivedDecl)) {
      auto *base = dyn_cast<AbstractFunctionDecl>(baseDecl);
      if (!base)
        return false;
      baseParams = base->getParameterList(1);
      derivedParams = derived->getParameterList(1);
    } else {
      auto *base = dyn_cast<SubscriptDecl>(baseDecl);
      if (!base)
        return false;
      baseParams = base->getIndices();
      derivedParams = cast<SubscriptDecl>(derivedDecl)->getIndices();
    }

    if (baseParams->size() != derivedParams->size())
      return false;

    auto subs = SubstitutionMap::getOverrideSubstitutions(baseDecl, derivedDecl,
                                                          /*derivedSubs=*/None);

    for (auto i : indices(baseParams->getArray())) {
      auto baseItfTy = baseParams->get(i)->getInterfaceType();
      auto baseParamTy =
          baseDecl->getAsGenericContext()->mapTypeIntoContext(baseItfTy);
      baseParamTy = baseParamTy.subst(subs);
      auto derivedParamTy = derivedParams->get(i)->getInterfaceType();

      // Attempt contravariant match.
      if (baseParamTy->matchesParameter(derivedParamTy, matchMode))
        continue;

      // Try once more for a match, using the underlying type of an
      // IUO if we're allowing that.
      if (baseParams->get(i)
              ->getAttrs()
              .hasAttribute<ImplicitlyUnwrappedOptionalAttr>() &&
          matchMode.contains(TypeMatchFlags::AllowNonOptionalForIUOParam)) {
        baseParamTy = baseParamTy->getOptionalObjectType();
        if (baseParamTy->matches(derivedParamTy, matchMode))
          continue;
      }

      // If there is no match, then we're done.
      return false;
    }

    return true;
  }

  /// Determine which method or subscript this method or subscript overrides
  /// (if any).
  ///
  /// \returns true if an error occurred.
  static bool checkOverrides(TypeChecker &TC, ValueDecl *decl) {
    if (decl->isInvalid() || decl->getOverriddenDecl())
      return false;

    auto *dc = decl->getDeclContext();

    auto owningTy = dc->getDeclaredInterfaceType();
    if (!owningTy)
      return false;

    auto classDecl = owningTy->getClassOrBoundGenericClass();
    if (!classDecl)
      return false;

    Type superclass = classDecl->getSuperclass();
    if (!superclass)
      return false;

    // Ignore accessor methods (e.g. getters and setters), they will be handled
    // when their storage decl is processed.
    if (isa<AccessorDecl>(decl))
      return false;
    
    auto method = dyn_cast<AbstractFunctionDecl>(decl);
    ConstructorDecl *ctor = nullptr;
    if (method)
      ctor = dyn_cast<ConstructorDecl>(method);

    auto abstractStorage = dyn_cast<AbstractStorageDecl>(decl);
    assert((method || abstractStorage) && "Not a method or abstractStorage?");
    SubscriptDecl *subscript = nullptr;
    if (abstractStorage)
      subscript = dyn_cast<SubscriptDecl>(abstractStorage);

    // Figure out the type of the declaration that we're using for comparisons.
    auto declTy = decl->getInterfaceType()->getUnlabeledType(TC.Context);
    if (method) {
      // For methods, strip off the 'Self' type.
      declTy = declTy->castTo<AnyFunctionType>()->getResult();
      adjustFunctionTypeForOverride(declTy);
    } if (subscript) {
      // For subscripts, we don't have a 'Self' type, but turn it
      // into a monomorphic function type.
      auto funcTy = declTy->castTo<AnyFunctionType>();
      declTy = FunctionType::get(funcTy->getInput(),
                                 funcTy->getResult());
    } else {
      // For properties, strip off ownership.
      declTy = declTy->getReferenceStorageReferent();
    }

    // Ignore the optionality of initializers when comparing types;
    // we'll enforce this separately
    if (ctor) {
      declTy = dropResultOptionality(declTy, 1);
    }

    // Look for members with the same name and matching types as this
    // one.
    auto attempt = OverrideCheckingAttempt::PerfectMatch;
    SmallVector<OverrideMatch, 2> matches;
    DeclName name = decl->getFullName();
    bool hadExactMatch = false;
    LookupResult members;

    do {
      switch (attempt) {
      case OverrideCheckingAttempt::PerfectMatch:
        break;
      case OverrideCheckingAttempt::MismatchedOptional:
        // Don't keep looking if the user didn't indicate it's an override.
        if (!decl->getAttrs().hasAttribute<OverrideAttr>())
          return false;
        break;
      case OverrideCheckingAttempt::MismatchedTypes:
        break;
      case OverrideCheckingAttempt::BaseName:
        // Don't keep looking if this is already a simple name, or if there
        // are no arguments.
        if (name.isSimpleName() || name.getArgumentNames().empty())
          return false;
        name = name.getBaseName();
        members.clear();
        break;
      case OverrideCheckingAttempt::BaseNameWithMismatchedOptional:
        break;
      case OverrideCheckingAttempt::Final:
        // Give up.
        return false;
      }

      if (members.empty()) {
        auto lookupOptions = defaultMemberLookupOptions;

        // Class methods cannot override declarations only
        // visible via dynamic dispatch.
        lookupOptions -= NameLookupFlags::DynamicLookup;

        // Class methods cannot override declarations only
        // visible as protocol requirements or protocol
        // extension members.
        lookupOptions -= NameLookupFlags::ProtocolMembers;
        lookupOptions -= NameLookupFlags::PerformConformanceCheck;

        members = TC.lookupMember(dc, superclass,
                                  name, lookupOptions);
      }

      for (auto memberResult : members) {
        auto member = memberResult.getValueDecl();

        if (member->isInvalid())
          continue;

        if (member->getKind() != decl->getKind())
          continue;

        if (!dc->getAsClassOrClassExtensionContext())
          continue;

        auto parentDecl = cast<ValueDecl>(member);

        // Check whether there are any obvious reasons why the two given
        // declarations do not have an overriding relationship.
        if (!areOverrideCompatibleSimple(decl, parentDecl))
          continue;

        auto parentMethod = dyn_cast<AbstractFunctionDecl>(parentDecl);
        auto parentStorage = dyn_cast<AbstractStorageDecl>(parentDecl);
        assert(parentMethod || parentStorage);

        // If both are Objective-C, then match based on selectors or
        // subscript kind and check the types separately.
        bool objCMatch = false;
        if (parentDecl->isObjC() && decl->isObjC()) {
          if (method) {
            if (method->getObjCSelector() == parentMethod->getObjCSelector())
              objCMatch = true;
          } else if (auto *parentSubscript =
                       dyn_cast<SubscriptDecl>(parentStorage)) {
            // If the subscript kinds don't match, it's not an override.
            if (subscript->getObjCSubscriptKind()
                  == parentSubscript->getObjCSubscriptKind())
              objCMatch = true;
          }

          // Properties don't need anything here since they are always
          // checked by name.
        }

        // Check whether the types are identical.
        auto parentDeclTy = owningTy->adjustSuperclassMemberDeclType(
            parentDecl, decl, parentDecl->getInterfaceType());
        if (parentDeclTy->hasError()) continue;
        parentDeclTy = parentDeclTy->getUnlabeledType(TC.Context);
        if (method) {
          // For methods, strip off the 'Self' type.
          parentDeclTy = parentDeclTy->castTo<FunctionType>()->getResult();
          adjustFunctionTypeForOverride(parentDeclTy);
        } else {
          // For properties, strip off ownership.
          parentDeclTy = parentDeclTy->getReferenceStorageReferent();
        }

        // Ignore the optionality of initializers when comparing types;
        // we'll enforce this separately
        if (ctor) {
          parentDeclTy = dropResultOptionality(parentDeclTy, 1);

          // Factory methods cannot be overridden.
          auto parentCtor = cast<ConstructorDecl>(parentDecl);
          if (parentCtor->isFactoryInit())
            continue;
        }

        // Canonicalize with respect to the override's generic signature, if any.
        auto *genericSig = decl->getInnermostDeclContext()
          ->getGenericSignatureOfContext();

        auto canDeclTy = declTy->getCanonicalType(genericSig);
        auto canParentDeclTy = parentDeclTy->getCanonicalType(genericSig);

        auto declIUOAttr =
            decl->getAttrs().hasAttribute<ImplicitlyUnwrappedOptionalAttr>();
        auto parentDeclIUOAttr =
            parentDecl->getAttrs()
                .hasAttribute<ImplicitlyUnwrappedOptionalAttr>();

        if (declIUOAttr == parentDeclIUOAttr && canDeclTy == canParentDeclTy) {
          matches.push_back({parentDecl, true, parentDeclTy});
          hadExactMatch = true;
          continue;
        }
        
        // If this is a property, we accept the match and then reject it below
        // if the types don't line up, since you can't overload properties based
        // on types.
        if (isa<VarDecl>(parentDecl) ||
            attempt == OverrideCheckingAttempt::MismatchedTypes) {
          matches.push_back({parentDecl, false, parentDeclTy});
          continue;
        }

        // Failing that, check for subtyping.
        TypeMatchOptions matchMode = TypeMatchFlags::AllowOverride;
        if (attempt == OverrideCheckingAttempt::MismatchedOptional ||
            attempt == OverrideCheckingAttempt::BaseNameWithMismatchedOptional){
          matchMode |= TypeMatchFlags::AllowTopLevelOptionalMismatch;
        } else if (parentDecl->isObjC()) {
          matchMode |= TypeMatchFlags::AllowNonOptionalForIUOParam;
          matchMode |=
              TypeMatchFlags::IgnoreNonEscapingForOptionalFunctionParam;
        }

        auto declFnTy = declTy->getAs<AnyFunctionType>();
        auto parentDeclFnTy = parentDeclTy->getAs<AnyFunctionType>();
        if (declFnTy && parentDeclFnTy) {
          auto paramsAndResultMatch = [=]() -> bool {
            return parameterTypesMatch(decl, parentDecl, matchMode) &&
                   declFnTy->getResult()->matches(parentDeclFnTy->getResult(),
                                                  matchMode);
          };

          if (declFnTy->matchesFunctionType(parentDeclFnTy, matchMode,
                                            paramsAndResultMatch)) {
            matches.push_back({parentDecl, objCMatch, parentDeclTy});
            hadExactMatch |= objCMatch;
            continue;
          }
        } else if (declTy->matches(parentDeclTy, matchMode)) {
          matches.push_back({parentDecl, objCMatch, parentDeclTy});
          hadExactMatch |= objCMatch;
          continue;
        }

        // Not a match. If we had an Objective-C match, this is a serious
        // problem.
        if (objCMatch) {
          if (method) {
            TC.diagnose(decl, diag::override_objc_type_mismatch_method,
                        method->getObjCSelector(), declTy);
          } else {
            TC.diagnose(decl, diag::override_objc_type_mismatch_subscript,
                        static_cast<unsigned>(
                          subscript->getObjCSubscriptKind()),
                        declTy);
          }
          TC.diagnose(parentDecl, diag::overridden_here_with_type,
                      parentDeclTy);
          
          // Put an invalid 'override' attribute here.
          makeInvalidOverrideAttr(TC, decl);

          return true;
        }
      }
      if (!matches.empty())
        break;

      ++attempt;
    } while (true);

    assert(!matches.empty());

    // If we had an exact match, throw away any non-exact matches.
    if (hadExactMatch)
      matches.erase(std::remove_if(matches.begin(), matches.end(),
                                   [&](OverrideMatch &match) {
                                     return !match.IsExact;
                                   }), matches.end());

    // If we override more than one declaration, complain.
    if (matches.size() > 1) {
      diagnoseGeneralOverrideFailure(TC, decl, matches, attempt);
      return true;
    }

    // If we have a single match (exact or not), take it.
    auto matchDecl = matches.front().Decl;
    auto matchType = matches.front().SubstType;
    bool emittedMatchError = false;

    // If the name of our match differs from the name we were looking for,
    // complain.
    if (decl->getFullName() != matchDecl->getFullName()) {
      auto diag = TC.diagnose(decl, diag::override_argument_name_mismatch,
                              isa<ConstructorDecl>(decl),
                              decl->getFullName(),
                              matchDecl->getFullName());
      fixDeclarationName(diag, cast<AbstractFunctionDecl>(decl),
                         matchDecl->getFullName());
      emittedMatchError = true;
    }

    // If we have an explicit ownership modifier and our parent doesn't,
    // complain.
    auto parentAttr =
        matchDecl->getAttrs().getAttribute<ReferenceOwnershipAttr>();
    if (auto ownershipAttr =
            decl->getAttrs().getAttribute<ReferenceOwnershipAttr>()) {
      ReferenceOwnership parentOwnership;
      if (parentAttr)
        parentOwnership = parentAttr->get();
      else
        parentOwnership = ReferenceOwnership::Strong;
      if (parentOwnership != ownershipAttr->get()) {
        TC.diagnose(decl, diag::override_ownership_mismatch,
                    parentOwnership, ownershipAttr->get());
        TC.diagnose(matchDecl, diag::overridden_here);
      }
    }

    // If a super method returns Self, and the subclass overrides it to
    // instead return the subclass type, complain.
    // This case gets this far because the type matching above specifically
    // strips out dynamic self via replaceCovariantResultType(), and that
    // is helpful in several cases - just not this one.
    if (decl->getASTContext().isSwiftVersionAtLeast(5) &&
        matchDecl->getInterfaceType()->hasDynamicSelfType() &&
        !decl->getInterfaceType()->hasDynamicSelfType() &&
        !classDecl->isFinal()) {
      TC.diagnose(decl, diag::override_dynamic_self_mismatch);
      TC.diagnose(matchDecl, diag::overridden_here);
    }

    // Check that the override has the required access level.
    // Overrides have to be at least as accessible as what they
    // override, except:
    //   - they don't have to be more accessible than their class and
    //   - a final method may be public instead of open.
    // Also diagnose attempts to override a non-open method from outside its
    // defining module.  This is not required for constructors, which are
    // never really "overridden" in the intended sense here, because of
    // course derived classes will change how the class is initialized.
    AccessLevel matchAccess = matchDecl->getFormalAccess(dc);
    if (matchAccess < AccessLevel::Open &&
        matchDecl->getModuleContext() != decl->getModuleContext() &&
        !isa<ConstructorDecl>(decl)) {
      TC.diagnose(decl, diag::override_of_non_open,
                  decl->getDescriptiveKind());

    } else if (matchAccess == AccessLevel::Open &&
               classDecl->getFormalAccess(dc) ==
                 AccessLevel::Open &&
               decl->getFormalAccess() != AccessLevel::Open &&
               !decl->isFinal()) {
      {
        auto diag = TC.diagnose(decl, diag::override_not_accessible,
                                /*setter*/false,
                                decl->getDescriptiveKind(),
                                /*fromOverridden*/true);
        fixItAccess(diag, decl, AccessLevel::Open);
      }
      TC.diagnose(matchDecl, diag::overridden_here);

    } else if (!isa<ConstructorDecl>(decl)) {
      auto matchAccessScope =
        matchDecl->getFormalAccessScope(dc);
      auto classAccessScope =
        classDecl->getFormalAccessScope(dc);
      auto requiredAccessScope =
        matchAccessScope.intersectWith(classAccessScope);
      auto scopeDC = requiredAccessScope->getDeclContext();

      bool shouldDiagnose = !decl->isAccessibleFrom(scopeDC);

      bool shouldDiagnoseSetter = false;
      if (!shouldDiagnose && matchDecl->isSettable(dc)){
        auto matchASD = cast<AbstractStorageDecl>(matchDecl);
        if (matchASD->isSetterAccessibleFrom(dc)) {
          auto matchSetterAccessScope = matchASD->getSetter()
            ->getFormalAccessScope(dc);
          auto requiredSetterAccessScope =
            matchSetterAccessScope.intersectWith(classAccessScope);
          auto setterScopeDC = requiredSetterAccessScope->getDeclContext();

          const auto *ASD = cast<AbstractStorageDecl>(decl);
          shouldDiagnoseSetter =
              ASD->isSettable(setterScopeDC) &&
              !ASD->isSetterAccessibleFrom(setterScopeDC);
        }
      }

      if (shouldDiagnose || shouldDiagnoseSetter) {
        bool overriddenForcesAccess =
          (requiredAccessScope->hasEqualDeclContextWith(matchAccessScope) &&
           matchAccess != AccessLevel::Open);
        AccessLevel requiredAccess =
          requiredAccessScope->requiredAccessForDiagnostics();
        {
          auto diag = TC.diagnose(decl, diag::override_not_accessible,
                                  shouldDiagnoseSetter,
                                  decl->getDescriptiveKind(),
                                  overriddenForcesAccess);
          fixItAccess(diag, decl, requiredAccess,
                             shouldDiagnoseSetter);
        }
        TC.diagnose(matchDecl, diag::overridden_here);
      }
    }

    bool mayHaveMismatchedOptionals =
        (attempt == OverrideCheckingAttempt::MismatchedOptional ||
         attempt == OverrideCheckingAttempt::BaseNameWithMismatchedOptional);

    auto declIUOAttr =
        decl->getAttrs().hasAttribute<ImplicitlyUnwrappedOptionalAttr>();
    auto matchDeclIUOAttr =
        matchDecl->getAttrs().hasAttribute<ImplicitlyUnwrappedOptionalAttr>();

    // If this is an exact type match, we're successful!
    if (declIUOAttr == matchDeclIUOAttr && declTy->isEqual(matchType)) {
      // Nothing to do.
      
    } else if (method) {
      if (attempt == OverrideCheckingAttempt::MismatchedTypes) {
        auto diagKind = diag::method_does_not_override;
        if (ctor)
          diagKind = diag::initializer_does_not_override;
        TC.diagnose(decl, diagKind);
        noteFixableMismatchedTypes(TC, decl, matchDecl);
        TC.diagnose(matchDecl, diag::overridden_near_match_here,
                    matchDecl->getDescriptiveKind(),
                    matchDecl->getFullName());
        emittedMatchError = true;

      } else if (!isa<AccessorDecl>(method) &&
                 (matchDecl->isObjC() || mayHaveMismatchedOptionals)) {
        // Private migration help for overrides of Objective-C methods.
        TypeLoc resultTL;
        if (auto *methodAsFunc = dyn_cast<FuncDecl>(method))
          resultTL = methodAsFunc->getBodyResultTypeLoc();
        emittedMatchError |= diagnoseMismatchedOptionals(
            TC, method, method->getParameterList(1), resultTL, matchDecl,
            cast<AbstractFunctionDecl>(matchDecl)->getParameterList(1),
            owningTy, mayHaveMismatchedOptionals);
      }
    } else if (auto subscript =
                 dyn_cast_or_null<SubscriptDecl>(abstractStorage)) {
      // Otherwise, if this is a subscript, validate that covariance is ok.
      // If the parent is non-mutable, it's okay to be covariant.
      auto parentSubscript = cast<SubscriptDecl>(matchDecl);
      if (parentSubscript->getSetter()) {
        TC.diagnose(subscript, diag::override_mutable_covariant_subscript,
                    declTy, matchType);
        TC.diagnose(matchDecl, diag::subscript_override_here);
        return true;
      }

      if (attempt == OverrideCheckingAttempt::MismatchedTypes) {
        TC.diagnose(decl, diag::subscript_does_not_override);
        noteFixableMismatchedTypes(TC, decl, matchDecl);
        TC.diagnose(matchDecl, diag::overridden_near_match_here,
                    matchDecl->getDescriptiveKind(),
                    matchDecl->getFullName());
        emittedMatchError = true;

      } else if (mayHaveMismatchedOptionals) {
        emittedMatchError |= diagnoseMismatchedOptionals(
            TC, subscript, subscript->getIndices(),
            subscript->getElementTypeLoc(), matchDecl,
            cast<SubscriptDecl>(matchDecl)->getIndices(), owningTy,
            mayHaveMismatchedOptionals);
      }
    } else if (auto property = dyn_cast_or_null<VarDecl>(abstractStorage)) {
      auto propertyTy = property->getInterfaceType();
      auto parentPropertyTy = superclass->adjustSuperclassMemberDeclType(
          matchDecl, decl, matchDecl->getInterfaceType());

      if (!propertyTy->matches(parentPropertyTy,
                               TypeMatchFlags::AllowOverride)) {
        TC.diagnose(property, diag::override_property_type_mismatch,
                    property->getName(), propertyTy, parentPropertyTy);
        noteFixableMismatchedTypes(TC, decl, matchDecl);
        TC.diagnose(matchDecl, diag::property_override_here);
        return true;
      }
      
      // Differing only in Optional vs. ImplicitlyUnwrappedOptional is fine.
      bool IsSilentDifference = false;
      if (auto propertyTyNoOptional = propertyTy->getOptionalObjectType())
        if (auto parentPropertyTyNoOptional =
                parentPropertyTy->getOptionalObjectType())
          if (propertyTyNoOptional->isEqual(parentPropertyTyNoOptional))
            IsSilentDifference = true;
      
      // The overridden property must not be mutable.
      if (cast<AbstractStorageDecl>(matchDecl)->getSetter() &&
          !IsSilentDifference) {
        TC.diagnose(property, diag::override_mutable_covariant_property,
                    property->getName(), parentPropertyTy, propertyTy);
        TC.diagnose(matchDecl, diag::property_override_here);
        return true;
      }
    }

    // Catch-all to make sure we don't silently accept something we shouldn't.
    if (attempt != OverrideCheckingAttempt::PerfectMatch &&
        !emittedMatchError) {
      diagnoseGeneralOverrideFailure(TC, decl, matches, attempt);
    }

    return recordOverride(TC, decl, matchDecl);
  }

  /// Attribute visitor that checks how the given attribute should be
  /// considered when overriding a declaration.
  ///
  /// Note that the attributes visited are those of the base
  /// declaration, so if you need to check that the overriding
  /// declaration doesn't have an attribute if the base doesn't have
  /// it, this isn't sufficient.
  class AttributeOverrideChecker
          : public AttributeVisitor<AttributeOverrideChecker> {
    TypeChecker &TC;
    ValueDecl *Base;
    ValueDecl *Override;

  public:
    AttributeOverrideChecker(TypeChecker &tc, ValueDecl *base,
                             ValueDecl *override)
      : TC(tc), Base(base), Override(override) { }

    /// Deleting this ensures that all attributes are covered by the visitor
    /// below.
    void visitDeclAttribute(DeclAttribute *A) = delete;

#define UNINTERESTING_ATTR(CLASS)                                              \
    void visit##CLASS##Attr(CLASS##Attr *) {}

    UNINTERESTING_ATTR(AccessControl)
    UNINTERESTING_ATTR(Alignment)
    UNINTERESTING_ATTR(CDecl)
    UNINTERESTING_ATTR(Consuming)
    UNINTERESTING_ATTR(DynamicMemberLookup)
    UNINTERESTING_ATTR(SILGenName)
    UNINTERESTING_ATTR(Exported)
    UNINTERESTING_ATTR(GKInspectable)
    UNINTERESTING_ATTR(IBAction)
    UNINTERESTING_ATTR(IBDesignable)
    UNINTERESTING_ATTR(IBInspectable)
    UNINTERESTING_ATTR(IBOutlet)
    UNINTERESTING_ATTR(Indirect)
    UNINTERESTING_ATTR(Inline)
    UNINTERESTING_ATTR(Optimize)
    UNINTERESTING_ATTR(Inlinable)
    UNINTERESTING_ATTR(Effects)
    UNINTERESTING_ATTR(FixedLayout)
    UNINTERESTING_ATTR(Lazy)
    UNINTERESTING_ATTR(LLDBDebuggerFunction)
    UNINTERESTING_ATTR(Mutating)
    UNINTERESTING_ATTR(NonMutating)
    UNINTERESTING_ATTR(NonObjC)
    UNINTERESTING_ATTR(NoReturn)
    UNINTERESTING_ATTR(NSApplicationMain)
    UNINTERESTING_ATTR(NSCopying)
    UNINTERESTING_ATTR(NSManaged)
    UNINTERESTING_ATTR(ObjCBridged)
    UNINTERESTING_ATTR(Optional)
    UNINTERESTING_ATTR(Override)
    UNINTERESTING_ATTR(RawDocComment)
    UNINTERESTING_ATTR(Required)
    UNINTERESTING_ATTR(Convenience)
    UNINTERESTING_ATTR(Semantics)
    UNINTERESTING_ATTR(SetterAccess)
    UNINTERESTING_ATTR(UIApplicationMain)
    UNINTERESTING_ATTR(UsableFromInline)
    UNINTERESTING_ATTR(ObjCNonLazyRealization)
    UNINTERESTING_ATTR(UnsafeNoObjCTaggedPointer)
    UNINTERESTING_ATTR(SwiftNativeObjCRuntimeBase)
    UNINTERESTING_ATTR(ShowInInterface)
    UNINTERESTING_ATTR(Specialize)

    // These can't appear on overridable declarations.
    UNINTERESTING_ATTR(Prefix)
    UNINTERESTING_ATTR(Postfix)
    UNINTERESTING_ATTR(Infix)
    UNINTERESTING_ATTR(ReferenceOwnership)

    UNINTERESTING_ATTR(SynthesizedProtocol)
    UNINTERESTING_ATTR(RequiresStoredPropertyInits)
    UNINTERESTING_ATTR(Transparent)
    UNINTERESTING_ATTR(SILStored)
    UNINTERESTING_ATTR(Testable)

    UNINTERESTING_ATTR(WarnUnqualifiedAccess)
    UNINTERESTING_ATTR(DiscardableResult)

    UNINTERESTING_ATTR(ObjCMembers)
    UNINTERESTING_ATTR(ObjCRuntimeName)
    UNINTERESTING_ATTR(RestatedObjCConformance)
    UNINTERESTING_ATTR(Implements)
    UNINTERESTING_ATTR(StaticInitializeObjCMetadata)
    UNINTERESTING_ATTR(DowngradeExhaustivityCheck)
    UNINTERESTING_ATTR(ImplicitlyUnwrappedOptional)
    UNINTERESTING_ATTR(ClangImporterSynthesizedType)
    UNINTERESTING_ATTR(WeakLinked)
    UNINTERESTING_ATTR(Frozen)
#undef UNINTERESTING_ATTR

    void visitAvailableAttr(AvailableAttr *attr) {
      // FIXME: Check that this declaration is at least as available as the
      // one it overrides.
    }

    void visitRethrowsAttr(RethrowsAttr *attr) {
      // 'rethrows' functions are a subtype of ordinary 'throws' functions.
      // Require 'rethrows' on the override if it was there on the base,
      // unless the override is completely non-throwing.
      if (!Override->getAttrs().hasAttribute<RethrowsAttr>() &&
          cast<AbstractFunctionDecl>(Override)->hasThrows()) {
        TC.diagnose(Override, diag::override_rethrows_with_non_rethrows,
                    isa<ConstructorDecl>(Override));
        TC.diagnose(Base, diag::overridden_here);
      }
    }

    void visitFinalAttr(FinalAttr *attr) {
      // If this is an accessor, don't complain if we would have
      // complained about the storage declaration.
      if (auto accessor = dyn_cast<AccessorDecl>(Override)) {
        if (auto storageDecl = accessor->getStorage()) {
          if (storageDecl->getOverriddenDecl() &&
              storageDecl->getOverriddenDecl()->isFinal())
            return;
        }
      }

      // FIXME: Customize message to the kind of thing.
      auto baseKind = Base->getDescriptiveKind();
      switch (baseKind) {
      case DescriptiveDeclKind::StaticLet:
      case DescriptiveDeclKind::StaticVar:
      case DescriptiveDeclKind::StaticMethod:
        TC.diagnose(Override, diag::override_static, baseKind);
        break;
      default:
        TC.diagnose(Override, diag::override_final,
                    Override->getDescriptiveKind(), baseKind);
        break;
      }

      TC.diagnose(Base, diag::overridden_here);
    }

    void visitDynamicAttr(DynamicAttr *attr) {
      // Final overrides are not dynamic.
      if (Override->isFinal())
        return;

      makeDynamic(TC.Context, Override);
    }

    void visitObjCAttr(ObjCAttr *attr) {
      // Checking for overrides of declarations that are implicitly @objc
      // and occur in class extensions, because overriding will no longer be
      // possible under the Swift 4 rules.

      // We only care about the storage declaration.
      if (isa<AccessorDecl>(Override)) return;

      // If @objc was explicit or handled elsewhere, nothing to do.
      if (!attr->isSwift3Inferred()) return;

      // If we aren't warning about Swift 3 @objc inference, we're done.
      if (TC.Context.LangOpts.WarnSwift3ObjCInference ==
            Swift3ObjCInferenceWarnings::None)
        return;

      // If 'dynamic' was implicit, we'll already have warned about this.
      if (auto dynamicAttr = Base->getAttrs().getAttribute<DynamicAttr>()) {
        if (!dynamicAttr->isImplicit()) return;
      }

      // The overridden declaration needs to be in an extension.
      if (!isa<ExtensionDecl>(Base->getDeclContext())) return;

      // Complain.
      TC.diagnose(Override, diag::override_swift3_objc_inference,
                  Override->getDescriptiveKind(),
                  Override->getFullName(),
                  Base->getDeclContext()
                    ->getAsNominalTypeOrNominalTypeExtensionContext()
                    ->getName());
      TC.diagnose(Base, diag::make_decl_objc, Base->getDescriptiveKind())
        .fixItInsert(Base->getAttributeInsertionLoc(false),
                     "@objc ");
    }
  };

  /// Determine whether overriding the given declaration requires a keyword.
  static bool overrideRequiresKeyword(ValueDecl *overridden) {
    if (auto ctor = dyn_cast<ConstructorDecl>(overridden)) {
      return ctor->isDesignatedInit() && !ctor->isRequired();
    }

    return true;
  }

  /// Returns true if a diagnostic about an accessor being less available
  /// than the accessor it overrides would be redundant because we will
  /// already emit another diagnostic.
  static bool
  isRedundantAccessorOverrideAvailabilityDiagnostic(TypeChecker &TC,
                                                    ValueDecl *override,
                                                    ValueDecl *base) {

    auto *overrideFn = dyn_cast<AccessorDecl>(override);
    auto *baseFn = dyn_cast<AccessorDecl>(base);
    if (!overrideFn || !baseFn)
      return false;

    AbstractStorageDecl *overrideASD = overrideFn->getStorage();
    AbstractStorageDecl *baseASD = baseFn->getStorage();
    if (overrideASD->getOverriddenDecl() != baseASD)
      return false;

    // If we have already emitted a diagnostic about an unsafe override
    // for the property, don't complain about the accessor.
    if (!TC.isAvailabilitySafeForOverride(overrideASD, baseASD)) {
      return true;
    }

    // Returns true if we will already diagnose a bad override
    // on the property's accessor of the given kind.
    auto accessorOverrideAlreadyDiagnosed = [&](AccessorKind kind) {
      FuncDecl *overrideAccessor = overrideASD->getAccessorFunction(kind);
      FuncDecl *baseAccessor = baseASD->getAccessorFunction(kind);
      if (overrideAccessor && baseAccessor &&
          !TC.isAvailabilitySafeForOverride(overrideAccessor, baseAccessor)) {
        return true;
      }
      return false;
    };

    // If we have already emitted a diagnostic about an unsafe override
    // for a getter or a setter, no need to complain about materializeForSet,
    // which is synthesized to be as available as both the getter and
    // the setter.
    if (overrideFn->isMaterializeForSet()) {
      if (accessorOverrideAlreadyDiagnosed(AccessorKind::IsGetter) ||
          accessorOverrideAlreadyDiagnosed(AccessorKind::IsSetter)) {
        return true;
      }
    }

    return false;
  }

  /// Diagnose an override for potential availability. Returns true if
  /// a diagnostic was emitted and false otherwise.
  static bool diagnoseOverrideForAvailability(TypeChecker &TC,
                                              ValueDecl *override,
                                              ValueDecl *base) {
    if (TC.isAvailabilitySafeForOverride(override, base))
      return false;

    // Suppress diagnostics about availability overrides for accessors
    // if they would be redundant with other diagnostics.
    if (isRedundantAccessorOverrideAvailabilityDiagnostic(TC, override, base))
      return false;

    if (auto *accessor = dyn_cast<AccessorDecl>(override)) {
      TC.diagnose(override, diag::override_accessor_less_available,
                  accessor->getDescriptiveKind(),
                  accessor->getStorage()->getBaseName());
      TC.diagnose(base, diag::overridden_here);
      return true;
    }

    TC.diagnose(override, diag::override_less_available,
                override->getBaseName());
    TC.diagnose(base, diag::overridden_here);

    return true;
  }

  /// Record that the \c overriding declarations overrides the
  /// \c overridden declaration.
  ///
  /// \returns true if an error occurred.
  static bool recordOverride(TypeChecker &TC, ValueDecl *override,
                             ValueDecl *base, bool isKnownObjC = false) {
    // Check property and subscript overriding.
    if (auto *baseASD = dyn_cast<AbstractStorageDecl>(base)) {
      auto *overrideASD = cast<AbstractStorageDecl>(override);
      
      // Make sure that the overriding property doesn't have storage.
      if (overrideASD->hasStorage() && !overrideASD->hasObservers()) {
        bool downgradeToWarning = false;
        if (!TC.Context.isSwiftVersionAtLeast(5) &&
            overrideASD->getAttrs().hasAttribute<LazyAttr>()) {
          // Swift 4.0 had a bug where lazy properties were considered
          // computed by the time of this check. Downgrade this diagnostic to
          // a warning.
          downgradeToWarning = true;
        }
        auto diagID = downgradeToWarning ?
            diag::override_with_stored_property_warn :
            diag::override_with_stored_property;
        TC.diagnose(overrideASD, diagID,
                    overrideASD->getBaseName().getIdentifier());
        TC.diagnose(baseASD, diag::property_override_here);
        if (!downgradeToWarning)
          return true;
      }

      // Make sure that an observing property isn't observing something
      // read-only.  Observing properties look at change, read-only properties
      // have nothing to observe!
      bool baseIsSettable = baseASD->isSettable(baseASD->getDeclContext());
      if (baseIsSettable && TC.Context.LangOpts.EnableAccessControl) {
        baseIsSettable =
           baseASD->isSetterAccessibleFrom(overrideASD->getDeclContext());
      }
      if (overrideASD->hasObservers() && !baseIsSettable) {
        TC.diagnose(overrideASD, diag::observing_readonly_property,
                    overrideASD->getBaseName().getIdentifier());
        TC.diagnose(baseASD, diag::property_override_here);
        return true;
      }

      // Make sure we're not overriding a settable property with a non-settable
      // one.  The only reasonable semantics for this would be to inherit the
      // setter but override the getter, and that would be surprising at best.
      if (baseIsSettable && !override->isSettable(override->getDeclContext())) {
        TC.diagnose(overrideASD, diag::override_mutable_with_readonly_property,
                    overrideASD->getBaseName().getIdentifier());
        TC.diagnose(baseASD, diag::property_override_here);
        return true;
      }
      
      
      // Make sure a 'let' property is only overridden by 'let' properties.  A
      // let property provides more guarantees than the getter of a 'var'
      // property.
      if (auto VD = dyn_cast<VarDecl>(baseASD)) {
        if (VD->isLet()) {
          TC.diagnose(overrideASD, diag::override_let_property,
                      VD->getName());
          TC.diagnose(baseASD, diag::property_override_here);
          return true;
        }
      }
    }
    
    // Non-Objective-C declarations in extensions cannot override or
    // be overridden.
    if ((base->getDeclContext()->isExtensionContext() ||
         override->getDeclContext()->isExtensionContext()) &&
        !base->isObjC() && !isKnownObjC) {
      bool baseCanBeObjC = TC.canBeRepresentedInObjC(base);
      TC.diagnose(override, diag::override_decl_extension, baseCanBeObjC,
                  !base->getDeclContext()->isExtensionContext());
      if (baseCanBeObjC) {
        SourceLoc insertionLoc =
          override->getAttributeInsertionLoc(/*forModifier=*/false);
        TC.diagnose(base, diag::overridden_here_can_be_objc)
          .fixItInsert(insertionLoc, "@objc ");
      } else {
        TC.diagnose(base, diag::overridden_here);
      }

      return true;
    }
    
    // If the overriding declaration does not have the 'override' modifier on
    // it, complain.
    if (!override->getAttrs().hasAttribute<OverrideAttr>() &&
        overrideRequiresKeyword(base)) {
      // FIXME: rdar://16320042 - For properties, we don't have a useful
      // location for the 'var' token.  Instead of emitting a bogus fixit, only
      // emit the fixit for 'func's.
      if (!isa<VarDecl>(override))
        TC.diagnose(override, diag::missing_override)
            .fixItInsert(override->getStartLoc(), "override ");
      else
        TC.diagnose(override, diag::missing_override);
      TC.diagnose(base, diag::overridden_here);
      override->getAttrs().add(
          new (TC.Context) OverrideAttr(SourceLoc()));
    }

    // If the overridden method is declared in a Swift Class Declaration,
    // dispatch will use table dispatch. If the override is in an extension
    // warn, since it is not added to the class vtable.
    //
    // FIXME: Only warn if the extension is in another module, and if
    // it is in the same module, update the vtable.
    if (auto *baseDecl = dyn_cast<ClassDecl>(base->getDeclContext())) {
      if (baseDecl->hasKnownSwiftImplementation() && 
          !base->isDynamic() && !isKnownObjC &&
          override->getDeclContext()->isExtensionContext()) {
        // For compatibility, only generate a warning in Swift 3
        TC.diagnose(override, (TC.Context.isSwiftVersion3()
          ? diag::override_class_declaration_in_extension_warning
          : diag::override_class_declaration_in_extension));
        TC.diagnose(base, diag::overridden_here);
      }
    }
    // If the overriding declaration is 'throws' but the base is not,
    // complain.
    if (auto overrideFn = dyn_cast<AbstractFunctionDecl>(override)) {
      if (overrideFn->hasThrows() &&
          !cast<AbstractFunctionDecl>(base)->hasThrows()) {
        TC.diagnose(override, diag::override_throws,
                    isa<ConstructorDecl>(override));
        TC.diagnose(base, diag::overridden_here);
      }

      if (!overrideFn->hasThrows() && base->isObjC() &&
          cast<AbstractFunctionDecl>(base)->hasThrows()) {
        TC.diagnose(override, diag::override_throws_objc,
                    isa<ConstructorDecl>(override));
        TC.diagnose(base, diag::overridden_here);
      }
    }

    // FIXME: Possibly should extend to more availability checking.
    if (auto *attr = base->getAttrs().getUnavailable(TC.Context)) {
      TC.diagnoseUnavailableOverride(override, base, attr);
    }
    
    if (!TC.getLangOpts().DisableAvailabilityChecking) {
      diagnoseOverrideForAvailability(TC, override, base);
    }

    /// Check attributes associated with the base; some may need to merged with
    /// or checked against attributes in the overriding declaration.
    AttributeOverrideChecker attrChecker(TC, base, override);
    for (auto attr : base->getAttrs()) {
      attrChecker.visit(attr);
    }

    if (auto overridingFunc = dyn_cast<FuncDecl>(override)) {
      overridingFunc->setOverriddenDecl(cast<FuncDecl>(base));
    } else if (auto overridingCtor = dyn_cast<ConstructorDecl>(override)) {
      overridingCtor->setOverriddenDecl(cast<ConstructorDecl>(base));
    } else if (auto overridingASD = dyn_cast<AbstractStorageDecl>(override)) {
      auto *baseASD = cast<AbstractStorageDecl>(base);
      overridingASD->setOverriddenDecl(baseASD);

      // Make sure we get consistent overrides for the accessors as well.
      assert(baseASD->hasAccessorFunctions());

      auto recordAccessorOverride = [&](AccessorKind kind) {
        // We need the same accessor on both.
        auto baseAccessor = baseASD->getAccessorFunction(kind);
        if (!baseAccessor) return;
        auto overridingAccessor = overridingASD->getAccessorFunction(kind);
        if (!overridingAccessor) return;

        // For setter accessors, we need the base's setter to be
        // accessible from the overriding context, or it's not an override.
        if ((kind == AccessorKind::IsSetter ||
             kind == AccessorKind::IsMaterializeForSet) &&
            !baseASD->isSetterAccessibleFrom(overridingASD->getDeclContext()))
          return;

        // A materializeForSet for an override of storage with a
        // forced static dispatch materializeForSet is not itself an
        // override.
        if (kind == AccessorKind::IsMaterializeForSet &&
            baseAccessor->hasForcedStaticDispatch())
          return;

        // FIXME: Egregious hack to set an 'override' attribute.
        if (!overridingAccessor->getAttrs().hasAttribute<OverrideAttr>()) {
          auto loc = overridingASD->getOverrideLoc();
          overridingAccessor->getAttrs().add(
              new (TC.Context) OverrideAttr(loc));
        }

        recordOverride(TC, overridingAccessor, baseAccessor,
                       baseASD->isObjC());
      };

      recordAccessorOverride(AccessorKind::IsGetter);
      recordAccessorOverride(AccessorKind::IsSetter);
      recordAccessorOverride(AccessorKind::IsMaterializeForSet);
    } else {
      llvm_unreachable("Unexpected decl");
    }
    
    return false;
  }

  void visitEnumCaseDecl(EnumCaseDecl *ECD) {
    // The type-checker doesn't care about how these are grouped.
  }

  void visitEnumElementDecl(EnumElementDecl *EED) {
    TC.validateDecl(EED);
    TC.checkDeclAttributes(EED);
    checkAccessControl(TC, EED);
  }

  void visitExtensionDecl(ExtensionDecl *ED) {
    TC.validateExtension(ED);

    TC.checkDeclAttributesEarly(ED);

    if (auto extendedTy = ED->getExtendedType()) {
      if (!extendedTy->is<NominalType>() &&
          !extendedTy->is<BoundGenericType>() &&
          !extendedTy->hasError()) {
        // FIXME: Redundant diagnostic test here?
        TC.diagnose(ED->getStartLoc(), diag::non_nominal_extension,
                    extendedTy);
        // FIXME: It would be nice to point out where we found the named type
        // declaration, if any.
        ED->setInvalid();
      }
    }

    TC.checkInheritanceClause(ED);
    if (auto extendedTy = ED->getExtendedType()) {
      if (auto nominal = extendedTy->getAnyNominal()) {
        TC.validateDecl(nominal);
        if (auto *classDecl = dyn_cast<ClassDecl>(nominal))
          TC.requestNominalLayout(classDecl);

        // Check the raw values of an enum, since we might synthesize
        // RawRepresentable while checking conformances on this extension.
        if (auto enumDecl = dyn_cast<EnumDecl>(nominal)) {
          if (enumDecl->hasRawType())
            checkEnumRawValues(TC, enumDecl);
        }
      }
    }

    validateAttributes(TC, ED);

    TC.computeDefaultAccessLevel(ED);

    for (Decl *Member : ED->getMembers())
      visit(Member);

    TC.ConformanceContexts.push_back(ED);

    if (!ED->isInvalid())
      TC.checkDeclAttributes(ED);

    if (auto *AA = ED->getAttrs().getAttribute<AccessControlAttr>()) {
      const auto access = AA->getAccess();
      AccessScope desiredAccessScope = AccessScope::getPublic();
      switch (access) {
      case AccessLevel::Private:
        assert((ED->isInvalid() ||
                ED->getDeclContext()->isModuleScopeContext()) &&
               "non-top-level extensions make 'private' != 'fileprivate'");
        LLVM_FALLTHROUGH;
      case AccessLevel::FilePrivate: {
        const DeclContext *DC = ED->getModuleScopeContext();
        bool isPrivate = access == AccessLevel::Private;
        desiredAccessScope = AccessScope(DC, isPrivate);
        break;
      }
      case AccessLevel::Internal:
        desiredAccessScope = AccessScope(ED->getModuleContext());
        break;
      case AccessLevel::Public:
      case AccessLevel::Open:
        break;
      }
      checkGenericParamAccess(TC, ED->getGenericParams(), ED,
                              desiredAccessScope, access);
    }
  }

  void visitTopLevelCodeDecl(TopLevelCodeDecl *TLCD) {
    // See swift::performTypeChecking for TopLevelCodeDecl handling.
    llvm_unreachable("TopLevelCodeDecls are handled elsewhere");
  }
  
  void visitIfConfigDecl(IfConfigDecl *ICD) {
    // The active members of the #if block will be type checked along with
    // their enclosing declaration.
    TC.checkDeclAttributesEarly(ICD);
    TC.checkDeclAttributes(ICD);
  }

  void visitPoundDiagnosticDecl(PoundDiagnosticDecl *PDD) {
    if (PDD->hasBeenEmitted()) { return; }
    PDD->markEmitted();
    TC.diagnose(PDD->getMessage()->getStartLoc(),
      PDD->isError() ? diag::pound_error : diag::pound_warning,
      PDD->getMessage()->getValue())
      .highlight(PDD->getMessage()->getSourceRange());
  }

  void visitConstructorDecl(ConstructorDecl *CD) {
    TC.validateDecl(CD);

    // If this initializer overrides a 'required' initializer, it must itself
    // be marked 'required'.
    if (!CD->getAttrs().hasAttribute<RequiredAttr>()) {
      if (CD->getOverriddenDecl() && CD->getOverriddenDecl()->isRequired()) {
        TC.diagnose(CD, diag::required_initializer_missing_keyword)
          .fixItInsert(CD->getLoc(), "required ");

        TC.diagnose(findNonImplicitRequiredInit(CD->getOverriddenDecl()),
                    diag::overridden_required_initializer_here);

        CD->getAttrs().add(
            new (TC.Context) RequiredAttr(/*IsImplicit=*/true));
      }
    }

    if (CD->isRequired()) {
      if (auto nominal = CD->getDeclContext()
              ->getAsNominalTypeOrNominalTypeExtensionContext()) {
        AccessLevel requiredAccess;
        switch (nominal->getFormalAccess()) {
        case AccessLevel::Open:
          requiredAccess = AccessLevel::Public;
          break;
        case AccessLevel::Public:
        case AccessLevel::Internal:
          requiredAccess = AccessLevel::Internal;
          break;
        case AccessLevel::FilePrivate:
        case AccessLevel::Private:
          requiredAccess = AccessLevel::FilePrivate;
          break;
        }
        if (CD->getFormalAccess() < requiredAccess) {
          auto diag = TC.diagnose(CD, diag::required_initializer_not_accessible,
                                  nominal->getFullName());
          fixItAccess(diag, CD, requiredAccess);
        }
      }
    }

    TC.checkDeclAttributes(CD);
    checkAccessControl(TC, CD);

    if (CD->hasBody() && !CD->isMemberwiseInitializer()) {
      TC.definedFunctions.push_back(CD);
    } else if (requiresDefinition(CD)) {
      // Complain if we should have a body.
      TC.diagnose(CD->getLoc(), diag::missing_initializer_def);
    }
  }

  void visitDestructorDecl(DestructorDecl *DD) {
    TC.validateDecl(DD);
    TC.checkDeclAttributes(DD);

    if (DD->hasBody())
      TC.definedFunctions.push_back(DD);
  }
};
} // end anonymous namespace

bool swift::checkOverrides(TypeChecker &TC, ValueDecl *decl) {
  return DeclChecker::checkOverrides(TC, decl);
}

bool TypeChecker::isAvailabilitySafeForOverride(ValueDecl *override,
                                                ValueDecl *base) {
  // API availability ranges are contravariant: make sure the version range
  // of an overridden declaration is fully contained in the range of the
  // overriding declaration.
  AvailabilityContext overrideInfo =
      AvailabilityInference::availableRange(override, Context);
  AvailabilityContext baseInfo =
      AvailabilityInference::availableRange(base, Context);

  return baseInfo.isContainedIn(overrideInfo);
}

bool TypeChecker::isAvailabilitySafeForConformance(
    ProtocolDecl *proto, ValueDecl *requirement, ValueDecl *witness,
    DeclContext *dc, AvailabilityContext &requirementInfo) {

  // We assume conformances in
  // non-SourceFiles have already been checked for availability.
  if (!dc->getParentSourceFile())
    return true;

  NominalTypeDecl *conformingDecl = dc->getAsNominalTypeOrNominalTypeExtensionContext();
  assert(conformingDecl && "Must have conforming declaration");

  // Make sure that any access of the witness through the protocol
  // can only occur when the witness is available. That is, make sure that
  // on every version where the conforming declaration is available, if the
  // requirement is available then the witness is available as well.
  // We do this by checking that (an over-approximation of) the intersection of
  // the requirement's available range with both the conforming declaration's
  // available range and the protocol's available range is fully contained in
  // (an over-approximation of) the intersection of the witnesses's available
  // range with both the conforming type's available range and the protocol
  // declaration's available range.
  AvailabilityContext witnessInfo =
      AvailabilityInference::availableRange(witness, Context);
  requirementInfo = AvailabilityInference::availableRange(requirement, Context);

  AvailabilityContext infoForConformingDecl =
      overApproximateAvailabilityAtLocation(conformingDecl->getLoc(),
                                            conformingDecl);

  // Constrain over-approximates intersection of version ranges.
  witnessInfo.constrainWith(infoForConformingDecl);
  requirementInfo.constrainWith(infoForConformingDecl);

  AvailabilityContext infoForProtocolDecl =
      overApproximateAvailabilityAtLocation(proto->getLoc(), proto);

  witnessInfo.constrainWith(infoForProtocolDecl);
  requirementInfo.constrainWith(infoForProtocolDecl);

  return requirementInfo.isContainedIn(witnessInfo);
}

void TypeChecker::typeCheckDecl(Decl *D) {
  checkForForbiddenPrefix(D);
  DeclChecker(*this).visit(D);
}

// A class is @objc if it does not have generic ancestry, and it either has
// an explicit @objc attribute, or its superclass is @objc.
static Optional<ObjCReason> shouldMarkClassAsObjC(TypeChecker &TC,
                                                  ClassDecl *CD) {
  ObjCClassKind kind = CD->checkObjCAncestry();

  if (auto attr = CD->getAttrs().getAttribute<ObjCAttr>()) {
    if (kind == ObjCClassKind::ObjCMembers) {
      if (attr->hasName() && !CD->isGenericContext()) {
        // @objc with a name on a non-generic subclass of a generic class is
        // just controlling the runtime name. Don't diagnose this case.
        CD->getAttrs().add(new (TC.Context) ObjCRuntimeNameAttr(*attr));
        return None;
      }

      TC.diagnose(attr->getLocation(), diag::objc_for_generic_class)
        .fixItRemove(attr->getRangeWithAt());
    }

    // Only allow ObjC-rooted classes to be @objc.
    // (Leave a hole for test cases.)
    if (kind == ObjCClassKind::ObjCWithSwiftRoot &&
        TC.getLangOpts().EnableObjCAttrRequiresFoundation) {
      TC.diagnose(attr->getLocation(), diag::invalid_objc_swift_rooted_class)
        .fixItRemove(attr->getRangeWithAt());
    }

    return ObjCReason::ExplicitlyObjC;
  }

  if (kind == ObjCClassKind::ObjCWithSwiftRoot ||
      kind == ObjCClassKind::ObjC)
    return ObjCReason::ImplicitlyObjC;

  return None;
}

/// Validate the underlying type of the given typealias.
static void validateTypealiasType(TypeChecker &tc, TypeAliasDecl *typeAlias) {
  TypeResolutionOptions options = TypeResolutionFlags::TypeAliasUnderlyingType;
  if (!typeAlias->getDeclContext()->isCascadingContextForLookup(
        /*functionsAreNonCascading*/true)) {
     options |= TypeResolutionFlags::KnownNonCascadingDependency;
  }

  if (typeAlias->getDeclContext()->isModuleScopeContext() &&
      typeAlias->getGenericParams() == nullptr) {
    IterativeTypeChecker ITC(tc);
    ITC.satisfy(requestResolveTypeDecl(typeAlias));
  } else {
    if (tc.validateType(typeAlias->getUnderlyingTypeLoc(),
                        typeAlias, options)) {
      typeAlias->setInvalid();
      typeAlias->getUnderlyingTypeLoc().setInvalidType(tc.Context);
    }

    typeAlias->setUnderlyingType(typeAlias->getUnderlyingTypeLoc().getType());
  }
}


/// Bind the given function declaration, which declares an operator, to
/// the corresponding operator declaration.
void bindFuncDeclToOperator(TypeChecker &TC, FuncDecl *FD) {
  OperatorDecl *op = nullptr;
  auto operatorName = FD->getFullName().getBaseIdentifier();

  // Check for static/final/class when we're in a type.
  auto dc = FD->getDeclContext();
  if (dc->isTypeContext()) {
    if (!FD->isStatic()) {
      TC.diagnose(FD->getLoc(), diag::nonstatic_operator_in_type,
                  operatorName,
                  dc->getDeclaredInterfaceType())
        .fixItInsert(FD->getAttributeInsertionLoc(/*forModifier=*/true),
                     "static ");

      FD->setStatic();
    } else if (auto classDecl = dc->getAsClassOrClassExtensionContext()) {
      // For a class, we also need the function or class to be 'final'.
      if (!classDecl->isFinal() && !FD->isFinal() &&
          FD->getStaticSpelling() != StaticSpellingKind::KeywordStatic) {
        TC.diagnose(FD->getLoc(), diag::nonfinal_operator_in_class,
                    operatorName, dc->getDeclaredInterfaceType())
          .fixItInsert(FD->getAttributeInsertionLoc(/*forModifier=*/true),
                       "final ");
        FD->getAttrs().add(new (TC.Context) FinalAttr(/*IsImplicit=*/true));
      }
    }
  } else if (!dc->isModuleScopeContext()) {
    TC.diagnose(FD, diag::operator_in_local_scope);
  }

  SourceFile &SF = *FD->getDeclContext()->getParentSourceFile();
  if (FD->isUnaryOperator()) {
    if (FD->getAttrs().hasAttribute<PrefixAttr>()) {
      op = SF.lookupPrefixOperator(operatorName,
                                   FD->isCascadingContextForLookup(false),
                                   FD->getLoc());
    } else if (FD->getAttrs().hasAttribute<PostfixAttr>()) {
      op = SF.lookupPostfixOperator(operatorName,
                                    FD->isCascadingContextForLookup(false),
                                    FD->getLoc());
    } else {
      auto prefixOp =
          SF.lookupPrefixOperator(operatorName,
                                  FD->isCascadingContextForLookup(false),
                                  FD->getLoc());
      auto postfixOp =
          SF.lookupPostfixOperator(operatorName,
                                   FD->isCascadingContextForLookup(false),
                                   FD->getLoc());

      // If we found both prefix and postfix, or neither prefix nor postfix,
      // complain. We can't fix this situation.
      if (static_cast<bool>(prefixOp) == static_cast<bool>(postfixOp)) {
        TC.diagnose(FD, diag::declared_unary_op_without_attribute);

        // If we found both, point at them.
        if (prefixOp) {
          TC.diagnose(prefixOp, diag::unary_operator_declaration_here, false)
            .fixItInsert(FD->getLoc(), "prefix ");
          TC.diagnose(postfixOp, diag::unary_operator_declaration_here, true)
            .fixItInsert(FD->getLoc(), "postfix ");
        } else {
          // FIXME: Introduce a Fix-It that adds the operator declaration?
        }

        // FIXME: Errors could cascade here, because name lookup for this
        // operator won't find this declaration.
        return;
      }

      // We found only one operator declaration, so we know whether this
      // should be a prefix or a postfix operator.

      // Fix the AST and determine the insertion text.
      const char *insertionText;
      auto &C = FD->getASTContext();
      if (postfixOp) {
        insertionText = "postfix ";
        op = postfixOp;
        FD->getAttrs().add(new (C) PostfixAttr(/*implicit*/false));
      } else {
        insertionText = "prefix ";
        op = prefixOp;
        FD->getAttrs().add(new (C) PrefixAttr(/*implicit*/false));
      }

      // Emit diagnostic with the Fix-It.
      TC.diagnose(FD->getFuncLoc(), diag::unary_op_missing_prepos_attribute,
                  static_cast<bool>(postfixOp))
        .fixItInsert(FD->getFuncLoc(), insertionText);
      TC.diagnose(op, diag::unary_operator_declaration_here,
                  static_cast<bool>(postfixOp));
    }
  } else if (FD->isBinaryOperator()) {
    op = SF.lookupInfixOperator(operatorName,
                                FD->isCascadingContextForLookup(false),
                                FD->getLoc());
  } else {
    TC.diagnose(FD, diag::invalid_arg_count_for_operator);
    return;
  }

  if (!op) {
    // FIXME: Add Fix-It introducing an operator declaration?
    TC.diagnose(FD, diag::declared_operator_without_operator_decl);
    return;
  }

  FD->setOperatorDecl(op);
}

void checkMemberOperator(TypeChecker &TC, FuncDecl *FD) {
  // Check that member operators reference the type of 'Self'.
  if (FD->getNumParameterLists() != 2 || FD->isInvalid()) return;

  auto *DC = FD->getDeclContext();
  auto selfNominal = DC->getAsNominalTypeOrNominalTypeExtensionContext();
  if (!selfNominal) return;

  // Check the parameters for a reference to 'Self'.
  bool isProtocol = isa<ProtocolDecl>(selfNominal);
  for (auto param : *FD->getParameterList(1)) {
    auto paramType = param->getInterfaceType();
    if (!paramType) break;

    // Look through 'inout'.
    paramType = paramType->getInOutObjectType();
    // Look through a metatype reference, if there is one.
    if (auto metatypeType = paramType->getAs<AnyMetatypeType>())
      paramType = metatypeType->getInstanceType();

    // Is it the same nominal type?
    if (paramType->getAnyNominal() == selfNominal) return;

    if (isProtocol) {
      // For a protocol, is it the 'Self' type parameter?
      if (auto genericParam = paramType->getAs<GenericTypeParamType>())
        if (genericParam->isEqual(DC->getSelfInterfaceType()))
          return;
    }
  }

  // We did not find 'Self'. Complain.
  TC.diagnose(FD, diag::operator_in_unrelated_type,
              FD->getDeclContext()->getDeclaredInterfaceType(),
              isProtocol, FD->getFullName());
}

bool checkDynamicSelfReturn(TypeChecker &TC, FuncDecl *func,
                            TypeRepr *typeRepr,
                            unsigned optionalDepth) {
  // Look through parentheses.
  if (auto parenRepr = dyn_cast<TupleTypeRepr>(typeRepr)) {
    if (!parenRepr->isParenType()) return false;
    return checkDynamicSelfReturn(TC, func, parenRepr->getElementType(0),
                                  optionalDepth);
  }

  // Look through attributes.
  if (auto attrRepr = dyn_cast<AttributedTypeRepr>(typeRepr)) {
    TypeAttributes attrs = attrRepr->getAttrs();
    if (!attrs.empty())
      return false;
    return checkDynamicSelfReturn(TC, func, attrRepr->getTypeRepr(),
                                  optionalDepth);
  }

  // Look through optional types.
  TypeRepr *base = nullptr;
  if (auto *optRepr = dyn_cast<OptionalTypeRepr>(typeRepr))
    base = optRepr->getBase();
  else if (auto *optRepr =
               dyn_cast<ImplicitlyUnwrappedOptionalTypeRepr>(typeRepr))
    base = optRepr->getBase();

  if (base) {
    // But only one level.
    if (optionalDepth != 0) return false;
    return checkDynamicSelfReturn(TC, func, base, optionalDepth + 1);
  }

  // Check whether we have a simple identifier type.
  auto simpleRepr = dyn_cast<SimpleIdentTypeRepr>(typeRepr);
  if (!simpleRepr)
    return false;

  // Check whether it is 'Self'.
  if (simpleRepr->getIdentifier() != TC.Context.Id_Self)
    return false;

  // Note that the function has a dynamic Self return type and set
  // the return type component to the dynamic self type.
  func->setDynamicSelf(true);
  return false;
}

/// Check for methods that return 'DynamicResult'.
bool checkDynamicSelfReturn(TypeChecker &TC, FuncDecl *func) {
  // Check whether we have a specified result type.
  auto typeRepr = func->getBodyResultTypeLoc().getTypeRepr();
  if (!typeRepr)
    return false;

  // 'Self' on a free function is not dynamic 'Self'.
  if (!func->getDeclContext()->getAsClassOrClassExtensionContext() &&
      !isa<ProtocolDecl>(func->getDeclContext()))
    return false;

  // 'Self' on a property accessor is not dynamic 'Self'...even on a read-only
  // property. We could implement it as such in the future.
  if (isa<AccessorDecl>(func))
    return false;

  return checkDynamicSelfReturn(TC, func, typeRepr, 0);
}

Type buildAddressorResultType(TypeChecker &TC,
                              AccessorDecl *addressor,
                              Type valueType) {
  assert(addressor->getAccessorKind() == AccessorKind::IsAddressor ||
         addressor->getAccessorKind() == AccessorKind::IsMutableAddressor);

  Type pointerType =
    (addressor->getAccessorKind() == AccessorKind::IsAddressor)
      ? TC.getUnsafePointerType(addressor->getLoc(), valueType)
      : TC.getUnsafeMutablePointerType(addressor->getLoc(), valueType);
  if (!pointerType) return Type();

  switch (addressor->getAddressorKind()) {
  case AddressorKind::NotAddressor:
    llvm_unreachable("addressor without addressor kind");

  // For unsafe addressors, it's just the pointer type.
  case AddressorKind::Unsafe:
    return pointerType;

  // For non-native owning addressors, the return type is actually
  //   (Unsafe{,Mutable}Pointer<T>, AnyObject)
  case AddressorKind::Owning: {
    TupleTypeElt elts[] = {
      pointerType,
      TC.Context.getAnyObjectType()
    };
    return TupleType::get(elts, TC.Context);
  }

  // For native owning addressors, the return type is actually
  //   (Unsafe{,Mutable}Pointer<T>, Builtin.NativeObject)
  case AddressorKind::NativeOwning: {
    TupleTypeElt elts[] = {
      pointerType,
      TC.Context.TheNativeObjectType
    };
    return TupleType::get(elts, TC.Context);
  }

  // For native pinning addressors, the return type is actually
  //   (Unsafe{,Mutable}Pointer<T>, Builtin.NativeObject?)
  case AddressorKind::NativePinning: {
    Type pinTokenType =
      TC.getOptionalType(addressor->getLoc(), TC.Context.TheNativeObjectType);
    if (!pinTokenType) return Type();

    TupleTypeElt elts[] = {
      pointerType,
      pinTokenType
    };
    return TupleType::get(elts, TC.Context);
  }
  }
  llvm_unreachable("bad addressor kind");
}

static TypeLoc getTypeLocForFunctionResult(FuncDecl *FD) {
  auto accessor = dyn_cast<AccessorDecl>(FD);
  if (!accessor) {
    return FD->getBodyResultTypeLoc();
  }

  assert(accessor->isGetter());
  auto *storage = accessor->getStorage();
  assert(isa<VarDecl>(storage) || isa<SubscriptDecl>(storage));

  if (auto *subscript = dyn_cast<SubscriptDecl>(storage))
    return subscript->getElementTypeLoc();

  return cast<VarDecl>(storage)->getTypeLoc();
}

void TypeChecker::validateDecl(ValueDecl *D) {
  // Generic parameters are validated as part of their context.
  if (isa<GenericTypeParamDecl>(D))
    return;

  // Handling validation failure due to re-entrancy is left
  // up to the caller, who must call hasValidSignature() to
  // check that validateDecl() returned a fully-formed decl.
  if (D->hasValidationStarted()) {
    // If this isn't reentrant (i.e. D has already been validated), the
    // signature better be valid.
    assert(D->isBeingValidated() || D->hasValidSignature());
    return;
  }

  // FIXME: It would be nicer if Sema would always synthesize fully-typechecked
  // declarations, but for now, you can make an imported type conform to a
  // protocol with property requirements, which requires synthesizing getters
  // and setters, etc.
  if (!isa<VarDecl>(D) && !isa<AccessorDecl>(D)) {
    assert(isa<SourceFile>(D->getDeclContext()->getModuleScopeContext()) &&
           "Should not validate imported or deserialized declarations");
  }

  PrettyStackTraceDecl StackTrace("validating", D);

  if (hasEnabledForbiddenTypecheckPrefix())
    checkForForbiddenPrefix(D);

  validateAccessControl(D);

  // Validate the context.
  auto dc = D->getDeclContext();
  if (auto nominal = dyn_cast<NominalTypeDecl>(dc)) {
    validateDecl(nominal);
    if (!nominal->hasValidSignature())
      return;
  } else if (auto ext = dyn_cast<ExtensionDecl>(dc)) {
    validateExtension(ext);
    if (!ext->hasValidSignature())
      return;
  }

  if (Context.Stats)
    Context.Stats->getFrontendCounters().NumDeclsValidated++;

  switch (D->getKind()) {
  case DeclKind::Import:
  case DeclKind::Extension:
  case DeclKind::PatternBinding:
  case DeclKind::EnumCase:
  case DeclKind::TopLevelCode:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
  case DeclKind::PrecedenceGroup:
  case DeclKind::IfConfig:
  case DeclKind::PoundDiagnostic:
  case DeclKind::MissingMember:
    llvm_unreachable("not a value decl");

  case DeclKind::Module:
    return;
      
  case DeclKind::GenericTypeParam:
    llvm_unreachable("handled above");

  case DeclKind::AssociatedType: {
    auto assocType = cast<AssociatedTypeDecl>(D);

    assocType->setIsBeingValidated();
    SWIFT_DEFER { assocType->setIsBeingValidated(false); };

    checkDeclAttributesEarly(assocType);
    checkInheritanceClause(assocType);

    // Check the default definition, if there is one.
    TypeLoc &defaultDefinition = assocType->getDefaultDefinitionLoc();
    if (!defaultDefinition.isNull()) {
      if (validateType(defaultDefinition, assocType->getDeclContext())) {
        defaultDefinition.setInvalidType(Context);
      } else {
        // associatedtype X = X is invalid
        auto mentionsItself =
            defaultDefinition.getType().findIf([&](Type type) {
              if (auto DMT = type->getAs<ArchetypeType>()) {
                return DMT->getAssocType() == assocType;
              }
              return false;
            });

        if (mentionsItself) {
          diagnose(defaultDefinition.getLoc(), diag::recursive_type_reference,
                   assocType->getDescriptiveKind(), assocType->getName());
          diagnose(assocType, diag::type_declared_here);
        }
      }
    }
    // Finally, set the interface type.
    if (!assocType->hasInterfaceType())
      assocType->computeType();

    checkDeclAttributes(assocType);
    break;
  }

  case DeclKind::TypeAlias: {
    auto typeAlias = cast<TypeAliasDecl>(D);
    // Check generic parameters, if needed.
    typeAlias->setIsBeingValidated();
    SWIFT_DEFER { typeAlias->setIsBeingValidated(false); };

    validateGenericTypeSignature(typeAlias);
    validateTypealiasType(*this, typeAlias);
    break;
  }

  case DeclKind::Enum:
  case DeclKind::Struct:
  case DeclKind::Class: {
    auto nominal = cast<NominalTypeDecl>(D);
    nominal->computeType();

    // Check generic parameters, if needed.
    nominal->setIsBeingValidated();
    validateGenericTypeSignature(nominal);
    nominal->setIsBeingValidated(false);

    checkInheritanceClause(D);

    validateAttributes(*this, D);

    if (auto CD = dyn_cast<ClassDecl>(nominal)) {
      // Mark a class as @objc. This must happen before checking its members.
      Optional<ObjCReason> isObjC = shouldMarkClassAsObjC(*this, CD);
      markAsObjC(*this, CD, isObjC);

      // Determine whether we require in-class initializers.
      if (CD->getAttrs().hasAttribute<RequiresStoredPropertyInitsAttr>() ||
          (CD->hasSuperclass() &&
           CD->getSuperclass()->getClassOrBoundGenericClass()
             ->requiresStoredPropertyInits()))
        CD->setRequiresStoredPropertyInits(true);

      // Inherit @objcMembers.
      if (auto superclass = CD->getSuperclassDecl()) {
        if (superclass->getAttrs().hasAttribute<ObjCMembersAttr>() &&
            !CD->getAttrs().hasAttribute<ObjCMembersAttr>()) {
          CD->getAttrs().add(new (Context) ObjCMembersAttr(/*IsImplicit=*/true));
        }
      }
    }

    if (auto *ED = dyn_cast<EnumDecl>(nominal)) {
      // @objc enums use their raw values as the value representation, so we
      // need to force the values to be checked.
      if (ED->isObjC())
        checkEnumRawValues(*this, ED);
    }

    if (!isa<ClassDecl>(nominal))
      requestNominalLayout(nominal);

    break;
  }

  case DeclKind::Protocol: {
    auto proto = cast<ProtocolDecl>(D);
    if (!proto->hasInterfaceType())
      proto->computeType();

    // Validate the generic type signature, which is just <Self : P>.
    proto->setIsBeingValidated();
    validateGenericTypeSignature(proto);
    proto->setIsBeingValidated(false);

    // See the comment in validateDeclForNameLookup(); we may have validated
    // the alias before we built the protocol's generic environment.
    //
    // FIXME: Hopefully this can all go away with the ITC.
    for (auto member : proto->getMembers()) {
      if (auto *aliasDecl = dyn_cast<TypeAliasDecl>(member)) {
        if (!aliasDecl->isGeneric()) {
          aliasDecl->setGenericEnvironment(proto->getGenericEnvironment());

          // If the underlying alias declaration has a type parameter,
          // we have unresolved dependent member types we will need to deal
          // with. Wipe out the types and validate them again.
          // FIXME: We never should have recorded such a type in the first
          // place.
          if (!aliasDecl->getUnderlyingTypeLoc().getType() ||
              aliasDecl->getUnderlyingTypeLoc().getType()
                ->findUnresolvedDependentMemberType()) {
            aliasDecl->getUnderlyingTypeLoc().setType(Type(),
                                                      /*validated=*/false);
            validateAccessControl(aliasDecl);

            // Check generic parameters, if needed.
            bool validated = aliasDecl->hasValidationStarted();
            if (!validated)
              aliasDecl->setIsBeingValidated();
            SWIFT_DEFER {
              if (!validated)
                aliasDecl->setIsBeingValidated(false);
            };

            validateTypealiasType(*this, aliasDecl);
          }
        }
      }
    }

    // Record inherited protocols.
    resolveInheritedProtocols(proto);

    validateAttributes(*this, D);

    // If the protocol is @objc, it may only refine other @objc protocols.
    // FIXME: Revisit this restriction.
    if (proto->getAttrs().hasAttribute<ObjCAttr>()) {
      Optional<ObjCReason> isObjC = ObjCReason::ImplicitlyObjC;

      for (auto inherited : proto->getInheritedProtocols()) {
        if (!inherited->isObjC()) {
          diagnose(proto->getLoc(),
                   diag::objc_protocol_inherits_non_objc_protocol,
                   proto->getDeclaredType(), inherited->getDeclaredType());
          diagnose(inherited->getLoc(), diag::protocol_here,
                   inherited->getName());
          isObjC = None;
        }
      }

      markAsObjC(*this, proto, isObjC);
    }

    // FIXME: IRGen likes to emit @objc protocol descriptors even if the
    // protocol comes from a different module or translation unit.
    //
    // It would be nice if it didn't have to do that, then we could remove
    // this case.
    if (proto->isObjC())
      requestNominalLayout(proto);

    break;
  }

  case DeclKind::Param: {
    // FIXME: This case is hit when code completion occurs in a function
    // parameter list. Previous parameters are definitely in scope, but
    // we don't really know how to type-check them.
    // We can also hit this when code-completing in a closure body.
    //
    // FIXME: Also, note that we don't call setValidationStarted() here,
    // because the ExprCleanser clears the type of ParamDecls, so we
    // can end up here multiple times for the same ParamDecl.
    auto *PD = cast<ParamDecl>(D);
    if (!PD->hasInterfaceType())
      PD->markInvalid();

    break;
  }

  case DeclKind::Var: {
    auto *VD = cast<VarDecl>(D);
    auto *PBD = VD->getParentPatternBinding();

    // Note that we need to handle the fact that some VarDecls don't
    // have a PatternBindingDecl, for example the iterator in a
    // 'for ... in ...' loop.
    if (PBD == nullptr) {
      if (!VD->hasInterfaceType()) {
        VD->setValidationStarted();
        VD->markInvalid();
      }

      break;
    }

    // If we're already checking our PatternBindingDecl, bail out
    // without setting our own 'is being validated' flag, since we
    // will attempt validation again later.
    if (PBD->isBeingValidated())
      return;

    D->setIsBeingValidated();

    if (!VD->hasInterfaceType()) {
      // Attempt to infer the type using initializer expressions.
      validatePatternBindingEntries(*this, PBD);

      auto parentPattern = VD->getParentPattern();
      if (PBD->isInvalid() || !parentPattern->hasType()) {
        parentPattern->setType(ErrorType::get(Context));
        setBoundVarsTypeError(parentPattern, Context);
      }

      // Should have set a type above.
      assert(VD->hasInterfaceType());
    }

    // We're not really done with processing the signature yet, but
    // @objc checking requires the declaration to call itself validated
    // so that it can be considered as a witness.
    D->setIsBeingValidated(false);

    checkDeclAttributesEarly(VD);
    validateAttributes(*this, VD);

    if (!DeclChecker::checkOverrides(*this, VD)) {
      // If a property has an override attribute but does not override
      // anything, complain.
      auto overridden = VD->getOverriddenDecl();
      if (auto *OA = VD->getAttrs().getAttribute<OverrideAttr>()) {
        if (!overridden) {
          diagnose(VD, diag::property_does_not_override)
            .highlight(OA->getLocation());
          OA->setInvalid();
        }
      }
    }

    // Properties need some special validation logic.
    if (auto *nominalDecl = VD->getDeclContext()
            ->getAsNominalTypeOrNominalTypeExtensionContext()) {
      // If this is a property, check if it needs to be exposed to
      // Objective-C.
      Optional<ObjCReason> isObjC = shouldMarkAsObjC(*this, VD);

      if (isObjC && !isRepresentableInObjC(VD, *isObjC))
        isObjC = None;

      markAsObjC(*this, VD, isObjC);

      // Under the Swift 3 inference rules, if we have @IBInspectable or
      // @GKInspectable but did not infer @objc, warn that the attribute is
      if (!isObjC && Context.LangOpts.EnableSwift3ObjCInference) {
        if (auto attr = VD->getAttrs().getAttribute<IBInspectableAttr>()) {
          diagnose(attr->getLocation(),
                   diag::attribute_meaningless_when_nonobjc,
                   attr->getAttrName())
            .fixItRemove(attr->getRange());
        }

        if (auto attr = VD->getAttrs().getAttribute<GKInspectableAttr>()) {
          diagnose(attr->getLocation(),
                   diag::attribute_meaningless_when_nonobjc,
                   attr->getAttrName())
            .fixItRemove(attr->getRange());
        }
      }

      // If this variable is a class member, mark it final if the
      // class is final, or if it was declared with 'let'.
      auto staticSpelling =
          VD->getParentPatternBinding()->getStaticSpelling();
      inferFinalAndDiagnoseIfNeeded(*this, VD, staticSpelling);

      if (VD->isLet() && isa<ClassDecl>(nominalDecl)) {
        makeFinal(Context, VD);

        if (VD->getFormalAccess() == AccessLevel::Open) {
          auto diagID = diag::implicitly_final_cannot_be_open;
          if (!Context.isSwiftVersionAtLeast(5))
            diagID = diag::implicitly_final_cannot_be_open_swift4;
          auto inFlightDiag =
              diagnose(D, diagID,
                       static_cast<unsigned>(ImplicitlyFinalReason::Let));
          fixItAccess(inFlightDiag, D, AccessLevel::Public);
        }
      }

      // Infer 'dynamic' after 'final' but before touching accessors.
      inferDynamic(Context, VD);
    }

    // Perform accessor-related validation.
    validateAbstractStorageDecl(*this, VD);

    // Synthesize accessors as necessary.
    maybeAddAccessorsToVariable(VD, *this);

    break;
  }

  case DeclKind::Func:
  case DeclKind::Accessor: {
    auto *FD = cast<FuncDecl>(D);
    assert(!FD->hasInterfaceType());

    // Bail out if we're in a recursive validation situation.
    if (auto accessor = dyn_cast<AccessorDecl>(FD)) {
      auto *storage = accessor->getStorage();
      validateDecl(storage);
      if (!storage->hasValidSignature())
        return;
    }

    checkDeclAttributesEarly(FD);
    computeAccessLevel(FD);

    FD->setIsBeingValidated();

    // Bind operator functions to the corresponding operator declaration.
    if (FD->isOperator())
      bindFuncDeclToOperator(*this, FD);

    // Validate 'static'/'class' on functions in extensions.
    auto StaticSpelling = FD->getStaticSpelling();
    if (StaticSpelling != StaticSpellingKind::None &&
        FD->getDeclContext()->isExtensionContext()) {
      if (auto *NTD = FD->getDeclContext()
              ->getAsNominalTypeOrNominalTypeExtensionContext()) {
        if (!isa<ClassDecl>(NTD)) {
          if (StaticSpelling == StaticSpellingKind::KeywordClass) {
            diagnose(FD, diag::class_func_not_in_class)
                .fixItReplace(FD->getStaticLoc(), "static");
            diagnose(NTD, diag::extended_type_declared_here);
          }
        }
      }
    }

    validateSelfAccessKind(*this, FD);

    // Check whether the return type is dynamic 'Self'.
    if (checkDynamicSelfReturn(*this, FD))
      FD->setInvalid();

    // Accessors should pick up various parts of their type signatures
    // directly from the storage declaration instead of re-deriving them.
    // FIXME: should this include the generic signature?
    if (auto accessor = dyn_cast<AccessorDecl>(FD)) {
      auto storage = accessor->getStorage();

      // Note that it's important for correctness that we're filling in
      // empty TypeLocs, because otherwise revertGenericFuncSignature might
      // erase the types we set, causing them to be re-validated in a later
      // pass.  That later validation might be incorrect even if the TypeLocs
      // are a clone of the type locs from which we derived the value type,
      // because the rules for interpreting types in parameter contexts
      // are sometimes different from the rules elsewhere; for example,
      // function types default to non-escaping.

      auto valueParams =
        accessor->getParameterList(accessor->getParent()->isTypeContext());

      // Determine the value type.
      Type valueIfaceTy, valueTy;
      if (auto VD = dyn_cast<VarDecl>(storage)) {
        valueIfaceTy = VD->getInterfaceType()->getReferenceStorageReferent();
        valueTy = VD->getType()->getReferenceStorageReferent();
      } else {
        auto SD = cast<SubscriptDecl>(storage);
        valueIfaceTy = SD->getElementInterfaceType();
        valueTy = SD->mapTypeIntoContext(valueIfaceTy);

        // Copy the index types instead of re-validating them.
        auto indices = SD->getIndices();
        for (size_t i = 0, e = indices->size(); i != e; ++i) {
          auto subscriptParam = indices->get(i);
          if (!subscriptParam->hasInterfaceType())
            continue;

          Type paramIfaceTy = subscriptParam->getInterfaceType();
          Type paramTy = SD->mapTypeIntoContext(paramIfaceTy);

          auto accessorParam = valueParams->get(valueParams->size() - e + i);
          accessorParam->setType(paramTy);
          accessorParam->setInterfaceType(paramIfaceTy);
          accessorParam->getTypeLoc().setType(paramTy);
        }
      }

      // Propagate the value type into the correct position.
      switch (accessor->getAccessorKind()) {
      // For getters, set the result type to the value type.
      case AccessorKind::IsGetter:
        accessor->getBodyResultTypeLoc().setType(valueIfaceTy, true);
        break;

      // For setters and observers, set the old/new value parameter's type
      // to the value type.
      case AccessorKind::IsDidSet:
      case AccessorKind::IsWillSet:
      case AccessorKind::IsSetter: {
        auto newValueParam = valueParams->get(0);
        newValueParam->setType(valueTy);
        newValueParam->setInterfaceType(valueIfaceTy);
        newValueParam->getTypeLoc().setType(valueTy);
        break;
      }

      // Addressor result types can get complicated because of the owner.
      case AccessorKind::IsAddressor:
      case AccessorKind::IsMutableAddressor:
        if (Type resultType =
              buildAddressorResultType(*this, accessor, valueIfaceTy)) {
          accessor->getBodyResultTypeLoc().setType(resultType, true);
        }
        break;

      // These don't mention the value types directly.
      case AccessorKind::IsMaterializeForSet:
        break;
      }
    }

    // Before anything else, set up the 'self' argument correctly if present.
    if (FD->getDeclContext()->isTypeContext())
      configureImplicitSelf(*this, FD);

    // If we have generic parameters, check the generic signature now.
    if (auto gp = FD->getGenericParams()) {
      gp->setOuterParameters(FD->getDeclContext()->getGenericParamsOfContext());

      auto *sig = validateGenericFuncSignature(FD);

      GenericEnvironment *env;
      if (auto AD = dyn_cast<AccessorDecl>(FD)) {
        env = cast<SubscriptDecl>(AD->getStorage())->getGenericEnvironment();
        assert(env && "accessor has generics but subscript is not generic");
      } else {
        env = sig->createGenericEnvironment();
      }
      FD->setGenericEnvironment(env);

      // Revert the types within the signature so it can be type-checked with
      // archetypes below.
      revertGenericFuncSignature(FD);
    } else if (auto genericSig =
                 FD->getDeclContext()->getGenericSignatureOfContext()) {
      if (!isa<AccessorDecl>(FD)) {
        (void)validateGenericFuncSignature(FD);

        // Revert all of the types within the signature of the function.
        revertGenericFuncSignature(FD);
      } else {
        // We've inherited all of the type information already.
        configureInterfaceType(FD, genericSig);
      }

      FD->setGenericEnvironment(
          FD->getDeclContext()->getGenericEnvironmentOfContext());
    }

    // Set the context type of 'self'.
    if (FD->getDeclContext()->isTypeContext())
      recordSelfContextType(FD);

    // Type check the parameters and return type again, now with archetypes.
    GenericTypeToArchetypeResolver resolver(FD);

    bool badType = false;
    if (!FD->getBodyResultTypeLoc().isNull()) {
      TypeResolutionOptions options = TypeResolutionFlags::AllowIUO;
      if (FD->hasDynamicSelf())
        options |= TypeResolutionFlags::DynamicSelfResult;

      if (validateType(FD->getBodyResultTypeLoc(), FD, options,
                       &resolver)) {
        badType = true;
      }
    }

    badType |= typeCheckParameterLists(FD, resolver);

    if (badType) {
      FD->setInterfaceType(ErrorType::get(Context));
      FD->setInvalid();
      FD->setIsBeingValidated(false);
      break;
    }

    if (!isa<AccessorDecl>(FD) || cast<AccessorDecl>(FD)->isGetter()) {
      auto *TyR = getTypeLocForFunctionResult(FD).getTypeRepr();
      if (TyR && TyR->getKind() == TypeReprKind::ImplicitlyUnwrappedOptional) {
        auto &C = FD->getASTContext();
        FD->getAttrs().add(
            new (C) ImplicitlyUnwrappedOptionalAttr(/* implicit= */ true));
      }
    }

    if (!FD->getGenericSignatureOfContext())
      configureInterfaceType(FD, FD->getGenericSignature());

    // We want the function to be available for name lookup as soon
    // as it has a valid interface type.
    FD->setIsBeingValidated(false);

    if (FD->isInvalid())
      break;

    validateAttributes(*this, FD);

    // Member functions need some special validation logic.
    if (FD->getDeclContext()->isTypeContext()) {
      if (!checkOverrides(*this, FD)) {
        // If a method has an 'override' keyword but does not
        // override anything, complain.
        if (auto *OA = FD->getAttrs().getAttribute<OverrideAttr>()) {
          if (!FD->getOverriddenDecl()) {
            diagnose(FD, diag::method_does_not_override)
              .highlight(OA->getLocation());
            OA->setInvalid();
          }
        }
      }

      if (FD->isOperator())
        checkMemberOperator(*this, FD);

      Optional<ObjCReason> isObjC = shouldMarkAsObjC(*this, FD);
      auto accessor = dyn_cast<AccessorDecl>(FD);

      auto *protocolContext = dyn_cast<ProtocolDecl>(
          FD->getDeclContext());
      if (protocolContext && accessor) {
        if (isObjC)
          isObjC = ObjCReason::Accessor;
      }

      if (accessor && accessor->isGetterOrSetter()) {
        // If the property decl is an instance property, its accessors will
        // be instance methods and the above condition will mark them ObjC.
        // The only additional condition we need to check is if the var decl
        // had an @objc or @iboutlet property.

        AbstractStorageDecl *storage = accessor->getStorage();
        // Validate the subscript or property because it might not be type
        // checked yet.
        validateDecl(storage);

        if (storage->getAttrs().hasAttribute<NonObjCAttr>())
          isObjC = None;
        else if (storage->isObjC()) {
          if (!isObjC) {
            // Make this accessor @objc because its property is @objc.
            isObjC = ObjCReason::Accessor;
          } else {
            // If @objc on the storage declaration was inferred using a
            // deprecated rule, but this accessor is @objc in its own right,
            // complain.
            auto storageObjCAttr = storage->getAttrs().getAttribute<ObjCAttr>();
            if (storageObjCAttr->isSwift3Inferred() &&
                shouldDiagnoseObjCReason(*isObjC, Context)) {
              diagnose(storage, diag::accessor_swift3_objc_inference,
                       storage->getDescriptiveKind(), storage->getFullName(),
                       isa<SubscriptDecl>(storage), accessor->isSetter())
                .fixItInsert(storage->getAttributeInsertionLoc(
                                                      /*forModifier=*/false),
                             "@objc ");
            }
          }
        }

        // If the storage is dynamic or final, propagate to this accessor.
        if (isObjC &&
            storage->isDynamic())
          makeDynamic(Context, FD);

        if (storage->isFinal())
          makeFinal(Context, FD);
      }

      Optional<ForeignErrorConvention> errorConvention;
      if (isObjC &&
          (FD->isInvalid() || !isRepresentableInObjC(FD, *isObjC,
                                                     errorConvention)))
        isObjC = None;
      markAsObjC(*this, FD, isObjC, errorConvention);

      inferFinalAndDiagnoseIfNeeded(*this, FD, FD->getStaticSpelling());
      inferDynamic(Context, FD);
    }

    // If the function is exported to C, it must be representable in (Obj-)C.
    if (auto CDeclAttr = FD->getAttrs().getAttribute<swift::CDeclAttr>()) {
      Optional<ForeignErrorConvention> errorConvention;
      if (isRepresentableInObjC(FD, ObjCReason::ExplicitlyCDecl,
                                errorConvention)) {
        if (FD->hasThrows()) {
          FD->setForeignErrorConvention(*errorConvention);
          diagnose(CDeclAttr->getLocation(), diag::cdecl_throws);
        }
      }
    }

    checkDeclAttributes(FD);

    break;
  }

  case DeclKind::Constructor: {
    auto *CD = cast<ConstructorDecl>(D);

    CD->setIsBeingValidated();

    checkDeclAttributesEarly(CD);
    computeAccessLevel(CD);

    // convenience initializers are only allowed on classes and in
    // extensions thereof.
    if (CD->isConvenienceInit()) {
      if (auto extType = CD->getDeclContext()->getDeclaredInterfaceType()) {
        auto extClass = extType->getClassOrBoundGenericClass();

        // Forbid convenience inits on Foreign CF types, as Swift does not yet
        // support user-defined factory inits.
        if (extClass &&
            extClass->getForeignClassKind() == ClassDecl::ForeignKind::CFType) {
          diagnose(CD->getLoc(), diag::cfclass_convenience_init);
        }

        if (!extClass && !extType->hasError()) {
          auto ConvenienceLoc =
            CD->getAttrs().getAttribute<ConvenienceAttr>()->getLocation();

          // Produce a tailored diagnostic for structs and enums.
          bool isStruct = extType->getStructOrBoundGenericStruct() != nullptr;
          if (isStruct || extType->getEnumOrBoundGenericEnum()) {
            diagnose(CD->getLoc(), diag::enumstruct_convenience_init,
                     isStruct ? "structs" : "enums")
              .fixItRemove(ConvenienceLoc);
          } else {
            diagnose(CD->getLoc(), diag::nonclass_convenience_init, extType)
              .fixItRemove(ConvenienceLoc);
          }
          CD->setInitKind(CtorInitializerKind::Designated);
        }
      }
    } else if (auto extType = CD->getDeclContext()->getDeclaredInterfaceType()) {
      // A designated initializer for a class must be written within the class
      // itself.
      //
      // This is because designated initializers of classes get a vtable entry,
      // and extensions cannot add vtable entries to the extended type.
      //
      // If we implement the ability for extensions defined in the same module
      // (or the same file) to add vtable entries, we can re-evaluate this
      // restriction.
      if (extType->getClassOrBoundGenericClass() &&
          isa<ExtensionDecl>(CD->getDeclContext())) {
        diagnose(CD->getLoc(), diag::designated_init_in_extension, extType)
          .fixItInsert(CD->getLoc(), "convenience ");
        CD->setInitKind(CtorInitializerKind::Convenience);
      } else if (CD->getDeclContext()->getAsProtocolExtensionContext()) {
        CD->setInitKind(CtorInitializerKind::Convenience);
      }
    }

    if (CD->getDeclContext()->isTypeContext())
      configureImplicitSelf(*this, CD);

    if (auto gp = CD->getGenericParams()) {
      // Write up generic parameters and check the generic parameter list.
      gp->setOuterParameters(CD->getDeclContext()->getGenericParamsOfContext());

      auto *sig = validateGenericFuncSignature(CD);
      auto *env = sig->createGenericEnvironment();
      CD->setGenericEnvironment(env);

      // Revert the types within the signature so it can be type-checked with
      // archetypes below.
      revertGenericFuncSignature(CD);
    } else if (CD->getDeclContext()->getGenericSignatureOfContext()) {
      (void)validateGenericFuncSignature(CD);

      // Revert all of the types within the signature of the constructor.
      revertGenericFuncSignature(CD);

      CD->setGenericEnvironment(
          CD->getDeclContext()->getGenericEnvironmentOfContext());
    }

    // Set the context type of 'self'.
    if (CD->getDeclContext()->isTypeContext())
      recordSelfContextType(CD);

    // Type check the constructor parameters.
    GenericTypeToArchetypeResolver resolver(CD);
    if (typeCheckParameterLists(CD, resolver) || CD->isInvalid()) {
      CD->setInterfaceType(ErrorType::get(Context));
      CD->setInvalid();
    } else {
      if (!CD->getGenericSignatureOfContext())
        configureInterfaceType(CD, CD->getGenericSignature());
    }

    // We want the constructor to be available for name lookup as soon
    // as it has a valid interface type.
    CD->setIsBeingValidated(false);

    validateAttributes(*this, CD);

    // Check whether this initializer overrides an initializer in its
    // superclass.
    if (!checkOverrides(*this, CD)) {
      // If an initializer has an override attribute but does not override
      // anything or overrides something that doesn't need an 'override'
      // keyword (e.g., a convenience initializer), complain.
      // anything, or overrides something that complain.
      if (auto *attr = CD->getAttrs().getAttribute<OverrideAttr>()) {
        if (!CD->getOverriddenDecl()) {
          diagnose(CD, diag::initializer_does_not_override)
            .highlight(attr->getLocation());
          attr->setInvalid();
        } else if (!DeclChecker::overrideRequiresKeyword(CD->getOverriddenDecl())) {
          // Special case: we are overriding a 'required' initializer, so we
          // need (only) the 'required' keyword.
          if (cast<ConstructorDecl>(CD->getOverriddenDecl())->isRequired()) {
            if (CD->getAttrs().hasAttribute<RequiredAttr>()) {
              diagnose(CD, diag::required_initializer_override_keyword)
                .fixItRemove(attr->getLocation());
            } else {
              diagnose(CD, diag::required_initializer_override_wrong_keyword)
                .fixItReplace(attr->getLocation(), "required");
              CD->getAttrs().add(
                new (Context) RequiredAttr(/*IsImplicit=*/true));
            }

            diagnose(findNonImplicitRequiredInit(CD->getOverriddenDecl()),
                     diag::overridden_required_initializer_here);
          } else {
            // We tried to override a convenience initializer.
            diagnose(CD, diag::initializer_does_not_override)
              .highlight(attr->getLocation());
            diagnose(CD->getOverriddenDecl(),
                     diag::convenience_init_override_here);
          }
        }
      }

      // A failable initializer cannot override a non-failable one.
      // This would normally be diagnosed by the covariance rules;
      // however, those are disabled so that we can provide a more
      // specific diagnostic here.
      if (CD->getFailability() != OTK_None &&
          CD->getOverriddenDecl() &&
          CD->getOverriddenDecl()->getFailability() == OTK_None) {
        diagnose(CD, diag::failable_initializer_override,
                 CD->getFullName());
        diagnose(CD->getOverriddenDecl(),
                 diag::nonfailable_initializer_override_here,
                 CD->getOverriddenDecl()->getFullName());
      }
    }

    // An initializer is ObjC-compatible if it's explicitly @objc or a member
    // of an ObjC-compatible class.
    if (CD->getDeclContext()->isTypeContext()) {
      Optional<ObjCReason> isObjC = shouldMarkAsObjC(*this, CD,
          /*allowImplicit=*/true);

      Optional<ForeignErrorConvention> errorConvention;
      if (isObjC &&
          (CD->isInvalid() ||
           !isRepresentableInObjC(CD, *isObjC, errorConvention)))
        isObjC = None;
      markAsObjC(*this, CD, isObjC, errorConvention);
    }

    inferDynamic(Context, CD);

    if (CD->getFailability() == OTK_ImplicitlyUnwrappedOptional) {
      auto &C = CD->getASTContext();
      CD->getAttrs().add(
          new (C) ImplicitlyUnwrappedOptionalAttr(/* implicit= */ true));
    }

    break;
  }

  case DeclKind::Destructor: {
    auto *DD = cast<DestructorDecl>(D);

    auto enclosingClass = dyn_cast<ClassDecl>(DD->getDeclContext());
    if (DD->isInvalid() ||
        enclosingClass == nullptr) {
      DD->setInterfaceType(ErrorType::get(Context));
      DD->setInvalid();
      return;
    }

    DD->setIsBeingValidated();

    assert(DD->getDeclContext()->isTypeContext()
           && "Decl parsing must prevent destructors outside of types!");

    checkDeclAttributesEarly(DD);
    DD->copyFormalAccessFrom(enclosingClass, /*sourceIsParentContext*/true);

    configureImplicitSelf(*this, DD);

    if (DD->getDeclContext()->getGenericSignatureOfContext()) {
      (void)validateGenericFuncSignature(DD);
      DD->setGenericEnvironment(
          DD->getDeclContext()->getGenericEnvironmentOfContext());
    }

    // Set the context type of 'self'.
    recordSelfContextType(DD);

    GenericTypeToArchetypeResolver resolver(DD);
    if (typeCheckParameterLists(DD, resolver)) {
      DD->setInterfaceType(ErrorType::get(Context));
      DD->setInvalid();
    }

    if (!DD->getGenericSignatureOfContext())
      configureInterfaceType(DD, DD->getGenericSignature());

    DD->setIsBeingValidated(false);

    // Do this before markAsObjC() to diagnose @nonobjc better
    validateAttributes(*this, DD);

    // Destructors are always @objc, because their Objective-C entry point is
    // -dealloc.
    markAsObjC(*this, DD, ObjCReason::ImplicitlyObjC);

    break;
  }

  case DeclKind::Subscript: {
    auto *SD = cast<SubscriptDecl>(D);

    SD->setIsBeingValidated();

    auto dc = SD->getDeclContext();

    if (auto gp = SD->getGenericParams()) {
      // Write up generic parameters and check the generic parameter list.
      gp->setOuterParameters(dc->getGenericParamsOfContext());

      auto *sig = validateGenericSubscriptSignature(SD);
      auto *env = sig->createGenericEnvironment();
      SD->setGenericEnvironment(env);

      // Revert the types within the signature so it can be type-checked with
      // archetypes below.
      revertGenericSubscriptSignature(SD);
    } else if (dc->getGenericSignatureOfContext()) {
      (void)validateGenericSubscriptSignature(SD);

      // Revert all of the types within the signature of the subscript.
      revertGenericSubscriptSignature(SD);

      SD->setGenericEnvironment(
          SD->getDeclContext()->getGenericEnvironmentOfContext());
    }

    // Type check the subscript parameters.
    GenericTypeToArchetypeResolver resolver(SD);

    bool isInvalid = validateType(SD->getElementTypeLoc(), SD,
                                  TypeResolutionFlags::AllowIUO,
                                  &resolver);
    TypeResolutionOptions options;
    options |= TypeResolutionFlags::SubscriptParameters;

    isInvalid |= typeCheckParameterList(SD->getIndices(), SD,
                                        options,
                                        resolver);

    if (isInvalid || SD->isInvalid()) {
      SD->setInterfaceType(ErrorType::get(Context));
      SD->setInvalid();
    } else {
      if (!SD->getGenericSignatureOfContext())
        configureInterfaceType(SD, SD->getGenericSignature());
    }

    SD->setIsBeingValidated(false);

    checkDeclAttributesEarly(SD);
    computeAccessLevel(SD);

    validateAttributes(*this, SD);

    auto *TyR = SD->getElementTypeLoc().getTypeRepr();
    if (TyR && TyR->getKind() == TypeReprKind::ImplicitlyUnwrappedOptional) {
      auto &C = SD->getASTContext();
      SD->getAttrs().add(
          new (C) ImplicitlyUnwrappedOptionalAttr(/* implicit= */ true));
    }

    if (!checkOverrides(*this, SD)) {
      // If a subscript has an override attribute but does not override
      // anything, complain.
      if (auto *OA = SD->getAttrs().getAttribute<OverrideAttr>()) {
        if (!SD->getOverriddenDecl()) {
          diagnose(SD, diag::subscript_does_not_override)
              .highlight(OA->getLocation());
          OA->setInvalid();
        }
      }
    }

    // Member subscripts need some special validation logic.
    if (dc->isTypeContext()) {
      // If this is a class member, mark it final if the class is final.
      inferFinalAndDiagnoseIfNeeded(*this, SD, StaticSpellingKind::None);

      // A subscript is ObjC-compatible if it's explicitly @objc, or a
      // member of an ObjC-compatible class or protocol.
      Optional<ObjCReason> isObjC = shouldMarkAsObjC(*this, SD);

      if (isObjC && !isRepresentableInObjC(SD, *isObjC))
        isObjC = None;
      markAsObjC(*this, SD, isObjC);

      // Infer 'dynamic' before touching accessors.
      inferDynamic(Context, SD);
    }

    // Perform accessor-related validation.
    validateAbstractStorageDecl(*this, SD);

    // If this is a get+mutableAddress property, synthesize the setter body.
    if (SD->getStorageKind() == SubscriptDecl::ComputedWithMutableAddress &&
        !SD->getSetter()->getBody()) {
      synthesizeSetterForMutableAddressedStorage(SD, *this);
    }

    break;
  }

  case DeclKind::EnumElement: {
    auto *EED = cast<EnumElementDecl>(D);

    checkDeclAttributesEarly(EED);
    validateAccessControl(EED);

    validateAttributes(*this, EED);

    EED->setIsBeingValidated(true);

    if (auto *PL = EED->getParameterList()) {
      GenericTypeToArchetypeResolver resolver(EED->getParentEnum());

      bool isInvalid
        = typeCheckParameterList(PL, EED->getParentEnum(),
                                 TypeResolutionFlags::EnumCase, resolver);

      if (isInvalid || EED->isInvalid()) {
        EED->setInterfaceType(ErrorType::get(Context));
        EED->setInvalid();
      } else {
        checkDefaultArguments(PL, EED);
      }
    }

    // If we have a raw value, make sure there's a raw type as well.
    if (auto *rawValue = EED->getRawValueExpr()) {
      EnumDecl *ED = EED->getParentEnum();
      if (!ED->hasRawType()) {
        diagnose(rawValue->getLoc(),diag::enum_raw_value_without_raw_type);
        // Recover by setting the raw type as this element's type.
        Expr *typeCheckedExpr = rawValue;
        if (!typeCheckExpression(typeCheckedExpr, ED)) {
          EED->setTypeCheckedRawValueExpr(typeCheckedExpr);
          checkEnumElementErrorHandling(EED);
        }
      } else {
        // Wait until the second pass, when all the raw value expressions
        // can be checked together.
      }
    }

    EED->setIsBeingValidated(false);

    // Now that we have an argument type we can set the element's declared
    // type.
    if (!EED->hasInterfaceType() && !EED->computeType())
      break;

    // Require the carried type to be materializable.
    if (auto argTy = EED->getArgumentInterfaceType()) {
      assert(!argTy->hasLValueType() && "enum element cannot carry @lvalue");

      if (!argTy->isMaterializable()) {
        diagnose(EED->getLoc(), diag::enum_element_not_materializable, argTy);
        EED->setInterfaceType(ErrorType::get(Context));
        EED->setInvalid();
      }
    }

    break;
  }
  }

  assert(D->hasValidSignature());
}

void TypeChecker::validateDeclForNameLookup(ValueDecl *D) {
  // Validate the context.
  auto dc = D->getDeclContext();
  if (auto nominal = dyn_cast<NominalTypeDecl>(dc)) {
    validateDeclForNameLookup(nominal);
    if (!nominal->hasInterfaceType())
      return;
  } else if (auto ext = dyn_cast<ExtensionDecl>(dc)) {
    validateExtension(ext);
    if (!ext->hasValidSignature())
      return;
  }

  switch (D->getKind()) {
  case DeclKind::Protocol: {
    auto proto = cast<ProtocolDecl>(D);
    if (proto->hasInterfaceType())
      return;
    proto->computeType();

    auto *gp = proto->getGenericParams();
    unsigned depth = gp->getDepth();
    for (auto paramDecl : *gp)
      paramDecl->setDepth(depth);

    validateAccessControl(proto);

    // Record inherited protocols.
    resolveInheritedProtocols(proto);

    for (auto ATD : proto->getAssociatedTypeMembers()) {
      validateDeclForNameLookup(ATD);
    }

    // Compute the requirement signature later to avoid circularity.
    DelayedRequirementSignatures.insert(proto);

    // FIXME: IRGen likes to emit @objc protocol descriptors even if the
    // protocol comes from a different module or translation unit.
    //
    // It would be nice if it didn't have to do that, then we could remove
    // this case.
    if (proto->isObjC())
      requestNominalLayout(proto);

    break;
  }
  case DeclKind::AssociatedType: {
    auto assocType = cast<AssociatedTypeDecl>(D);
    if (assocType->hasInterfaceType())
      return;
    assocType->computeType();
    validateAccessControl(assocType);
    break;
  }
  case DeclKind::TypeAlias: {
    auto typealias = cast<TypeAliasDecl>(D);
    if (typealias->getUnderlyingTypeLoc().getType())
      return;

    // Perform earlier validation of typealiases in protocols.
    if (isa<ProtocolDecl>(dc)) {
      if (!typealias->getGenericParams()) {
        if (typealias->isBeingValidated()) return;

        typealias->setIsBeingValidated();
        SWIFT_DEFER { typealias->setIsBeingValidated(false); };

        validateAccessControl(typealias);

        ProtocolRequirementTypeResolver resolver;
        if (validateType(typealias->getUnderlyingTypeLoc(),
                         typealias, TypeResolutionOptions(), &resolver)) {
          typealias->setInvalid();
          typealias->getUnderlyingTypeLoc().setInvalidType(Context);
        }

        typealias->setUnderlyingType(
                                typealias->getUnderlyingTypeLoc().getType());

        // Note that this doesn't set the generic environment of the alias yet,
        // because we haven't built one for the protocol.
        //
        // See how validateDecl() sets the generic environment on alias members
        // explicitly.
        //
        // FIXME: Hopefully this can all go away with the ITC.
        return;
      }
    }
    LLVM_FALLTHROUGH;
  }

  default:
    validateDecl(D);
    break;
  }
}

static bool shouldValidateMemberDuringFinalization(NominalTypeDecl *nominal,
                                                   ValueDecl *VD) {
  // For enums, we only need to validate enum elements to know
  // the layout.
  if (isa<EnumDecl>(nominal) &&
      isa<EnumElementDecl>(VD))
    return true;

  // For structs, we only need to validate stored properties to
  // know the layout.
  if (isa<StructDecl>(nominal) &&
      (isa<VarDecl>(VD) &&
       !cast<VarDecl>(VD)->isStatic() &&
       (cast<VarDecl>(VD)->hasStorage() ||
        VD->getAttrs().hasAttribute<LazyAttr>())))
    return true;

  // For classes, we need to validate properties and functions,
  // but skipping nested types is OK.
  if (isa<ClassDecl>(nominal) &&
      !isa<TypeDecl>(VD))
    return true;

  // For protocols, skip nested typealiases and nominal types.
  if (isa<ProtocolDecl>(nominal) &&
      !isa<GenericTypeDecl>(VD))
    return true;

  return false;
}

void TypeChecker::requestMemberLayout(ValueDecl *member) {
  auto *dc = member->getDeclContext();
  if (auto *classDecl = dyn_cast<ClassDecl>(dc))
    requestNominalLayout(classDecl);
  if (auto *protocolDecl = dyn_cast<ProtocolDecl>(dc))
    requestNominalLayout(protocolDecl);
}

void TypeChecker::requestNominalLayout(NominalTypeDecl *nominalDecl) {
  if (nominalDecl->hasValidatedLayout())
    return;

  nominalDecl->setHasValidatedLayout();

  if (isa<SourceFile>(nominalDecl->getModuleScopeContext()))
    DeclsToFinalize.insert(nominalDecl);
}

void TypeChecker::requestSuperclassLayout(ClassDecl *classDecl) {
  auto superclassTy = classDecl->getSuperclass();
  if (superclassTy) {
    auto *superclassDecl = superclassTy->getClassOrBoundGenericClass();
    if (superclassDecl)
      requestNominalLayout(superclassDecl);
  }
}

static void finalizeType(TypeChecker &TC, NominalTypeDecl *nominal) {
  assert(!nominal->hasClangNode());
  assert(isa<SourceFile>(nominal->getModuleScopeContext()));

  for (auto *D : nominal->getMembers()) {
    auto VD = dyn_cast<ValueDecl>(D);
    if (!VD)
      continue;

    if (!shouldValidateMemberDuringFinalization(nominal, VD))
      continue;

    TC.validateDecl(VD);

    // The only thing left to do is synthesize storage for lazy variables.
    auto *prop = dyn_cast<VarDecl>(D);
    if (!prop)
      continue;

    if (prop->getAttrs().hasAttribute<LazyAttr>() && !prop->isStatic()
                                                  && prop->getGetter()) {
      assert(!prop->getGetter()->hasBody());
      TC.completeLazyVarImplementation(prop);
    }
  }

  if (auto *CD = dyn_cast<ClassDecl>(nominal)) {
    // We need to add implicit initializers and dtors because it
    // affects vtable layout.
    TC.addImplicitConstructors(CD);
    CD->addImplicitDestructor();

    // We need the superclass vtable layout as well.
    TC.requestSuperclassLayout(CD);

    auto useConformance = [&](ProtocolDecl *protocol) {
      if (auto ref = TC.conformsToProtocol(
            CD->getDeclaredInterfaceType(), protocol, CD,
            ConformanceCheckFlags::SkipConditionalRequirements,
            SourceLoc())) {
        if (ref->getConcrete()->getDeclContext() == CD)
          TC.markConformanceUsed(*ref, CD);
      }
    };

    // If the class is Encodable, Decodable or Hashable, force those
    // conformances to ensure that the synthesized members appear in the vtable.
    //
    // FIXME: Generalize this to other protocols for which
    // we can derive conformances.
    useConformance(TC.Context.getProtocol(KnownProtocolKind::Decodable));
    useConformance(TC.Context.getProtocol(KnownProtocolKind::Encodable));
    useConformance(TC.Context.getProtocol(KnownProtocolKind::Hashable));
  }

  // validateDeclForNameLookup will not trigger an immediate full
  // validation of protocols, but clients will assume that things
  // like the requirement signature have been set.
  if (auto PD = dyn_cast<ProtocolDecl>(nominal)) {
    if (!PD->isRequirementSignatureComputed()) {
      TC.validateDecl(PD);
    }
  }
}

void TypeChecker::finalizeDecl(ValueDecl *decl) {
  if (auto nominal = dyn_cast<NominalTypeDecl>(decl)) {
    finalizeType(*this, nominal);
  } else if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
    // We synthesize certain functions --- mostly accessors --- at
    // times that can be inconvenient for immediate validation.  We add
    // them to the list of declarations to finalize so that we can
    // fully validate them at a more opportune time.
    validateDecl(func);
  } else {
    auto storage = cast<AbstractStorageDecl>(decl);
    finalizeAbstractStorageDecl(*this, storage);
  }
}

void TypeChecker::validateAccessControl(ValueDecl *D) {
  if (D->hasAccess())
    return;

  // FIXME: Encapsulate the following in computeAccessLevel() ?

  switch (D->getKind()) {
  case DeclKind::Import:
  case DeclKind::Extension:
  case DeclKind::PatternBinding:
  case DeclKind::EnumCase:
  case DeclKind::TopLevelCode:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
  case DeclKind::PrecedenceGroup:
  case DeclKind::IfConfig:
  case DeclKind::PoundDiagnostic:
  case DeclKind::MissingMember:
    llvm_unreachable("not a value decl");

  case DeclKind::Module:
    break;

  case DeclKind::TypeAlias:
    computeAccessLevel(D);
    break;

  case DeclKind::GenericTypeParam:
    // Ultimately handled in generic signature validation.
    return;

  case DeclKind::AssociatedType: {
      auto assocType = cast<AssociatedTypeDecl>(D);
      auto prot = assocType->getProtocol();
      validateAccessControl(prot);
      assocType->setAccess(std::max(prot->getFormalAccess(),
                                    AccessLevel::Internal));
      break;
    }

  case DeclKind::Enum:
  case DeclKind::Struct:
  case DeclKind::Class:
  case DeclKind::Protocol:
  case DeclKind::Var:
  case DeclKind::Param:
  case DeclKind::Func:
  case DeclKind::Accessor:
  case DeclKind::Subscript:
  case DeclKind::Constructor:
    computeAccessLevel(D);
    break;

  case DeclKind::Destructor:
  case DeclKind::EnumElement: {
    if (D->isInvalid()) {
      D->setAccess(AccessLevel::Private);
    } else {
      auto container = cast<NominalTypeDecl>(D->getDeclContext());
      validateAccessControl(container);
      D->setAccess(std::max(container->getFormalAccess(),
                            AccessLevel::Internal));
    }
    break;
  }
  }

  assert(D->hasAccess());
}

bool swift::isPassThroughTypealias(TypeAliasDecl *typealias) {
  // Pass-through only makes sense when the typealias refers to a nominal
  // type.
  Type underlyingType = typealias->getUnderlyingTypeLoc().getType();
  auto nominal = underlyingType->getAnyNominal();
  if (!nominal) return false;

  // Check that the nominal type and the typealias are either both generic
  // at this level or neither are.
  if (nominal->isGeneric() != typealias->isGeneric())
    return false;

  // Make sure either both have generic signatures or neither do.
  auto nominalSig = nominal->getGenericSignature();
  auto typealiasSig = typealias->getGenericSignature();
  if (static_cast<bool>(nominalSig) != static_cast<bool>(typealiasSig))
    return false;

  // If neither is generic, we're done: it's a pass-through alias.
  if (!nominalSig) return true;

  // Check that the type parameters are the same the whole way through.
  auto nominalGenericParams = nominalSig->getGenericParams();
  auto typealiasGenericParams = typealiasSig->getGenericParams();
  if (nominalGenericParams.size() != typealiasGenericParams.size())
    return false;
  if (!std::equal(nominalGenericParams.begin(), nominalGenericParams.end(),
                  typealiasGenericParams.begin(),
                  [](GenericTypeParamType *gp1, GenericTypeParamType *gp2) {
                    return gp1->isEqual(gp2);
                  }))
    return false;

  // If neither is generic at this level, we have a pass-through typealias.
  if (!typealias->isGeneric()) return true;

  auto boundGenericType = underlyingType->getAs<BoundGenericType>();
  if (!boundGenericType) return false;

  // If our arguments line up with our innermost generic parameters, it's
  // a passthrough typealias.
  auto innermostGenericParams = typealiasSig->getInnermostGenericParams();
  auto boundArgs = boundGenericType->getGenericArgs();
  if (boundArgs.size() != innermostGenericParams.size())
    return false;

  return std::equal(boundArgs.begin(), boundArgs.end(),
                    innermostGenericParams.begin(),
                    [](Type arg, GenericTypeParamType *gp) {
                      return arg->isEqual(gp);
                    });
}

/// Form the interface type of an extension from the raw type and the
/// extension's list of generic parameters.
static Type formExtensionInterfaceType(TypeChecker &tc, ExtensionDecl *ext,
                                       Type type,
                                       GenericParamList *genericParams,
                                       bool &mustInferRequirements) {
  // Find the nominal type declaration and its parent type.
  Type parentType;
  GenericTypeDecl *genericDecl;
  if (auto unbound = type->getAs<UnboundGenericType>()) {
    parentType = unbound->getParent();
    genericDecl = unbound->getDecl();
  } else {
    if (type->is<ProtocolCompositionType>())
      type = type->getCanonicalType();
    auto nominalType = type->castTo<NominalType>();
    parentType = nominalType->getParent();
    genericDecl = nominalType->getDecl();
  }

  // Reconstruct the parent, if there is one.
  if (parentType) {
    // Build the nested extension type.
    auto parentGenericParams = genericDecl->getGenericParams()
                                 ? genericParams->getOuterParameters()
                                 : genericParams;
    parentType =
      formExtensionInterfaceType(tc, ext, parentType, parentGenericParams,
                                 mustInferRequirements);
  }

  // Find the nominal type.
  auto nominal = dyn_cast<NominalTypeDecl>(genericDecl);
  auto typealias = dyn_cast<TypeAliasDecl>(genericDecl);
  if (!nominal) {
    Type underlyingType = typealias->getUnderlyingTypeLoc().getType();
    nominal = underlyingType->getNominalOrBoundGenericNominal();
  }

  // Form the result.
  Type resultType;
  SmallVector<Type, 2> genericArgs;
  if (!nominal->isGeneric() || isa<ProtocolDecl>(nominal)) {
    resultType = NominalType::get(nominal, parentType,
                                  nominal->getASTContext());
  } else {
    // Form the bound generic type with the type parameters provided.
    for (auto gp : *genericParams) {
      genericArgs.push_back(gp->getDeclaredInterfaceType());
    }

    resultType = BoundGenericType::get(nominal, parentType, genericArgs);
  }

  // If we have a typealias, try to form type sugar.
  if (typealias && isPassThroughTypealias(typealias)) {
    auto typealiasSig = typealias->getGenericSignature();
    SubstitutionMap subMap;
    if (typealiasSig) {
      subMap = typealiasSig->getSubstitutionMap(
                              [](SubstitutableType *type) -> Type {
                                return Type(type);
                              },
                              [](CanType dependentType,
                                 Type replacementType,
                                 ProtocolType *protoType) {
                                auto proto = protoType->getDecl();
                                return ProtocolConformanceRef(proto);
                              });

      mustInferRequirements = true;
    }

    resultType = NameAliasType::get(typealias, parentType, subMap,
                                         resultType);
  }

  return resultType;
}

/// Visit the given generic parameter lists from the outermost to the innermost,
/// calling the visitor function for each list.
static void visitOuterToInner(
                      GenericParamList *genericParams,
                      llvm::function_ref<void(GenericParamList *)> visitor) {
  if (auto outerGenericParams = genericParams->getOuterParameters())
    visitOuterToInner(outerGenericParams, visitor);

  visitor(genericParams);
}

/// Check the generic parameters of an extension, recursively handling all of
/// the parameter lists within the extension.
static std::pair<GenericEnvironment *, Type>
checkExtensionGenericParams(TypeChecker &tc, ExtensionDecl *ext, Type type,
                            GenericParamList *genericParams) {
  assert(!ext->getGenericEnvironment());

  // Form the interface type of the extension.
  bool mustInferRequirements = false;
  Type extInterfaceType =
    formExtensionInterfaceType(tc, ext, type, genericParams,
                               mustInferRequirements);

  // Prepare all of the generic parameter lists for generic signature
  // validation.
  visitOuterToInner(genericParams, [&](GenericParamList *gpList) {
    tc.prepareGenericParamList(gpList, ext);
  });

  // Local function used to infer requirements from the extended type.
  auto inferExtendedTypeReqs = [&](GenericSignatureBuilder &builder) {
    auto source =
      GenericSignatureBuilder::FloatingRequirementSource::forInferred(nullptr);

    builder.inferRequirements(*ext->getModuleContext(),
                              TypeLoc::withoutLoc(extInterfaceType),
                              source);
  };

  // Validate the generic type signature.
  auto *env = tc.checkGenericEnvironment(genericParams,
                                         ext->getDeclContext(), nullptr,
                                         /*allowConcreteGenericParams=*/true,
                                         ext, inferExtendedTypeReqs,
                                         mustInferRequirements);

  // Validate the generic parameters for the last time, to splat down
  // actual archetypes.
  visitOuterToInner(genericParams, [&](GenericParamList *gpList) {
    tc.revertGenericParamList(gpList);
  });
  GenericTypeToArchetypeResolver archetypeResolver(env);
  visitOuterToInner(genericParams, [&](GenericParamList *gpList) {
    tc.checkGenericParamList(nullptr, gpList, nullptr, &archetypeResolver);
  });

  Type extContextType =
    env->mapTypeIntoContext(extInterfaceType);
  return { env, extContextType };
}

void TypeChecker::validateExtension(ExtensionDecl *ext) {
  // If we're currently validating, or have already validated this extension,
  // there's nothing more to do now.
  if (ext->hasValidationStarted())
    return;

  ext->setIsBeingValidated();
  SWIFT_DEFER { ext->setIsBeingValidated(false); };

  // If the extension is already known to be invalid, we're done.
  if (ext->isInvalid())
    return;

  // FIXME: We need to check whether anything is specialized, because
  // the innermost extended type might itself be a non-generic type
  // within a generic type.
  auto extendedType = ext->getExtendedType();

  if (extendedType.isNull() || extendedType->hasError())
    return;

  // Validate the nominal type declaration being extended.
  NominalTypeDecl *nominal = extendedType->getAnyNominal();
  if (!nominal) {
    auto unbound = cast<UnboundGenericType>(extendedType.getPointer());
    auto typealias = cast<TypeAliasDecl>(unbound->getDecl());
    validateDecl(typealias);

    nominal = typealias->getUnderlyingTypeLoc().getType()->getAnyNominal();
  }
  validateDecl(nominal);

  if (nominal->getGenericParamsOfContext()) {
    auto genericParams = ext->getGenericParams();
    assert(genericParams && "bindExtensionDecl didn't set generic params?");

    // Check generic parameters.
    GenericEnvironment *env;
    std::tie(env, extendedType) = checkExtensionGenericParams(
        *this, ext, ext->getExtendedType(),
        genericParams);

    ext->getExtendedTypeLoc().setType(extendedType);
    ext->setGenericEnvironment(env);
    return;
  }

  assert(extendedType->is<NominalType>());
  assert(!nominal->isGenericContext());
}

llvm::TinyPtrVector<ProtocolDecl *>
TypeChecker::getDirectConformsTo(ProtocolDecl *proto) {
  resolveInheritedProtocols(proto);
  return proto->getInheritedProtocols();
}

/// Build a default initializer string for the given pattern.
///
/// This string is suitable for display in diagnostics.
static Optional<std::string> buildDefaultInitializerString(TypeChecker &tc,
                                                           DeclContext *dc,
                                                           Pattern *pattern) {
  switch (pattern->getKind()) {
#define REFUTABLE_PATTERN(Id, Parent) case PatternKind::Id:
#define PATTERN(Id, Parent)
#include "swift/AST/PatternNodes.def"
    return None;
  case PatternKind::Any:
    return None;

  case PatternKind::Named: {
    if (!pattern->hasType())
      return None;

    // Special-case the various types we might see here.
    auto type = pattern->getType();

    // For literal-convertible types, form the corresponding literal.
#define CHECK_LITERAL_PROTOCOL(Kind, String) \
    if (auto proto = tc.getProtocol(SourceLoc(), KnownProtocolKind::Kind)) { \
      if (tc.conformsToProtocol(type, proto, dc, \
                                ConformanceCheckFlags::InExpression)) \
        return std::string(String); \
    }
    CHECK_LITERAL_PROTOCOL(ExpressibleByArrayLiteral, "[]")
    CHECK_LITERAL_PROTOCOL(ExpressibleByDictionaryLiteral, "[:]")
    CHECK_LITERAL_PROTOCOL(ExpressibleByUnicodeScalarLiteral, "\"\"")
    CHECK_LITERAL_PROTOCOL(ExpressibleByExtendedGraphemeClusterLiteral, "\"\"")
    CHECK_LITERAL_PROTOCOL(ExpressibleByFloatLiteral, "0.0")
    CHECK_LITERAL_PROTOCOL(ExpressibleByIntegerLiteral, "0")
    CHECK_LITERAL_PROTOCOL(ExpressibleByStringLiteral, "\"\"")
#undef CHECK_LITERAL_PROTOCOL

    // For optional types, use 'nil'.
    if (type->getOptionalObjectType())
      return std::string("nil");

    return None;
  }

  case PatternKind::Paren: {
    if (auto sub = buildDefaultInitializerString(
                     tc, dc, cast<ParenPattern>(pattern)->getSubPattern())) {
      return "(" + *sub + ")";
    }

    return None;
  }

  case PatternKind::Tuple: {
    std::string result = "(";
    bool first = true;
    for (auto elt : cast<TuplePattern>(pattern)->getElements()) {
      if (auto sub = buildDefaultInitializerString(tc, dc, elt.getPattern())) {
        if (first) {
          first = false;
        } else {
          result += ", ";
        }

        result += *sub;
      } else {
        return None;
      }
    }
    result += ")";
    return result;
  }

  case PatternKind::Typed:
    return buildDefaultInitializerString(
             tc, dc, cast<TypedPattern>(pattern)->getSubPattern());

  case PatternKind::Var:
    return buildDefaultInitializerString(
             tc, dc, cast<VarPattern>(pattern)->getSubPattern());
  }

  llvm_unreachable("Unhandled PatternKind in switch.");
}

/// Diagnose a class that does not have any initializers.
static void diagnoseClassWithoutInitializers(TypeChecker &tc,
                                             ClassDecl *classDecl) {
  tc.diagnose(classDecl, diag::class_without_init,
              classDecl->getDeclaredType());

  // HACK: We've got a special case to look out for and diagnose specifically to
  // improve the experience of seeing this, and mitigate some confusion.
  //
  // For a class A which inherits from Decodable class B, class A may have
  // additional members which prevent default initializer synthesis (and
  // inheritance of other initializers). The user may have assumed that this
  // case would synthesize Encodable/Decodable conformance for class A the same
  // way it may have for class B, or other classes.
  //
  // It is helpful to suggest here that the user may have forgotten to override
  // init(from:) (and encode(to:), if applicable) in a note, before we start
  // listing the members that prevented initializer synthesis.
  // TODO: Add a fixit along with this suggestion.
  if (auto *superclassDecl = classDecl->getSuperclassDecl()) {
    ASTContext &C = tc.Context;
    auto *decodableProto = C.getProtocol(KnownProtocolKind::Decodable);
    auto superclassType = superclassDecl->getDeclaredInterfaceType();
    if (auto ref = tc.conformsToProtocol(superclassType, decodableProto,
                                         superclassDecl,
                                         ConformanceCheckOptions(),
                                         SourceLoc())) {
      // super conforms to Decodable, so we've failed to inherit init(from:).
      // Let's suggest overriding it here.
      //
      // We're going to diagnose on the concrete init(from:) decl if it exists
      // and isn't implicit; otherwise, on the subclass itself.
      ValueDecl *diagDest = classDecl;
      auto initFrom = DeclName(C, DeclBaseName::createConstructor(), C.Id_from);
      auto result = tc.lookupMember(superclassDecl, superclassType, initFrom,
                                    NameLookupFlags::ProtocolMembers |
                                    NameLookupFlags::IgnoreAccessControl);

      if (!result.empty() && !result.front().getValueDecl()->isImplicit())
        diagDest = result.front().getValueDecl();

      auto diagName = diag::decodable_suggest_overriding_init_here;

      // This is also a bit of a hack, but the best place we've got at the
      // moment to suggest this.
      //
      // If the superclass also conforms to Encodable, it's quite
      // likely that the user forgot to override its encode(to:). In this case,
      // we can produce a slightly different diagnostic to suggest doing so.
      auto *encodableProto = C.getProtocol(KnownProtocolKind::Encodable);
      if ((ref = tc.conformsToProtocol(superclassType, encodableProto,
                                       superclassDecl,
                                       ConformanceCheckOptions(),
                                       SourceLoc()))) {
        // We only want to produce this version of the diagnostic if the
        // subclass doesn't directly implement encode(to:).
        // The direct lookup here won't see an encode(to:) if it is inherited
        // from the superclass.
        auto encodeTo = DeclName(C, C.Id_encode, C.Id_to);
        if (classDecl->lookupDirect(encodeTo).empty())
          diagName = diag::codable_suggest_overriding_init_here;
      }

      tc.diagnose(diagDest, diagName);
    }
  }

  for (auto member : classDecl->getMembers()) {
    auto pbd = dyn_cast<PatternBindingDecl>(member);
    if (!pbd)
      continue;

    if (pbd->isStatic() || !pbd->hasStorage() ||
        pbd->isDefaultInitializable() || pbd->isInvalid())
      continue;
   
    for (auto entry : pbd->getPatternList()) {
      if (entry.getInit()) continue;
      
      SmallVector<VarDecl *, 4> vars;
      entry.getPattern()->collectVariables(vars);
      if (vars.empty()) continue;

      auto varLoc = vars[0]->getLoc();
      
      Optional<InFlightDiagnostic> diag;
      switch (vars.size()) {
      case 1:
        diag.emplace(tc.diagnose(varLoc, diag::note_no_in_class_init_1,
                                 vars[0]->getName()));
        break;
      case 2:
        diag.emplace(tc.diagnose(varLoc, diag::note_no_in_class_init_2,
                                 vars[0]->getName(), vars[1]->getName()));
        break;
      case 3:
        diag.emplace(tc.diagnose(varLoc, diag::note_no_in_class_init_3plus,
                                 vars[0]->getName(), vars[1]->getName(), 
                                 vars[2]->getName(), false));
        break;
      default:
        diag.emplace(tc.diagnose(varLoc, diag::note_no_in_class_init_3plus,
                                 vars[0]->getName(), vars[1]->getName(), 
                                 vars[2]->getName(), true));
        break;
      }

      if (auto defaultValueSuggestion
             = buildDefaultInitializerString(tc, classDecl, entry.getPattern()))
        diag->fixItInsertAfter(entry.getPattern()->getEndLoc(),
                               " = " + *defaultValueSuggestion);
    }
  }
}

void TypeChecker::maybeDiagnoseClassWithoutInitializers(ClassDecl *classDecl) {
  // Some heuristics to skip emitting a diagnostic if the class is already
  // irreperably busted.
  if (classDecl->isInvalid() ||
      classDecl->inheritsSuperclassInitializers(nullptr))
    return;

  auto *superclassDecl = classDecl->getSuperclassDecl();
  if (superclassDecl &&
      superclassDecl->hasMissingDesignatedInitializers())
    return;

  for (auto member : classDecl->lookupDirect(DeclBaseName::createConstructor())) {
    auto ctor = dyn_cast<ConstructorDecl>(member);
    if (ctor && ctor->isDesignatedInit())
      return;
  }

  diagnoseClassWithoutInitializers(*this, classDecl);
}

/// Diagnose a missing required initializer.
static void diagnoseMissingRequiredInitializer(
              TypeChecker &TC,
              ClassDecl *classDecl,
              ConstructorDecl *superInitializer) {
  // Find the location at which we should insert the new initializer.
  SourceLoc insertionLoc;
  SourceLoc indentationLoc;
  for (auto member : classDecl->getMembers()) {
    // If we don't have an indentation location yet, grab one from this
    // member.
    if (indentationLoc.isInvalid()) {
      indentationLoc = member->getLoc();
    }

    // We only want to look at explicit constructors.
    auto ctor = dyn_cast<ConstructorDecl>(member);
    if (!ctor)
      continue;

    if (ctor->isImplicit())
      continue;

    insertionLoc = ctor->getEndLoc();
    indentationLoc = ctor->getLoc();
  }

  // If no initializers were listed, start at the opening '{' for the class.
  if (insertionLoc.isInvalid()) {
    insertionLoc = classDecl->getBraces().Start;
  }
  if (indentationLoc.isInvalid()) {
    indentationLoc = classDecl->getBraces().End;
  }

  // Adjust the insertion location to point at the end of this line (i.e.,
  // the start of the next line).
  insertionLoc = Lexer::getLocForEndOfLine(TC.Context.SourceMgr,
                                           insertionLoc);

  // Find the indentation used on the indentation line.
  StringRef extraIndentation;
  StringRef indentation = Lexer::getIndentationForLine(
      TC.Context.SourceMgr, indentationLoc, &extraIndentation);

  // Pretty-print the superclass initializer into a string.
  // FIXME: Form a new initializer by performing the appropriate
  // substitutions of subclass types into the superclass types, so that
  // we get the right generic parameters.
  std::string initializerText;
  {
    PrintOptions options;
    options.PrintDefaultParameterPlaceholder = false;
    options.PrintImplicitAttrs = false;

    // Render the text.
    llvm::raw_string_ostream out(initializerText);
    {
      ExtraIndentStreamPrinter printer(out, indentation);
      printer.printNewline();

      // If there is no explicit 'required', print one.
      bool hasExplicitRequiredAttr = false;
      if (auto requiredAttr
            = superInitializer->getAttrs().getAttribute<RequiredAttr>())
          hasExplicitRequiredAttr = !requiredAttr->isImplicit();

      if (!hasExplicitRequiredAttr)
        printer << "required ";

      superInitializer->print(printer, options);
    }

    // Add a dummy body.
    out << " {\n";
    out << indentation << extraIndentation << "fatalError(\"";
    superInitializer->getFullName().printPretty(out);
    out << " has not been implemented\")\n";
    out << indentation << "}\n";
  }

  // Complain.
  TC.diagnose(insertionLoc, diag::required_initializer_missing,
              superInitializer->getFullName(),
              superInitializer->getDeclContext()->getDeclaredInterfaceType())
    .fixItInsert(insertionLoc, initializerText);

  TC.diagnose(findNonImplicitRequiredInit(superInitializer),
              diag::required_initializer_here);
}

void TypeChecker::addImplicitConstructors(NominalTypeDecl *decl) {
  // We can only synthesize implicit constructors for classes and structs.
 if (!isa<ClassDecl>(decl) && !isa<StructDecl>(decl))
   return;

  // If we already added implicit initializers, we're done.
  if (decl->addedImplicitInitializers())
    return;
  
  // Don't add implicit constructors for an invalid declaration
  if (decl->isInvalid())
    return;

  // Local function that produces the canonical parameter type of the given
  // initializer.
  // FIXME: Doesn't work properly for generics.
  auto getInitializerParamType = [](ConstructorDecl *ctor) -> CanType {
    auto interfaceTy = ctor->getInterfaceType();

    // Skip the 'self' parameter.
    auto uncurriedInitTy = interfaceTy->castTo<AnyFunctionType>()->getResult();

    // Grab the parameter type;
    auto paramTy = uncurriedInitTy->castTo<AnyFunctionType>()->getInput();

    return paramTy->getCanonicalType();
  };

  // Bail out if we're validating one of our constructors already; we'll
  // revisit the issue later.
  if (isa<ClassDecl>(decl)) {
    for (auto member : decl->getMembers()) {
      if (auto ctor = dyn_cast<ConstructorDecl>(member)) {
        validateDecl(ctor);
        if (!ctor->hasValidSignature())
          return;
      }
    }
  }

  decl->setAddedImplicitInitializers();

  // Check whether there is a user-declared constructor or an instance
  // variable.
  bool FoundMemberwiseInitializedProperty = false;
  bool SuppressDefaultInitializer = false;
  bool SuppressMemberwiseInitializer = false;
  bool FoundDesignatedInit = false;

  SmallPtrSet<CanType, 4> initializerParamTypes;
  llvm::SmallPtrSet<ConstructorDecl *, 4> overriddenInits;
  if (decl->hasClangNode() && isa<ClassDecl>(decl)) {
    // Objective-C classes may have interesting initializers in extensions.
    for (auto member : decl->lookupDirect(DeclBaseName::createConstructor())) {
      auto ctor = dyn_cast<ConstructorDecl>(member);
      if (!ctor)
        continue;

      // Swift initializers added in extensions of Objective-C classes can never
      // be overrides.
      if (!ctor->hasClangNode())
        continue;

      if (auto overridden = ctor->getOverriddenDecl())
        overriddenInits.insert(overridden);
    }

  } else {
    for (auto member : decl->getMembers()) {
      if (auto ctor = dyn_cast<ConstructorDecl>(member)) {
        // Initializers that were synthesized to fulfill derived conformances
        // should not prevent default initializer synthesis.
        if (ctor->isDesignatedInit() && !ctor->isSynthesized())
          FoundDesignatedInit = true;

        if (isa<StructDecl>(decl))
          continue;

        if (!ctor->isInvalid())
          initializerParamTypes.insert(getInitializerParamType(ctor));

        if (auto overridden = ctor->getOverriddenDecl())
          overriddenInits.insert(overridden);

        continue;
      }

      if (auto var = dyn_cast<VarDecl>(member)) {
        if (var->hasStorage() && !var->isStatic() && !var->isInvalid()) {
          // Initialized 'let' properties have storage, but don't get an argument
          // to the memberwise initializer since they already have an initial
          // value that cannot be overridden.
          if (var->isLet() && var->getParentInitializer()) {
          
            // We cannot handle properties like:
            //   let (a,b) = (1,2)
            // for now, just disable implicit init synthesization in structs in
            // this case.
            auto SP = var->getParentPattern();
            if (auto *TP = dyn_cast<TypedPattern>(SP))
              SP = TP->getSubPattern();
            if (!isa<NamedPattern>(SP) && isa<StructDecl>(decl))
              return;
          
            continue;
          }
        
          FoundMemberwiseInitializedProperty = true;
        }
      
        // FIXME: Disable memberwise initializer if a property uses a behavior.
        // Behaviors should be able to control whether they interact with
        // memberwise initialization.
        if (var->hasBehavior())
          SuppressMemberwiseInitializer = true;
        continue;
      }

      // If a stored property lacks an initial value and if there is no way to
      // synthesize an initial value (e.g. for an optional) then we suppress
      // generation of the default initializer.
      if (auto pbd = dyn_cast<PatternBindingDecl>(member)) {
        if (pbd->hasStorage() && !pbd->isStatic() && !pbd->isImplicit())
          for (auto entry : pbd->getPatternList()) {
            if (entry.getInit()) continue;

            // If one of the bound variables is @NSManaged, go ahead no matter
            // what.
            bool CheckDefaultInitializer = true;
            entry.getPattern()->forEachVariable([&](VarDecl *vd) {
              if (vd->getAttrs().hasAttribute<NSManagedAttr>())
                CheckDefaultInitializer = false;
            });
          
            // If we cannot default initialize the property, we cannot
            // synthesize a default initializer for the class.
            if (CheckDefaultInitializer && !pbd->isDefaultInitializable())
              SuppressDefaultInitializer = true;
          }
        continue;
      }
    }
  }

  if (auto structDecl = dyn_cast<StructDecl>(decl)) {
    assert(!structDecl->hasUnreferenceableStorage() &&
           "User-defined structs cannot have unreferenceable storage");

    if (!FoundDesignatedInit && !SuppressMemberwiseInitializer) {
      // For a struct with memberwise initialized properties, we add a
      // memberwise init.
      if (FoundMemberwiseInitializedProperty) {
        // Create the implicit memberwise constructor.
        auto ctor = createImplicitConstructor(
                      *this, decl, ImplicitConstructorKind::Memberwise);
        decl->addMember(ctor);
      }

      // If we found a stored property, add a default constructor.
      if (!SuppressDefaultInitializer)
        defineDefaultConstructor(decl);
    }
    return;
  }
 
  // For a class with a superclass, automatically define overrides
  // for all of the superclass's designated initializers.
  // FIXME: Currently skipping generic classes.
  auto classDecl = cast<ClassDecl>(decl);
  if (classDecl->hasSuperclass()) {
    bool canInheritInitializers = (!SuppressDefaultInitializer &&
                                   !FoundDesignatedInit);

    // We can't define these overrides if we have any uninitialized
    // stored properties.
    if (SuppressDefaultInitializer && !FoundDesignatedInit &&
        !classDecl->hasClangNode()) {
      return;
    }

    auto superclassTy = classDecl->getSuperclass();
    auto *superclassDecl = superclassTy->getClassOrBoundGenericClass();
    assert(superclassDecl && "Superclass of class is not a class?");
    if (!superclassDecl->addedImplicitInitializers())
      addImplicitConstructors(superclassDecl);

    auto ctors = lookupConstructors(classDecl, superclassTy,
                                    NameLookupFlags::IgnoreAccessControl);

    bool canInheritConvenienceInitalizers =
        !superclassDecl->hasMissingDesignatedInitializers();
    SmallVector<ConstructorDecl *, 4> requiredConvenienceInitializers;
    for (auto memberResult : ctors) {
      auto member = memberResult.getValueDecl();

      // Skip unavailable superclass initializers.
      if (AvailableAttr::isUnavailable(member))
        continue;

      // Skip invalid superclass initializers.
      auto superclassCtor = dyn_cast<ConstructorDecl>(member);
      if (superclassCtor->isInvalid())
        continue;

      // If we have an override for this constructor, it's okay.
      if (overriddenInits.count(superclassCtor) > 0)
        continue;

      // We only care about required or designated initializers.
      if (!superclassCtor->isDesignatedInit()) {
        if (superclassCtor->isRequired()) {
          assert(superclassCtor->isInheritable() &&
                 "factory initializers cannot be 'required'");
          requiredConvenienceInitializers.push_back(superclassCtor);
        }
        continue;
      }

      // Otherwise, it may no longer be safe to inherit convenience
      // initializers.
      canInheritConvenienceInitalizers &= canInheritInitializers;

      // Everything after this is only relevant for Swift classes being defined.
      if (classDecl->hasClangNode())
        continue;

      // Diagnose a missing override of a required initializer.
      if (superclassCtor->isRequired() && !canInheritInitializers) {
        diagnoseMissingRequiredInitializer(*this, classDecl, superclassCtor);
        continue;
      }

      // A designated or required initializer has not been overridden.

      // If we have already introduced an initializer with this parameter type,
      // don't add one now.
      if (!initializerParamTypes.insert(
             getInitializerParamType(superclassCtor)).second)
        continue;

      // If we're inheriting initializers, create an override delegating
      // to 'super.init'. Otherwise, create a stub which traps at runtime.
      auto kind = canInheritInitializers
                    ? DesignatedInitKind::Chaining
                    : DesignatedInitKind::Stub;

      // If the superclass initializer is not accessible from the derived
      // class, we cannot chain to 'super.init' either -- create a stub.
      if (!superclassCtor->isAccessibleFrom(classDecl)) {
        assert(!superclassCtor->isRequired() &&
               "required initializer less visible than the class?");
        kind = DesignatedInitKind::Stub;
      }

      // We have a designated initializer. Create an override of it.
      if (auto ctor = createDesignatedInitOverride(
                        *this, classDecl, superclassCtor, kind)) {
        Context.addSynthesizedDecl(ctor);
        classDecl->addMember(ctor);
      }
    }

    if (canInheritConvenienceInitalizers) {
      classDecl->setInheritsSuperclassInitializers();
    } else {
      for (ConstructorDecl *requiredCtor : requiredConvenienceInitializers)
        diagnoseMissingRequiredInitializer(*this, classDecl, requiredCtor);
    }

    return;
  }

  if (!FoundDesignatedInit) {
    // For a class with no superclass, automatically define a default
    // constructor.

    // ... unless there are uninitialized stored properties.
    if (SuppressDefaultInitializer)
      return;

    defineDefaultConstructor(decl);
  }
}

void TypeChecker::synthesizeMemberForLookup(NominalTypeDecl *target,
                                            DeclName member) {
  auto baseName = member.getBaseName();

  // Checks whether the target conforms to the given protocol. If the
  // conformance is incomplete, force the conformance.
  //
  // Returns whether the target conforms to the protocol.
  auto evaluateTargetConformanceTo = [&](ProtocolDecl *protocol) {
    auto targetType = target->getDeclaredInterfaceType();
    if (auto ref = conformsToProtocol(
                        targetType, protocol, target,
                        (ConformanceCheckFlags::Used|
                         ConformanceCheckFlags::SkipConditionalRequirements),
                         SourceLoc())) {
      if (auto *conformance = ref->getConcrete()->getRootNormalConformance()) {
        if (conformance->getState() == ProtocolConformanceState::Incomplete) {
          checkConformance(conformance);
        }
      }

      return true;
    }

    return false;
  };

  if (member.isSimpleName() && !baseName.isSpecial()) {
    if (baseName.getIdentifier() == Context.Id_CodingKeys) {
      // CodingKeys is a special type which may be synthesized as part of
      // Encodable/Decodable conformance. If the target conforms to either
      // protocol and would derive conformance to either, the type may be
      // synthesized.
      // If the target conforms to either and the conformance has not yet been
      // evaluated, then we should do that here.
      //
      // Try to synthesize Decodable first. If that fails, try to synthesize
      // Encodable. If either succeeds and CodingKeys should have been
      // synthesized, it will be synthesized.
      auto *decodableProto = Context.getProtocol(KnownProtocolKind::Decodable);
      auto *encodableProto = Context.getProtocol(KnownProtocolKind::Encodable);
      if (!evaluateTargetConformanceTo(decodableProto))
        (void)evaluateTargetConformanceTo(encodableProto);
    }
  } else {
    auto argumentNames = member.getArgumentNames();
    if (argumentNames.size() != 1)
      return;

    auto argumentName = argumentNames.front();
    if (baseName == DeclBaseName::createConstructor() &&
        argumentName == Context.Id_from) {
      // init(from:) may be synthesized as part of derived conformance to the
      // Decodable protocol.
      // If the target should conform to the Decodable protocol, check the
      // conformance here to attempt synthesis.
      auto *decodableProto = Context.getProtocol(KnownProtocolKind::Decodable);
      (void)evaluateTargetConformanceTo(decodableProto);
    } else if (!baseName.isSpecial() &&
               baseName.getIdentifier() == Context.Id_encode &&
               argumentName == Context.Id_to) {
      // encode(to:) may be synthesized as part of derived conformance to the
      // Encodable protocol.
      // If the target should conform to the Encodable protocol, check the
      // conformance here to attempt synthesis.
      auto *encodableProto = Context.getProtocol(KnownProtocolKind::Encodable);
      (void)evaluateTargetConformanceTo(encodableProto);
    }
  }
}

void TypeChecker::defineDefaultConstructor(NominalTypeDecl *decl) {
  PrettyStackTraceDecl stackTrace("defining default constructor for",
                                  decl);

  // Clang-imported types should never get a default constructor, just a
  // memberwise one.
  if (decl->hasClangNode())
    return;

  // For a class, check whether the superclass (if it exists) is
  // default-initializable.
  if (isa<ClassDecl>(decl)) {
    // We need to look for a default constructor.
    if (auto superTy = getSuperClassOf(decl->getDeclaredInterfaceType())) {
      // If there are no default ctors for our supertype, we can't do anything.
      auto ctors = lookupConstructors(decl, superTy);
      if (!ctors)
        return;

      // Check whether we have a constructor that can be called with an empty
      // tuple.
      bool foundDefaultConstructor = false;
      for (auto memberResult : ctors) {
        auto member = memberResult.getValueDecl();

        // Dig out the parameter tuple for this constructor.
        auto ctor = dyn_cast<ConstructorDecl>(member);
        if (!ctor || ctor->isInvalid())
          continue;

        // Check to see if this ctor has zero arguments, or if they all have
        // default values.
        auto params = ctor->getParameters();
        
        bool missingInit = false;
        for (auto param : *params) {
          if (!param->isDefaultArgument()) {
            missingInit = true;
            break;
          }
        }

        // Check to see if this is an impossible candidate.
        if (missingInit) {
          // If we found an impossible designated initializer, then we cannot
          // call super.init(), even if there is a match.
          if (ctor->isDesignatedInit())
            return;

          // Otherwise, keep looking.
          continue;
        }

        // Ok, we found a constructor that can be invoked with an empty tuple.
        // If this is our second, then we bail out, because we don't want to
        // pick one arbitrarily.
        if (foundDefaultConstructor)
          return;

        foundDefaultConstructor = true;
      }

      // If our superclass isn't default constructible, we aren't either.
      if (!foundDefaultConstructor) return;
    }
  }

  // Create the default constructor.
  auto ctor = createImplicitConstructor(*this, decl,
                                        ImplicitConstructorKind::Default);

  // Add the constructor.
  decl->addMember(ctor);

  // Create an empty body for the default constructor. The type-check of the
  // constructor body will introduce default initializations of the members.
  ctor->setBody(BraceStmt::create(Context, SourceLoc(), { }, SourceLoc()));

  // Make sure we type check the constructor later.
  Context.addSynthesizedDecl(ctor);
}

static void validateAttributes(TypeChecker &TC, Decl *D) {
  DeclAttributes &Attrs = D->getAttrs();

  auto checkObjCDeclContext = [](Decl *D) {
    DeclContext *DC = D->getDeclContext();
    if (DC->getAsClassOrClassExtensionContext())
      return true;
    if (auto *PD = dyn_cast<ProtocolDecl>(DC))
      if (PD->isObjC())
        return true;
    return false;
  };

  if (auto objcAttr = Attrs.getAttribute<ObjCAttr>()) {
    // Only certain decls can be ObjC.
    Optional<Diag<>> error;
    if (isa<ClassDecl>(D) ||
        isa<ProtocolDecl>(D)) {
      /* ok */
    } else if (auto Ext = dyn_cast<ExtensionDecl>(D)) {
      if (!Ext->getAsClassOrClassExtensionContext())
        error = diag::objc_extension_not_class;
    } else if (auto ED = dyn_cast<EnumDecl>(D)) {
      if (ED->isGenericContext())
        error = diag::objc_enum_generic;
    } else if (auto EED = dyn_cast<EnumElementDecl>(D)) {
      auto ED = EED->getParentEnum();
      if (!ED->getAttrs().hasAttribute<ObjCAttr>())
        error = diag::objc_enum_case_req_objc_enum;
      else if (objcAttr->hasName() && EED->getParentCase()->getElements().size() > 1)
        error = diag::objc_enum_case_multi;
    } else if (auto *func = dyn_cast<FuncDecl>(D)) {
      if (!checkObjCDeclContext(D))
        error = diag::invalid_objc_decl_context;
      else if (auto accessor = dyn_cast<AccessorDecl>(func))
        if (!accessor->isGetterOrSetter())
          error = diag::objc_observing_accessor;
    } else if (isa<ConstructorDecl>(D) ||
               isa<DestructorDecl>(D) ||
               isa<SubscriptDecl>(D) ||
               isa<VarDecl>(D)) {
      if (!checkObjCDeclContext(D))
        error = diag::invalid_objc_decl_context;
      /* ok */
    } else {
      error = diag::invalid_objc_decl;
    }

    if (error) {
      TC.diagnose(D->getStartLoc(), *error)
        .fixItRemove(objcAttr->getRangeWithAt());
      objcAttr->setInvalid();
      return;
    }

    // If there is a name, check whether the kind of name is
    // appropriate.
    if (auto objcName = objcAttr->getName()) {
      if (isa<ClassDecl>(D) || isa<ProtocolDecl>(D) || isa<VarDecl>(D)
          || isa<EnumDecl>(D) || isa<EnumElementDecl>(D)
          || isa<ExtensionDecl>(D)) {
        // Types and properties can only have nullary
        // names. Complain and recover by chopping off everything
        // after the first name.
        if (objcName->getNumArgs() > 0) {
          SourceLoc firstNameLoc = objcAttr->getNameLocs().front();
          SourceLoc afterFirstNameLoc = 
            Lexer::getLocForEndOfToken(TC.Context.SourceMgr, firstNameLoc);
          TC.diagnose(firstNameLoc, diag::objc_name_req_nullary,
                      D->getDescriptiveKind())
            .fixItRemoveChars(afterFirstNameLoc, objcAttr->getRParenLoc());
          const_cast<ObjCAttr *>(objcAttr)->setName(
            ObjCSelector(TC.Context, 0, objcName->getSelectorPieces()[0]),
            /*implicit=*/false);
        }
      } else if (isa<SubscriptDecl>(D) || isa<DestructorDecl>(D)) {
        TC.diagnose(objcAttr->getLParenLoc(),
                    isa<SubscriptDecl>(D)
                      ? diag::objc_name_subscript
                      : diag::objc_name_deinit);
        const_cast<ObjCAttr *>(objcAttr)->clearName();
      } else {
        // We have a function. Make sure that the number of parameters
        // matches the "number of colons" in the name.
        auto func = cast<AbstractFunctionDecl>(D);
        auto params = func->getParameterList(1);
        unsigned numParameters = params->size();
        if (auto CD = dyn_cast<ConstructorDecl>(func))
          if (CD->isObjCZeroParameterWithLongSelector())
            numParameters = 0;  // Something like "init(foo: ())"

        // A throwing method has an error parameter.
        if (func->hasThrows())
          ++numParameters;

        unsigned numArgumentNames = objcName->getNumArgs();
        if (numArgumentNames != numParameters) {
          TC.diagnose(objcAttr->getNameLocs().front(), 
                      diag::objc_name_func_mismatch,
                      isa<FuncDecl>(func), 
                      numArgumentNames, 
                      numArgumentNames != 1,
                      numParameters,
                      numParameters != 1,
                      func->hasThrows());
          D->getAttrs().add(
            ObjCAttr::createUnnamed(TC.Context,
                                    objcAttr->AtLoc,
                                    objcAttr->Range.Start));
          D->getAttrs().removeAttribute(objcAttr);
        }
      }
    } else if (isa<EnumElementDecl>(D)) {
      // Enum elements require names.
      TC.diagnose(objcAttr->getLocation(), diag::objc_enum_case_req_name)
        .fixItRemove(objcAttr->getRangeWithAt());
      objcAttr->setInvalid();
    }
  }

  if (auto nonObjcAttr = Attrs.getAttribute<NonObjCAttr>()) {
    // Only extensions of classes; methods, properties, subscripts
    // and constructors can be NonObjC.
    // The last three are handled automatically by generic attribute
    // validation -- for the first one, we have to check FuncDecls
    // ourselves.
    Optional<Diag<>> error;

    auto func = dyn_cast<FuncDecl>(D);
    if (func &&
        (isa<DestructorDecl>(func) ||
         !checkObjCDeclContext(func) ||
         (isa<AccessorDecl>(func) &&
          !cast<AccessorDecl>(func)->isGetterOrSetter()))) {
      error = diag::invalid_nonobjc_decl;
    }

    if (auto ext = dyn_cast<ExtensionDecl>(D)) {
      if (!ext->getAsClassOrClassExtensionContext())
        error = diag::invalid_nonobjc_extension;
    }

    if (error) {
      TC.diagnose(D->getStartLoc(), *error)
        .fixItRemove(nonObjcAttr->getRangeWithAt());
      nonObjcAttr->setInvalid();
      return;
    }
  }

  // Only protocol members can be optional.
  if (auto *OA = Attrs.getAttribute<OptionalAttr>()) {
    if (!isa<ProtocolDecl>(D->getDeclContext())) {
      TC.diagnose(OA->getLocation(), diag::optional_attribute_non_protocol)
        .fixItRemove(OA->getRange());
      D->getAttrs().removeAttribute(OA);
    } else if (!cast<ProtocolDecl>(D->getDeclContext())->isObjC()) {
      TC.diagnose(OA->getLocation(),
                  diag::optional_attribute_non_objc_protocol);
      D->getAttrs().removeAttribute(OA);
    } else if (isa<ConstructorDecl>(D)) {
      TC.diagnose(OA->getLocation(),
                  diag::optional_attribute_initializer);
      D->getAttrs().removeAttribute(OA);
    } else {
      auto objcAttr = D->getAttrs().getAttribute<ObjCAttr>();
      if (!objcAttr || objcAttr->isImplicit()) {
        auto diag = TC.diagnose(OA->getLocation(),
                                diag::optional_attribute_missing_explicit_objc);
        if (auto VD = dyn_cast<ValueDecl>(D))
          diag.fixItInsert(VD->getAttributeInsertionLoc(false), "@objc ");
      }
    }
  }

  // Only protocols that are @objc can have "unavailable" methods.
  if (auto AvAttr = Attrs.getUnavailable(TC.Context)) {
    if (auto PD = dyn_cast<ProtocolDecl>(D->getDeclContext())) {
      if (!PD->isObjC()) {
        TC.diagnose(AvAttr->getLocation(),
                    diag::unavailable_method_non_objc_protocol);
        D->getAttrs().removeAttribute(AvAttr);
      }
    }
  }
}
