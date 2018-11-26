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
#include "TypeCheckAccess.h"
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
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Strings.h"
#include "swift/AST/TypeCheckRequests.h"
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

/// Check that the declaration attributes are ok.
static void validateAttributes(TypeChecker &TC, Decl *D);

void TypeChecker::resolveTrailingWhereClause(ProtocolDecl *proto) {
  ProtocolRequirementTypeResolver resolver;
  validateWhereClauses(proto, &resolver);
}

void TypeChecker::validateWhereClauses(ProtocolDecl *protocol,
                                       GenericTypeResolver *resolver) {
  TypeResolutionOptions options;

  options |= TypeResolutionFlags::ProtocolWhereClause;

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
  for (unsigned i : indices(protocol->getInherited()))
    (void)protocol->getInheritedType(i);
  resolveTrailingWhereClause(protocol);
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

  if (auto type = dyn_cast<TypeDecl>(decl)) {
    inheritedClause = type->getInherited();
  } else {
    auto ext = cast<ExtensionDecl>(decl);
    if (!ext->getExtendedType())
      return;

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
  llvm::SmallDenseMap<CanType, std::pair<unsigned, SourceRange>> inheritedTypes;
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

    if (inheritedTy->isExistentialType()) {
      auto layout = inheritedTy->getExistentialLayout();

      // @objc protocols cannot have superclass constraints.
      if (layout.explicitSuperclass) {
        if (auto *protoDecl = dyn_cast<ProtocolDecl>(decl)) {
          if (protoDecl->isObjC()) {
            diagnose(protoDecl,
                    diag::objc_protocol_with_superclass,
                    protoDecl->getName());
            continue;
          }
        }
      }

      // Protocols, generic parameters and associated types can inherit
      // from subclass existentials, which are "exploded" into their
      // corresponding requirements.
      //
      // Extensions, structs and enums can only inherit from protocol
      // compositions that do not contain AnyObject or class members.
      if (isa<ProtocolDecl>(decl) ||
          isa<AbstractTypeParamDecl>(decl) ||
          (!layout.hasExplicitAnyObject &&
           !layout.explicitSuperclass)) {
        continue;
      }

      // Classes can inherit from subclass existentials as long as they
      // do not contain an explicit AnyObject member.
      if (isa<ClassDecl>(decl) &&
          !layout.hasExplicitAnyObject) {
        // Superclass inheritance is handled below.
        inheritedTy = layout.explicitSuperclass;
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

      // @objc protocols cannot have superclass constraints.
      if (auto *protoDecl = dyn_cast<ProtocolDecl>(decl)) {
        if (protoDecl->isObjC()) {
          diagnose(protoDecl,
                   diag::objc_protocol_with_superclass,
                   protoDecl->getName());
          continue;
        }
      }

      // If the declaration we're looking at doesn't allow a superclass,
      // complain.
      if (isa<StructDecl>(decl) || isa<ExtensionDecl>(decl)) {
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
      if (isa<ClassDecl>(decl) && i > 0) {
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
             (isa<StructDecl>(decl) || isa<ExtensionDecl>(decl))
               ? diag::inheritance_from_non_protocol
               : diag::inheritance_from_non_protocol_or_class,
             inheritedTy);
    // FIXME: Note pointing to the declaration 'inheritedTy' references?
    inherited.setInvalidType(Context);
  }
}

/// Retrieve the set of protocols the given protocol inherits.
static llvm::TinyPtrVector<ProtocolDecl *>
getInheritedForCycleCheck(TypeChecker &tc,
                          ProtocolDecl *proto,
                          ProtocolDecl **scratch) {
  TinyPtrVector<ProtocolDecl *> result;

  for (unsigned index : indices(proto->getInherited())) {
    if (auto type = proto->getInheritedType(index)) {
      if (type->isExistentialType()) {
        auto layout = type->getExistentialLayout();
        for (auto protoTy : layout.getProtocols()) {
          auto *protoDecl = protoTy->getDecl();
          result.push_back(protoDecl);
        }
      }
    }
  }

  return result;
}

/// Retrieve the superclass of the given class.
static ArrayRef<ClassDecl *> getInheritedForCycleCheck(TypeChecker &tc,
                                                       ClassDecl *classDecl,
                                                       ClassDecl **scratch) {
  if (classDecl->hasSuperclass()) {
    *scratch = classDecl->getSuperclassDecl();
    return *scratch;
  }
  return { };
}

/// Retrieve the raw type of the given enum.
static ArrayRef<EnumDecl *> getInheritedForCycleCheck(TypeChecker &tc,
                                                      EnumDecl *enumDecl,
                                                      EnumDecl **scratch) {
  if (enumDecl->hasRawType()) {
    *scratch = enumDecl->getRawType()->getEnumOrBoundGenericEnum();
    return *scratch ? ArrayRef<EnumDecl*>(*scratch) : ArrayRef<EnumDecl*>{};
  }
  return { };
}

/// Check for circular inheritance.
template<typename T>
static void checkCircularity(TypeChecker &tc, T *decl,
                             Diag<Identifier> circularDiag,
                             DescriptiveDeclKind declKind,
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
                  path.back()->getName());

      break;
    }

    // Diagnose the cycle.
    tc.diagnose(decl->getLoc(), circularDiag,
                (*cycleStart)->getName());
    for (auto i = cycleStart + 1, iEnd = path.end(); i != iEnd; ++i) {
      tc.diagnose(*i, diag::kind_identifier_declared_here,
                  declKind, (*i)->getName());
    }

    break;
  }

  case CircularityCheck::Unchecked: {
    // Walk to the inherited class or protocols.
    path.push_back(decl);
    decl->setCircularityCheck(CircularityCheck::Checking);
    T *scratch = nullptr;
    for (auto inherited : getInheritedForCycleCheck(tc, decl, &scratch)) {
      checkCircularity(tc, inherited, circularDiag, declKind, path);
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

    const auto markInvalid = [&current]() {
      current->setInvalid();
      if (auto *varDecl = dyn_cast<VarDecl>(current))
        if (varDecl->hasType())
          varDecl->setType(ErrorType::get(varDecl->getType()));
      if (current->hasInterfaceType())
        current->setInterfaceType(ErrorType::get(current->getInterfaceType()));
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

      // If both are VarDecls, and both have exactly the same type, then
      // matching the Swift 4 behaviour (i.e. just emitting the future-compat
      // warning) will result in SILGen crashes due to both properties mangling
      // the same, so it's better to just follow the Swift 5 behaviour and emit
      // the actual error.
      if (wouldBeSwift5Redeclaration && isa<VarDecl>(current) &&
          isa<VarDecl>(other) &&
          current->getInterfaceType()->isEqual(other->getInterfaceType())) {
        wouldBeSwift5Redeclaration = false;
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
          tc.diagnose(binding, diag::class_var_not_in_class, false)
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
    // We used to not apply the solution to lazy bindings here, but that's
    // unnecessary: the code for building lazy getters already has to handle
    // initializers which have had solutions applied, and not applying the
    // solution blocks other diagnostics if we decide not to synthesize the
    // getter.
    bool skipApplyingSolution = false;
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
  assert(binding->getPattern(entryNumber)->hasType() && "Type missing?");
  if (auto var = binding->getSingleVar()) {
    tc.checkTypeModifyingDeclAttributes(var);
  }
}

/// Validate the entries in the given pattern binding declaration.
static void validatePatternBindingEntries(TypeChecker &tc,
                                          PatternBindingDecl *binding) {
  if (binding->hasValidationStarted())
    return;

  DeclValidationRAII IBV(binding);

  for (unsigned i = 0, e = binding->getNumPatternEntries(); i != e; ++i)
    validatePatternBindingEntry(tc, binding, i);
}

void swift::makeFinal(ASTContext &ctx, ValueDecl *D) {
  if (D && !D->isFinal()) {
    assert(isa<ClassDecl>(D) || D->isPotentiallyOverridable());
    D->getAttrs().add(new (ctx) FinalAttr(/*IsImplicit=*/true));
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

  if (selfParam.getPlainType()->is<ErrorType>())
    selfDecl->setInvalid();
}

static void recordParamContextTypes(AbstractFunctionDecl *func) {
  auto *env = func->getGenericEnvironment();

  if (auto *selfDecl = func->getImplicitSelfDecl()) {
    if (!env)
      selfDecl->setType(selfDecl->getInterfaceType());
    else
      selfDecl->setType(env->mapTypeIntoContext(selfDecl->getInterfaceType()));
  }

  for (auto param : *func->getParameters()) {
    if (!env)
      param->setType(param->getInterfaceType());
    else
      param->setType(env->mapTypeIntoContext(param->getInterfaceType()));
  }
}

static void recordIndexContextTypes(SubscriptDecl *subscript) {
  auto *env = subscript->getGenericEnvironment();
  for (auto param : *subscript->getIndices()) {
    if (!env)
      param->setType(param->getInterfaceType());
    else
      param->setType(env->mapTypeIntoContext(param->getInterfaceType()));
  }
}

/// Try to make the given declaration 'dynamic', checking any semantic
/// constraints before doing so.
///
/// \returns true if it can be made dynamic, false otherwise.
static bool makeDynamic(ValueDecl *decl) {
  // Only  members of classes can be dynamic.
  auto classDecl = decl->getDeclContext()->getAsClassOrClassExtensionContext();
  if (!classDecl) {
    auto attr = decl->getAttrs().getAttribute<DynamicAttr>();
    decl->diagnose(diag::dynamic_not_in_class)
      .fixItRemove(attr ? SourceRange(attr->getLocation()) : SourceRange());
    return false;
  }

  // 'dynamic' is only supported through the Objective-C runtime.
  if (!decl->isObjC()) {
    decl->diagnose(diag::dynamic_requires_objc,
                   decl->getDescriptiveKind(), decl->getFullName())
      .fixItInsert(decl->getAttributeInsertionLoc(/*forModifier=*/false),
                   "@objc ");
    return false;
  }

  // If there isn't already a 'dynamic' attribute, add an inferred one.
  if (!decl->getAttrs().hasAttribute<DynamicAttr>()) {
    auto attr = new (decl->getASTContext()) DynamicAttr(/*implicit=*/true);
    decl->getAttrs().add(attr);
  }

  return true;
}

bool IsDynamicRequest::evaluate(Evaluator &evaluator, ValueDecl *decl) const {
  // If we can't infer dynamic here, don't.
  if (!DeclAttribute::canAttributeAppearOnDecl(DAK_Dynamic, decl))
    return false;

  // If 'dynamic' was explicitly specified, check it.
  if (auto dynamicAttr = decl->getAttrs().getAttribute<DynamicAttr>()) {
    return makeDynamic(decl);
  }

  // Runtime-replacable accessors are dynamic when their storage declaration
  // is dynamic. Other accessors are never dynamic.
  if (auto accessor = dyn_cast<AccessorDecl>(decl)) {
    switch (accessor->getAccessorKind()) {
    case AccessorKind::Get:
    case AccessorKind::Set:
      if (evaluator(IsDynamicRequest{accessor->getStorage()}))
        return makeDynamic(decl);

      return false;

#define OBJC_ACCESSOR(ID, KEYWORD)
#define ACCESSOR(ID) \
    case AccessorKind::ID:
#include "swift/AST/AccessorKinds.def"
      return false;
    }
  }

  // The 'NSManaged' attribute implies 'dynamic'.
  // FIXME: Use a semantic check for NSManaged rather than looking for the
  // attribute (which could be ill-formed).
  if (decl->getAttrs().hasAttribute<NSManagedAttr>()) {
    return makeDynamic(decl);
  }

  // The presence of 'final' blocks the inference of 'dynamic'.
  if (decl->isFinal())
    return false;

  // Types are never 'dynamic'.
  if (isa<TypeDecl>(decl))
    return false;

  // A non-@objc entity is never 'dynamic'.
  if (!decl->isObjC())
    return false;

  // @objc declarations in class extensions are implicitly dynamic.
  // This is intended to enable overriding the declarations.
  auto dc = decl->getDeclContext();
  if (isa<ExtensionDecl>(dc) && dc->getAsClassOrClassExtensionContext()) {
    return makeDynamic(decl);
  }

  // If any of the declarations overridden by this declaration are dynamic
  // or were imported from Objective-C, this declaration is dynamic.
  // Don't do this if the declaration is not exposed to Objective-C; that's
  // currently the (only) manner in which one can make an override of a
  // dynamic declaration non-dynamic.
  for (auto overridden : evaluator(OverriddenDeclsRequest{decl})) {
    if (overridden->isDynamic())
      return makeDynamic(decl);

    if (overridden->hasClangNode())
      return makeDynamic(decl);
  }

  return false;
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
    // If the prevValue is not a well-typed integer, then break.
    // This could happen if the literal value overflows _MaxBuiltinIntegerType.
    if (!prevValue->getType())
      return nullptr;

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
    return;
  }

  if (ED->getGenericEnvironmentOfContext() != nullptr)
    rawTy = ED->mapTypeIntoContext(rawTy);
  if (rawTy->hasError())
    return;

  AutomaticEnumValueKind valueKind;
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
    }
    
    {
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

  // Build interface and context substitution maps.
  auto interfaceSubsMap =
    SubstitutionMap::getProtocolSubstitutions(
                                behaviorProto,
                                behaviorInterfaceSelf,
                                ProtocolConformanceRef(conformance));

  auto contextSubsMap =
    SubstitutionMap::getProtocolSubstitutions(
                                  behaviorProto,
                                  behaviorSelf,
                                  ProtocolConformanceRef(conformance));

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
                      diag::identifier_declared_here, TC.Context.Id_initStorage);
          TC.diagnose(parameterizedInitStorageDecl->getLoc(),
                      diag::identifier_declared_here, TC.Context.Id_initStorage);
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
                                           interfaceSubsMap,
                                           contextSubsMap);
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
          TC.diagnose(varReqt->getLoc(), diag::identifier_declared_here,
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
                                             interfaceSubsMap,
                                             contextSubsMap);
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
  auto substValueTy =
    behavior->ValueDecl->getInterfaceType().subst(interfaceSubsMap);
  
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
                                       interfaceSubsMap, contextSubsMap);
  
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
  DeclValidationRAII IBV(PGD);

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
          diagnose(group->getNameLoc(), diag::kind_declared_here,
                   DescriptiveDeclKind::PrecedenceGroup);
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
  switch (storage->getReadImpl()) {
  case ReadImplKind::Stored:
    return false;

  case ReadImplKind::Get:
  case ReadImplKind::Inherited:
    return validateAccessorIsMutating(TC, storage->getGetter());

  case ReadImplKind::Address:
    return validateAccessorIsMutating(TC, storage->getAddressor());

  case ReadImplKind::Read:
    return validateAccessorIsMutating(TC, storage->getReadCoroutine());
  }

  llvm_unreachable("bad impl kind");
}

static bool computeIsSetterMutating(TypeChecker &TC,
                                    AbstractStorageDecl *storage) {
  auto impl = storage->getImplInfo();
  switch (impl.getWriteImpl()) {
  case WriteImplKind::Immutable:
  case WriteImplKind::Stored:
    // Instance member setters are mutating; static property setters and
    // top-level setters are not.
    // It's important that we use this logic for "immutable" storage
    // in order to handle initialization of let-properties.
    return storage->isInstanceMember() &&
           doesContextHaveValueSemantics(storage->getDeclContext());

  case WriteImplKind::StoredWithObservers:
  case WriteImplKind::InheritedWithObservers:
  case WriteImplKind::Set: {
    auto result = validateAccessorIsMutating(TC, storage->getSetter());

    // As a special extra check, if the user also gave us a modify
    // coroutine, check that it has the same mutatingness as the setter.
    // TODO: arguably this should require the spelling to match even when
    // it's the implied value.
    if (impl.getReadWriteImpl() == ReadWriteImplKind::Modify) {
      auto modifyAccessor = storage->getModifyCoroutine();
      auto modifyResult = validateAccessorIsMutating(TC, modifyAccessor);
      if (result != modifyResult) {
        TC.diagnose(modifyAccessor,
                    diag::modify_mutatingness_differs_from_setter,
                    modifyResult);
        TC.diagnose(storage->getSetter(), diag::previous_accessor, "setter", 0);
        modifyAccessor->setInvalid();
      }
    }

    return result;
  }

  case WriteImplKind::MutableAddress:
    return validateAccessorIsMutating(TC, storage->getMutableAddressor());

  case WriteImplKind::Modify:
    return validateAccessorIsMutating(TC, storage->getModifyCoroutine());
  }
  llvm_unreachable("bad storage kind");
}

static void validateAbstractStorageDecl(TypeChecker &TC,
                                        AbstractStorageDecl *storage) {
  // isGetterMutating and isSetterMutating are part of the signature
  // of a storage declaration and need to be validated immediately.
  storage->setIsGetterMutating(computeIsGetterMutating(TC, storage));
  storage->setIsSetterMutating(computeIsSetterMutating(TC, storage));

  // Add any mandatory accessors now.
  maybeAddAccessorsToStorage(TC, storage);

  // Everything else about the accessors can wait until finalization.
  // This will validate all the accessors.
  TC.DeclsToFinalize.insert(storage);
}

static void finalizeAbstractStorageDecl(TypeChecker &TC,
                                        AbstractStorageDecl *storage) {
  TC.validateDecl(storage);

  for (auto accessor : storage->getAllAccessors()) {
    // Are there accessors we can safely ignore here, like maybe observers?
    TC.validateDecl(accessor);
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

      // Make sure we finalize this declaration.
      TC.DeclsToFinalize.insert(VD);

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
      // maybeAddAccessorsToStorage(), to ensure they run when a
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

    if (!checkOverrides(VD)) {
      // If a property has an override attribute but does not override
      // anything, complain.
      auto overridden = VD->getOverriddenDecl();
      if (auto *OA = VD->getAttrs().getAttribute<OverrideAttr>()) {
        if (!overridden) {
          TC.diagnose(VD, diag::property_does_not_override)
            .highlight(OA->getLocation());
          OA->setInvalid();
        }
      }
    }

    TC.checkDeclAttributes(VD);

    triggerAccessorSynthesis(TC, VD);

    // Under the Swift 3 inference rules, if we have @IBInspectable or
    // @GKInspectable but did not infer @objc, warn that the attribute is
    if (!VD->isObjC() && TC.Context.LangOpts.EnableSwift3ObjCInference) {
      if (auto attr = VD->getAttrs().getAttribute<IBInspectableAttr>()) {
        TC.diagnose(attr->getLocation(),
                    diag::attribute_meaningless_when_nonobjc,
                    attr->getAttrName())
          .fixItRemove(attr->getRange());
      }

      if (auto attr = VD->getAttrs().getAttribute<GKInspectableAttr>()) {
        TC.diagnose(attr->getLocation(),
                    diag::attribute_meaningless_when_nonobjc,
                    attr->getAttrName())
          .fixItRemove(attr->getRange());
      }
    }
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
        if (var->getWriteImpl() == WriteImplKind::StoredWithObservers &&
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

    AccessControlChecker::checkAccessControl(TC, PBD);
    UsableFromInlineChecker::checkUsableFromInline(TC, PBD);

    // If the initializers in the PBD aren't checked yet, do so now.
    for (unsigned i = 0, e = PBD->getNumPatternEntries(); i != e; ++i) {
      if (!PBD->isInitializerChecked(i) && PBD->getInit(i))
        TC.typeCheckPatternBinding(PBD, i, /*skipApplyingSolution*/false);
    }
  }

  void visitSubscriptDecl(SubscriptDecl *SD) {
    TC.validateDecl(SD);

    if (!SD->isInvalid()) {
      TC.checkReferencedGenericParams(SD);
      TC.checkProtocolSelfRequirements(SD);
    }

    TC.checkDeclAttributes(SD);

    AccessControlChecker::checkAccessControl(TC, SD);
    UsableFromInlineChecker::checkUsableFromInline(TC, SD);

    if (!checkOverrides(SD)) {
      // If a subscript has an override attribute but does not override
      // anything, complain.
      if (auto *OA = SD->getAttrs().getAttribute<OverrideAttr>()) {
        if (!SD->getOverriddenDecl()) {
          TC.diagnose(SD, diag::subscript_does_not_override)
            .highlight(OA->getLocation());
          OA->setInvalid();
        }
      }
    }

    triggerAccessorSynthesis(TC, SD);
  }

  void visitTypeAliasDecl(TypeAliasDecl *TAD) {
    TC.checkDeclAttributesEarly(TAD);

    TC.validateDecl(TAD);
    TC.checkDeclAttributes(TAD);

    AccessControlChecker::checkAccessControl(TC, TAD);
    UsableFromInlineChecker::checkUsableFromInline(TC, TAD);
  }
  
  void visitAssociatedTypeDecl(AssociatedTypeDecl *AT) {
    TC.checkDeclAttributesEarly(AT);

    TC.validateDecl(AT);
    TC.checkDeclAttributes(AT);

    TC.checkInheritanceClause(AT);

    auto *proto = AT->getProtocol();
    if (proto->isObjC()) {
      TC.diagnose(AT->getLoc(),
                  diag::associated_type_objc,
                  AT->getName(),
                  proto->getName());
    }

    AccessControlChecker::checkAccessControl(TC, AT);
    UsableFromInlineChecker::checkUsableFromInline(TC, AT);

    // Trigger the checking for overridden declarations.
    (void)AT->getOverriddenDecls();
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

    checkUnsupportedNestedType(ED);
    TC.validateDecl(ED);

    {
      // Check for circular inheritance of the raw type.
      SmallVector<EnumDecl *, 8> path;
      path.push_back(ED);
      checkCircularity(TC, ED, diag::circular_enum_inheritance,
                       DescriptiveDeclKind::Enum, path);
    }

    for (Decl *member : ED->getMembers())
      visit(member);

    TC.checkDeclAttributes(ED);

    TC.checkInheritanceClause(ED);

    AccessControlChecker::checkAccessControl(TC, ED);
    UsableFromInlineChecker::checkUsableFromInline(TC, ED);

    if (ED->hasRawType() && !ED->isObjC()) {
      // ObjC enums have already had their raw values checked, but pure Swift
      // enums haven't.
      checkEnumRawValues(TC, ED);
    }

    ED->getAllConformances();

    TC.checkDeclCircularity(ED);
    TC.ConformanceContexts.push_back(ED);
  }

  void visitStructDecl(StructDecl *SD) {
    TC.checkDeclAttributesEarly(SD);

    checkUnsupportedNestedType(SD);

    TC.validateDecl(SD);

    TC.addImplicitConstructors(SD);

    for (Decl *Member : SD->getMembers())
      visit(Member);

    TC.checkDeclAttributes(SD);

    TC.checkInheritanceClause(SD);

    AccessControlChecker::checkAccessControl(TC, SD);
    UsableFromInlineChecker::checkUsableFromInline(TC, SD);

    SD->getAllConformances();

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

      classDecl = classDecl->getSuperclassDecl();
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
          auto superclass = source->getSuperclassDecl();
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

    checkUnsupportedNestedType(CD);

    TC.validateDecl(CD);
    TC.requestSuperclassLayout(CD);

    {
      // Check for circular inheritance.
      SmallVector<ClassDecl *, 8> path;
      path.push_back(CD);
      checkCircularity(TC, CD, diag::circular_class_inheritance,
                       DescriptiveDeclKind::Class, path);
    }

    for (Decl *Member : CD->getMembers()) {
      visit(Member);
    }

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
          !Super->hasOpenAccess(CD->getDeclContext()) &&
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

    CD->getAllConformances();

    TC.checkDeclAttributes(CD);

    TC.checkInheritanceClause(CD);

    AccessControlChecker::checkAccessControl(TC, CD);
    UsableFromInlineChecker::checkUsableFromInline(TC, CD);

    TC.checkDeclCircularity(CD);
    TC.ConformanceContexts.push_back(CD);
  }

  void visitProtocolDecl(ProtocolDecl *PD) {
    TC.checkDeclAttributesEarly(PD);

    checkUnsupportedNestedType(PD);

    TC.validateDecl(PD);
    if (!PD->hasValidSignature())
      return;

    {
      // Check for circular inheritance within the protocol.
      SmallVector<ProtocolDecl *, 8> path;
      path.push_back(PD);
      checkCircularity(TC, PD, diag::circular_protocol_def,
                       DescriptiveDeclKind::Protocol, path);

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

    AccessControlChecker::checkAccessControl(TC, PD);
    UsableFromInlineChecker::checkUsableFromInline(TC, PD);

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

    // Explicitly calculate this bit.
    (void) PD->existentialTypeSupported(&TC);
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

    // We get bogus errors here with generic subscript materializeForSet.
    if (!FD->isInvalid()) {
      if (!isa<AccessorDecl>(FD) ||
          !cast<AccessorDecl>(FD)->isMaterializeForSet()) {
        TC.checkReferencedGenericParams(FD);
        TC.checkProtocolSelfRequirements(FD);
      }
    }

    AccessControlChecker::checkAccessControl(TC, FD);
    UsableFromInlineChecker::checkUsableFromInline(TC, FD);

    if (!checkOverrides(FD)) {
      // If a method has an 'override' keyword but does not
      // override anything, complain.
      if (auto *OA = FD->getAttrs().getAttribute<OverrideAttr>()) {
        if (!FD->getOverriddenDecl()) {
          TC.diagnose(FD, diag::method_does_not_override)
            .highlight(OA->getLocation());
          OA->setInvalid();
        }
      }
    }

    if (FD->hasBody()) {
      // Record the body.
      TC.definedFunctions.push_back(FD);
    } else if (requiresDefinition(FD)) {
      // Complain if we should have a body.
      TC.diagnose(FD->getLoc(), diag::func_decl_without_brace);
    }
  }

  void visitModuleDecl(ModuleDecl *) { }

  void visitEnumCaseDecl(EnumCaseDecl *ECD) {
    // The type-checker doesn't care about how these are grouped.
  }

  void visitEnumElementDecl(EnumElementDecl *EED) {
    TC.checkDeclAttributesEarly(EED);

    TC.validateDecl(EED);
    TC.checkDeclAttributes(EED);

    AccessControlChecker::checkAccessControl(TC, EED);
    UsableFromInlineChecker::checkUsableFromInline(TC, EED);
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

      AccessControlChecker ACC(TC);
      ACC.checkGenericParamAccess(ED->getGenericParams(), ED,
                                  desiredAccessScope, access);
    }

    // Trigger the creation of all of the conformances associated with this
    // nominal type.
    // FIXME: This is a hack to make sure that the type checker precomputes
    // enough information for later passes that might query conformances.
    if (auto nominal = ED->getAsNominalTypeOrNominalTypeExtensionContext())
      (void)nominal->getAllConformances();
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

    if (!CD->isInvalid()) {
      TC.checkReferencedGenericParams(CD);
      TC.checkProtocolSelfRequirements(CD);
    }

    // Check whether this initializer overrides an initializer in its
    // superclass.
    if (!checkOverrides(CD)) {
      // If an initializer has an override attribute but does not override
      // anything or overrides something that doesn't need an 'override'
      // keyword (e.g., a convenience initializer), complain.
      // anything, or overrides something that complain.
      if (auto *attr = CD->getAttrs().getAttribute<OverrideAttr>()) {
        if (!CD->getOverriddenDecl()) {
          TC.diagnose(CD, diag::initializer_does_not_override)
            .highlight(attr->getLocation());
          attr->setInvalid();
        } else if (attr->isImplicit()) {
          // Don't diagnose implicit attributes.
        } else if (!overrideRequiresKeyword(CD->getOverriddenDecl())) {
          // Special case: we are overriding a 'required' initializer, so we
          // need (only) the 'required' keyword.
          if (cast<ConstructorDecl>(CD->getOverriddenDecl())->isRequired()) {
            if (CD->getAttrs().hasAttribute<RequiredAttr>()) {
              TC.diagnose(CD, diag::required_initializer_override_keyword)
                .fixItRemove(attr->getLocation());
            } else {
              TC.diagnose(CD, diag::required_initializer_override_wrong_keyword)
                .fixItReplace(attr->getLocation(), "required");
              CD->getAttrs().add(
                new (TC.Context) RequiredAttr(/*IsImplicit=*/true));
            }

            TC.diagnose(findNonImplicitRequiredInit(CD->getOverriddenDecl()),
                        diag::overridden_required_initializer_here);
          } else {
            // We tried to override a convenience initializer.
            TC.diagnose(CD, diag::initializer_does_not_override)
              .highlight(attr->getLocation());
            TC.diagnose(CD->getOverriddenDecl(),
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
        TC.diagnose(CD, diag::failable_initializer_override,
                    CD->getFullName());
        TC.diagnose(CD->getOverriddenDecl(),
                    diag::nonfailable_initializer_override_here,
                    CD->getOverriddenDecl()->getFullName());
      }
    }

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

    AccessControlChecker::checkAccessControl(TC, CD);
    UsableFromInlineChecker::checkUsableFromInline(TC, CD);

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

/// Validate the underlying type of the given typealias.
static void validateTypealiasType(TypeChecker &tc, TypeAliasDecl *typeAlias) {
  TypeResolutionOptions options = TypeResolutionFlags::TypeAliasUnderlyingType;
  if (!typeAlias->getDeclContext()->isCascadingContextForLookup(
        /*functionsAreNonCascading*/true)) {
     options |= TypeResolutionFlags::KnownNonCascadingDependency;
  }

  // This can happen when code completion is attempted inside
  // of typealias underlying type e.g. `typealias F = () -> Int#^TOK^#`
  auto underlyingType = typeAlias->getUnderlyingTypeLoc();
  if (underlyingType.isNull()) {
    typeAlias->getUnderlyingTypeLoc().setInvalidType(tc.Context);
    typeAlias->setInterfaceType(ErrorType::get(tc.Context));
    typeAlias->setInvalid();
    return;
  }

  if (tc.validateType(typeAlias->getUnderlyingTypeLoc(),
                      typeAlias, options)) {
    typeAlias->setInvalid();
    typeAlias->getUnderlyingTypeLoc().setInvalidType(tc.Context);
  }

  typeAlias->setUnderlyingType(typeAlias->getUnderlyingTypeLoc().getType());
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
  if (FD->isInvalid()) return;

  auto *DC = FD->getDeclContext();
  auto selfNominal = DC->getAsNominalTypeOrNominalTypeExtensionContext();
  if (!selfNominal) return;

  // Check the parameters for a reference to 'Self'.
  bool isProtocol = isa<ProtocolDecl>(selfNominal);
  for (auto param : *FD->getParameters()) {
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

bool checkDynamicSelfReturn(FuncDecl *func,
                            TypeRepr *typeRepr,
                            unsigned optionalDepth) {
  // Look through parentheses.
  if (auto parenRepr = dyn_cast<TupleTypeRepr>(typeRepr)) {
    if (!parenRepr->isParenType()) return false;
    return checkDynamicSelfReturn(func, parenRepr->getElementType(0),
                                  optionalDepth);
  }

  // Look through attributes.
  if (auto attrRepr = dyn_cast<AttributedTypeRepr>(typeRepr)) {
    TypeAttributes attrs = attrRepr->getAttrs();
    if (!attrs.empty())
      return false;
    return checkDynamicSelfReturn(func, attrRepr->getTypeRepr(),
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
    return checkDynamicSelfReturn(func, base, optionalDepth + 1);
  }

  // Check whether we have a simple identifier type.
  auto simpleRepr = dyn_cast<SimpleIdentTypeRepr>(typeRepr);
  if (!simpleRepr)
    return false;

  // Check whether it is 'Self'.
  if (simpleRepr->getIdentifier() != func->getASTContext().Id_Self)
    return false;

  // Note that the function has a dynamic Self return type and set
  // the return type component to the dynamic self type.
  return true;
}

/// Check for methods that return 'DynamicResult'.
bool checkDynamicSelfReturn(FuncDecl *func) {
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

  return checkDynamicSelfReturn(func, typeRepr, 0);
}

Type buildAddressorResultType(TypeChecker &TC,
                              AccessorDecl *addressor,
                              Type valueType) {
  assert(addressor->getAccessorKind() == AccessorKind::Address ||
         addressor->getAccessorKind() == AccessorKind::MutableAddress);

  Type pointerType =
    (addressor->getAccessorKind() == AccessorKind::Address)
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

  (void) D->getFormalAccess();

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

  // Validating the parent may have triggered validation of this declaration,
  // so just return if that was the case.
  if (D->hasValidationStarted()) {
    assert(D->hasValidSignature());
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

    DeclValidationRAII IBV(assocType);

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
          diagnose(assocType, diag::kind_declared_here, DescriptiveDeclKind::Type);
        }
      }
    }
    // Finally, set the interface type.
    if (!assocType->hasInterfaceType())
      assocType->computeType();

    break;
  }

  case DeclKind::TypeAlias: {
    auto typeAlias = cast<TypeAliasDecl>(D);
    // Check generic parameters, if needed.
    DeclValidationRAII IBV(typeAlias);

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
    DeclValidationRAII IBV(nominal);
    validateGenericTypeSignature(nominal);
    nominal->setSignatureIsValidated();

    validateAttributes(*this, D);

    if (auto CD = dyn_cast<ClassDecl>(nominal)) {
      // Determine whether we require in-class initializers.
      if (CD->getAttrs().hasAttribute<RequiresStoredPropertyInitsAttr>() ||
          (CD->hasSuperclass() &&
           CD->getSuperclassDecl()->requiresStoredPropertyInits()))
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
    DeclValidationRAII IBV(proto);
    validateGenericTypeSignature(proto);
    proto->setSignatureIsValidated();

    // See the comment in validateDeclForNameLookup(); we may have validated
    // the alias before we built the protocol's generic environment.
    //
    // FIXME: Hopefully this can all go away with the ITC.
    for (auto member : proto->getMembers()) {
      if (auto *aliasDecl = dyn_cast<TypeAliasDecl>(member)) {
        if (!aliasDecl->isGeneric()) {
          aliasDecl->setGenericEnvironment(proto->getGenericEnvironment());

          // The generic environment didn't exist until now, we may have
          // unresolved types we will need to deal with, and need to record the
          // appropriate substitutions for that environment. Wipe out the types
          // and validate them again.
          aliasDecl->getUnderlyingTypeLoc().setType(Type());
          aliasDecl->setInterfaceType(Type());

          (void) aliasDecl->getFormalAccess();

          // Check generic parameters, if needed.
          if (aliasDecl->hasValidationStarted()) {
            validateTypealiasType(*this, aliasDecl);
          } else {
            DeclValidationRAII IBV(aliasDecl);
            validateTypealiasType(*this, aliasDecl);
          }
        }
      }
    }

    // Record inherited protocols.
    resolveInheritedProtocols(proto);
    resolveTrailingWhereClause(proto);

    validateAttributes(*this, D);

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
    auto *PD = cast<ParamDecl>(D);
    if (!PD->hasInterfaceType()) {
      // Can't fallthough because parameter without a type doesn't have
      // valid signature, but that shouldn't matter anyway.
      return;
    }

    auto type = PD->getInterfaceType();
    if (type->hasError())
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
        VD->setValidationToChecked();
        VD->markInvalid();
      }

      break;
    }

    // If we're already checking our PatternBindingDecl, bail out
    // without setting our own 'is being validated' flag, since we
    // will attempt validation again later.
    if (PBD->isBeingValidated())
      return;

    DeclValidationRAII IBV(D);

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
    D->setSignatureIsValidated();

    checkDeclAttributesEarly(VD);
    validateAttributes(*this, VD);

    // Properties need some special validation logic.
    if (auto *nominalDecl = VD->getDeclContext()
            ->getAsNominalTypeOrNominalTypeExtensionContext()) {
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
    }

    // Perform accessor-related validation.
    validateAbstractStorageDecl(*this, VD);

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

    DeclValidationRAII IBV(FD);

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
            diagnose(FD, diag::class_func_not_in_class, false)
                .fixItReplace(FD->getStaticLoc(), "static");
            diagnose(NTD, diag::extended_type_declared_here);
          }
        }
      }
    }

    validateSelfAccessKind(*this, FD);

    // Check whether the return type is dynamic 'Self'.
    FD->setDynamicSelf(checkDynamicSelfReturn(FD));

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

      auto valueParams = accessor->getParameters();

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
          accessorParam->getTypeLoc().setType(paramIfaceTy);
        }
      }

      // Propagate the value type into the correct position.
      switch (accessor->getAccessorKind()) {
      // For getters, set the result type to the value type.
      case AccessorKind::Get:
        accessor->getBodyResultTypeLoc().setType(valueIfaceTy);
        break;

      // For setters and observers, set the old/new value parameter's type
      // to the value type.
      case AccessorKind::DidSet:
      case AccessorKind::WillSet:
        // Make sure that observing accessors are marked final if in a class.
        if (!accessor->isFinal() &&
            accessor->getDeclContext()->getAsClassOrClassExtensionContext()) {
          makeFinal(Context, accessor);
        }
        LLVM_FALLTHROUGH;

      case AccessorKind::Set: {
        auto newValueParam = valueParams->get(0);
        newValueParam->setType(valueTy);
        newValueParam->setInterfaceType(valueIfaceTy);
        newValueParam->getTypeLoc().setType(valueTy);
        break;
      }

      // Addressor result types can get complicated because of the owner.
      case AccessorKind::Address:
      case AccessorKind::MutableAddress:
        if (Type resultType =
              buildAddressorResultType(*this, accessor, valueIfaceTy)) {
          accessor->getBodyResultTypeLoc().setType(resultType);
        }
        break;

      // These don't mention the value type directly.
      // If we add yield types to the function type, we'll need to update this.
      case AccessorKind::MaterializeForSet:
      case AccessorKind::Read:
      case AccessorKind::Modify:
        break;
      }
    }

    // Before anything else, set up the 'self' argument correctly if present.
    if (FD->getDeclContext()->isTypeContext())
      configureImplicitSelf(*this, FD);

    // If we have generic parameters, check the generic signature now.
    if (FD->getGenericParams() || !isa<AccessorDecl>(FD)) {
      validateGenericFuncSignature(FD);
      recordParamContextTypes(FD);
    } else {
      // We've inherited all of the type information already.
      FD->setGenericEnvironment(
        FD->getDeclContext()->getGenericEnvironmentOfContext());

      FD->computeType();

      if (FD->getInterfaceType()->hasError()) {
        FD->setInterfaceType(ErrorType::get(Context));
        FD->setInvalid();
      }
    }

    if (!isa<AccessorDecl>(FD) || cast<AccessorDecl>(FD)->isGetter()) {
      auto *TyR = getTypeLocForFunctionResult(FD).getTypeRepr();
      if (TyR && TyR->getKind() == TypeReprKind::ImplicitlyUnwrappedOptional) {
        auto &C = FD->getASTContext();
        FD->getAttrs().add(
            new (C) ImplicitlyUnwrappedOptionalAttr(/* implicit= */ true));
      }
    }

    // We want the function to be available for name lookup as soon
    // as it has a valid interface type.
    FD->setSignatureIsValidated();

    if (FD->isInvalid())
      break;

    validateAttributes(*this, FD);

    // Member functions need some special validation logic.
    if (FD->getDeclContext()->isTypeContext()) {
      if (FD->isOperator())
        checkMemberOperator(*this, FD);

      auto accessor = dyn_cast<AccessorDecl>(FD);

      if (accessor && accessor->isGetterOrSetter()) {
        // If the property decl is an instance property, its accessors will
        // be instance methods and the above condition will mark them ObjC.
        // The only additional condition we need to check is if the var decl
        // had an @objc or @iboutlet property.

        AbstractStorageDecl *storage = accessor->getStorage();

        // If the storage is final, propagate to this accessor.
        if (storage->isFinal())
          makeFinal(Context, FD);
      }

      inferFinalAndDiagnoseIfNeeded(*this, FD, FD->getStaticSpelling());
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

    DeclValidationRAII IBV(CD);

    checkDeclAttributesEarly(CD);

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

    configureImplicitSelf(*this, CD);

    validateGenericFuncSignature(CD);
    recordParamContextTypes(CD);

    // We want the constructor to be available for name lookup as soon
    // as it has a valid interface type.
    CD->setSignatureIsValidated();

    validateAttributes(*this, CD);

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

    DeclValidationRAII IBV(DD);

    assert(DD->getDeclContext()->isTypeContext()
           && "Decl parsing must prevent destructors outside of types!");

    checkDeclAttributesEarly(DD);
    DD->copyFormalAccessFrom(enclosingClass, /*sourceIsParentContext*/true);

    configureImplicitSelf(*this, DD);

    validateGenericFuncSignature(DD);
    recordParamContextTypes(DD);

    DD->setSignatureIsValidated();

    validateAttributes(*this, DD);
    break;
  }

  case DeclKind::Subscript: {
    auto *SD = cast<SubscriptDecl>(D);

    DeclValidationRAII IBV(SD);

    validateGenericSubscriptSignature(SD);
    recordIndexContextTypes(SD);

    SD->setSignatureIsValidated();

    checkDeclAttributesEarly(SD);

    validateAttributes(*this, SD);

    auto *TyR = SD->getElementTypeLoc().getTypeRepr();
    if (TyR && TyR->getKind() == TypeReprKind::ImplicitlyUnwrappedOptional) {
      auto &C = SD->getASTContext();
      SD->getAttrs().add(
          new (C) ImplicitlyUnwrappedOptionalAttr(/* implicit= */ true));
    }

    // Member subscripts need some special validation logic.
    if (SD->getDeclContext()->isTypeContext()) {
      // If this is a class member, mark it final if the class is final.
      inferFinalAndDiagnoseIfNeeded(*this, SD, StaticSpellingKind::None);
    }

    // Perform accessor-related validation.
    validateAbstractStorageDecl(*this, SD);

    break;
  }

  case DeclKind::EnumElement: {
    auto *EED = cast<EnumElementDecl>(D);
    EnumDecl *ED = EED->getParentEnum();

    validateAttributes(*this, EED);

    DeclValidationRAII IBV(EED);

    if (auto *PL = EED->getParameterList()) {
      CompleteGenericTypeResolver resolver(*this, ED->getGenericSignature());

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

    // Now that we have an argument type we can set the element's declared
    // type.
    if (!EED->isInvalid())
      EED->computeType();

    EED->setSignatureIsValidated();

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

    (void) proto->getFormalAccess();

    // Record inherited protocols.
    resolveInheritedProtocols(proto);
    resolveTrailingWhereClause(proto);

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
    (void) assocType->getFormalAccess();
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

        auto helper = [&] {
          (void) typealias->getFormalAccess();

          ProtocolRequirementTypeResolver resolver;
          TypeResolutionOptions options =
            TypeResolutionFlags::TypeAliasUnderlyingType;
          if (validateType(typealias->getUnderlyingTypeLoc(),
                           typealias, options, &resolver)) {
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
        };

        if (typealias->hasValidationStarted()) {
          helper();
        } else {
          DeclValidationRAII IBV(typealias);
          helper();
        }

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

  if (auto ext = dyn_cast<ExtensionDecl>(dc)) {
    if (ext->getAsClassOrClassExtensionContext()) {
      // Finalize members of class extensions, to ensure we compute their
      // @objc and dynamic state.
      DeclsToFinalize.insert(member);
    }
  }

  // If this represents (abstract) storage, form the appropriate accessors.
  if (auto storage = dyn_cast<AbstractStorageDecl>(member)) {
    validateAbstractStorageDecl(*this, storage);

    // Request layout of the accessors for an @objc declaration.
    // We can't delay validation of getters and setters on @objc properties,
    // because if they never get validated at all then conformance checkers
    // will complain about selector mismatches.
    if (storage->isObjC()) {
      for (auto accessor : storage->getAllAccessors()) {
        requestMemberLayout(accessor);
      }
    }
  }
}

void TypeChecker::requestNominalLayout(NominalTypeDecl *nominalDecl) {
  if (isa<SourceFile>(nominalDecl->getModuleScopeContext()))
    DeclsToFinalize.insert(nominalDecl);
}

void TypeChecker::requestSuperclassLayout(ClassDecl *classDecl) {
  if (auto *superclassDecl = classDecl->getSuperclassDecl()) {
    if (superclassDecl)
      requestNominalLayout(superclassDecl);
  }
}

/// "Finalize" the type so that SILGen can make copies of it, call
/// methods on it, etc. This requires forcing enough computation so
/// that (for example) a class can layout its vtable or a struct can
/// be laid out in memory.
static void finalizeType(TypeChecker &TC, NominalTypeDecl *nominal) {
  assert(!nominal->hasClangNode());
  assert(isa<SourceFile>(nominal->getModuleScopeContext()));

  if (auto *CD = dyn_cast<ClassDecl>(nominal)) {
    // We need to add implicit initializers and dtors because it
    // affects vtable layout.
    TC.addImplicitConstructors(CD);
    CD->addImplicitDestructor();
  }

  for (auto *D : nominal->getMembers()) {
    auto VD = dyn_cast<ValueDecl>(D);
    if (!VD)
      continue;

    if (!shouldValidateMemberDuringFinalization(nominal, VD))
      continue;

    TC.DeclsToFinalize.insert(VD);

    // The only thing left to do is synthesize storage for lazy variables.
    auto *prop = dyn_cast<VarDecl>(D);
    if (!prop)
      continue;

    if (prop->getAttrs().hasAttribute<LazyAttr>() && !prop->isStatic() &&
        (!prop->getGetter() || !prop->getGetter()->hasBody())) {
      finalizeAbstractStorageDecl(TC, prop);
      TC.completeLazyVarImplementation(prop);
    }
  }

  if (auto *CD = dyn_cast<ClassDecl>(nominal)) {
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
  validateDecl(decl);

  if (auto nominal = dyn_cast<NominalTypeDecl>(decl)) {
    finalizeType(*this, nominal);
  } else if (auto storage = dyn_cast<AbstractStorageDecl>(decl)) {
    finalizeAbstractStorageDecl(*this, storage);
  }

  // Compute overrides.
  (void)decl->getOverriddenDecls();

  // Check whether the member is @objc or dynamic.
  (void)decl->isObjC();
  (void)decl->isDynamic();
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
      subMap = SubstitutionMap::get(
          typealiasSig,
          [](SubstitutableType *type) -> Type {
            return Type(type);
          },
          MakeAbstractConformanceForGenericType());

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

  DeclValidationRAII IBV(ext);

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

  SmallVector<std::pair<ValueDecl *, Type>, 4> declaredInitializers;
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

        if (!ctor->isInvalid()) {
          auto type = getMemberTypeForComparison(Context, ctor, nullptr,
                                                 /*stripLabels*/ false);
          declaredInitializers.push_back({ctor, type});
        }

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
  if (Type superclassTy = classDecl->getSuperclass()) {
    bool canInheritInitializers = (!SuppressDefaultInitializer &&
                                   !FoundDesignatedInit);

    // We can't define these overrides if we have any uninitialized
    // stored properties.
    if (SuppressDefaultInitializer && !FoundDesignatedInit &&
        !classDecl->hasClangNode()) {
      return;
    }

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

      bool alreadyDeclared = false;
      for (const auto &ctorAndType : declaredInitializers) {
        auto *ctor = ctorAndType.first;
        auto type = ctorAndType.second;
        auto parentType = getMemberTypeForComparison(
            Context, superclassCtor, ctor, /*stripLabels*/ false);

        if (isOverrideBasedOnType(ctor, type, superclassCtor, parentType)) {
          alreadyDeclared = true;
          break;
        }
      }

      // If we have already introduced an initializer with this parameter type,
      // don't add one now.
      if (alreadyDeclared)
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
    if (!protocol)
      return false;

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
    if (member.isCompoundName() && argumentNames.size() != 1)
      return;

    if (baseName == DeclBaseName::createConstructor() &&
        (member.isSimpleName() || argumentNames.front() == Context.Id_from)) {
      // init(from:) may be synthesized as part of derived conformance to the
      // Decodable protocol.
      // If the target should conform to the Decodable protocol, check the
      // conformance here to attempt synthesis.
      auto *decodableProto = Context.getProtocol(KnownProtocolKind::Decodable);
      (void)evaluateTargetConformanceTo(decodableProto);
    } else if (!baseName.isSpecial() &&
               baseName.getIdentifier() == Context.Id_encode &&
               (member.isSimpleName() ||
                argumentNames.front() == Context.Id_to)) {
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
    if (auto superTy = decl->getDeclaredInterfaceType()->getSuperclass()) {
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
        auto params = func->getParameters();
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
