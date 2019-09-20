//===--- TypeCheckDecl.cpp - Type Checking for Declarations ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
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
#include "TypeCheckDecl.h"
#include "TypeCheckAvailability.h"
#include "TypeCheckObjC.h"
#include "TypeCheckType.h"
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
#include "swift/AST/Initializer.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/PropertyWrappers.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/ReferencedNameTracker.h"
#include "swift/AST/SourceFile.h"
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
#include "llvm/Support/DJB.h"

using namespace swift;

#define DEBUG_TYPE "Serialization"

STATISTIC(NumLazyRequirementSignaturesLoaded,
          "# of lazily-deserialized requirement signatures loaded");

#undef DEBUG_TYPE
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
      return DenseMapInfo<StringRef>::getHashValue(k.stringValue);
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

/// Check the inheritance clause of a type declaration or extension thereof.
///
/// This routine performs detailed checking of the inheritance clause of the
/// given type or extension. It need only be called within the primary source
/// file.
static void checkInheritanceClause(
                    llvm::PointerUnion<TypeDecl *, ExtensionDecl *> declUnion) {
  DeclContext *DC;
  MutableArrayRef<TypeLoc> inheritedClause;
  ExtensionDecl *ext = nullptr;
  TypeDecl *typeDecl = nullptr;
  Decl *decl;
  if ((ext = declUnion.dyn_cast<ExtensionDecl *>())) {
    decl = ext;
    DC = ext;

    inheritedClause = ext->getInherited();

    // Protocol extensions cannot have inheritance clauses.
    if (auto proto = ext->getExtendedProtocolDecl()) {
      if (!inheritedClause.empty()) {
        ext->diagnose(diag::extension_protocol_inheritance,
                 proto->getName())
          .highlight(SourceRange(inheritedClause.front().getSourceRange().Start,
                                 inheritedClause.back().getSourceRange().End));
        return;
      }
    }
  } else {
    typeDecl = declUnion.get<TypeDecl *>();
    decl = typeDecl;
    if (auto nominal = dyn_cast<NominalTypeDecl>(typeDecl)) {
      DC = nominal;
    } else {
      DC = typeDecl->getDeclContext();
    }

    inheritedClause = typeDecl->getInherited();
  }

  // Can this declaration's inheritance clause contain a class or
  // subclass existential?
  bool canHaveSuperclass = (isa<ClassDecl>(decl) ||
                            (isa<ProtocolDecl>(decl) &&
                             !cast<ProtocolDecl>(decl)->isObjC()));

  ASTContext &ctx = decl->getASTContext();
  auto &diags = ctx.Diags;

  // Retrieve the location of the start of the inheritance clause.
  auto getStartLocOfInheritanceClause = [&] {
    if (ext)
      return ext->getSourceRange().End;

    return typeDecl->getNameLoc();
  };

  // Compute the source range to be used when removing something from an
  // inheritance clause.
  auto getRemovalRange = [&](unsigned i) {
    // If there is just one entry, remove the entire inheritance clause.
    if (inheritedClause.size() == 1) {
      SourceLoc start = getStartLocOfInheritanceClause();
      SourceLoc end = inheritedClause[i].getSourceRange().End;
      return SourceRange(Lexer::getLocForEndOfToken(ctx.SourceMgr, start),
                         Lexer::getLocForEndOfToken(ctx.SourceMgr, end));
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
      Lexer::getLocForEndOfToken(ctx.SourceMgr,
                                 inheritedClause[i-1].getSourceRange().End);

    SourceLoc afterMyEndLoc =
      Lexer::getLocForEndOfToken(ctx.SourceMgr,
                                 inheritedClause[i].getSourceRange().End);

    return SourceRange(afterPriorLoc, afterMyEndLoc);
  };

  // Check all of the types listed in the inheritance clause.
  Type superclassTy;
  SourceRange superclassRange;
  Optional<std::pair<unsigned, SourceRange>> inheritedAnyObject;
  for (unsigned i = 0, n = inheritedClause.size(); i != n; ++i) {
    auto &inherited = inheritedClause[i];

    // Validate the type.
    InheritedTypeRequest request{declUnion, i, TypeResolutionStage::Interface};
    Type inheritedTy = evaluateOrDefault(ctx.evaluator, request, Type());

    // If we couldn't resolve an the inherited type, or it contains an error,
    // ignore it.
    if (!inheritedTy || inheritedTy->hasError())
      continue;

    // For generic parameters and associated types, the GSB checks constraints;
    // however, we still want to fire off the requests to produce diagnostics
    // in some circular validation cases.
    if (isa<AbstractTypeParamDecl>(decl))
      continue;

    // Check whether we inherited from 'AnyObject' twice.
    // Other redundant-inheritance scenarios are checked below, the
    // GenericSignatureBuilder (for protocol inheritance) or the
    // ConformanceLookupTable (for protocol conformance).
    if (inheritedTy->isAnyObject()) {
      if (inheritedAnyObject) {
        // If the first occurrence was written as 'class', downgrade the error
        // to a warning in such case for backward compatibility with
        // Swift <= 4.
        auto knownIndex = inheritedAnyObject->first;
        auto knownRange = inheritedAnyObject->second;
        SourceRange removeRange = getRemovalRange(knownIndex);
        if (!ctx.LangOpts.isSwiftVersionAtLeast(5) &&
            (isa<ProtocolDecl>(decl) || isa<AbstractTypeParamDecl>(decl)) &&
            Lexer::getTokenAtLocation(ctx.SourceMgr, knownRange.Start)
              .is(tok::kw_class)) {
          SourceLoc classLoc = knownRange.Start;

          diags.diagnose(classLoc, diag::duplicate_anyobject_class_inheritance)
            .fixItRemoveChars(removeRange.Start, removeRange.End);
        } else {
          diags.diagnose(inherited.getSourceRange().Start,
                 diag::duplicate_inheritance, inheritedTy)
            .fixItRemoveChars(removeRange.Start, removeRange.End);
        }
        continue;
      }

      // Note that we saw inheritance from 'AnyObject'.
      inheritedAnyObject = { i, inherited.getSourceRange() };
    }

    if (inheritedTy->isExistentialType()) {
      auto layout = inheritedTy->getExistentialLayout();

      // Subclass existentials are not allowed except on classes and
      // non-@objc protocols.
      if (layout.explicitSuperclass &&
          !canHaveSuperclass) {
        decl->diagnose(diag::inheritance_from_protocol_with_superclass,
                       inheritedTy);
        continue;
      }

      // AnyObject is not allowed except on protocols.
      if (layout.hasExplicitAnyObject &&
          !isa<ProtocolDecl>(decl)) {
        decl->diagnose(canHaveSuperclass
                       ? diag::inheritance_from_non_protocol_or_class
                       : diag::inheritance_from_non_protocol,
                       inheritedTy);
        continue;
      }

      // If the existential did not have a class constraint, we're done.
      if (!layout.explicitSuperclass)
        continue;

      // Classes and protocols can inherit from subclass existentials.
      // For classes, we check for a duplicate superclass below.
      // For protocols, the GSB emits its own warning instead.
      if (isa<ProtocolDecl>(decl))
        continue;

      assert(isa<ClassDecl>(decl));
      assert(canHaveSuperclass);
      inheritedTy = layout.explicitSuperclass;
    }

    // If this is an enum inheritance clause, check for a raw type.
    if (isa<EnumDecl>(decl)) {
      // Check if we already had a raw type.
      if (superclassTy) {
        if (superclassTy->isEqual(inheritedTy)) {
          auto removeRange = getRemovalRange(i);
          diags.diagnose(inherited.getSourceRange().Start,
                         diag::duplicate_inheritance, inheritedTy)
            .fixItRemoveChars(removeRange.Start, removeRange.End);
        } else {
          diags.diagnose(inherited.getSourceRange().Start,
                         diag::multiple_enum_raw_types, superclassTy,
                         inheritedTy)
            .highlight(superclassRange);
        }
        continue;
      }
      
      // If this is not the first entry in the inheritance clause, complain.
      if (i > 0) {
        auto removeRange = getRemovalRange(i);

        diags.diagnose(inherited.getSourceRange().Start,
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

    // If this is a class type, it may be the superclass. We end up here when
    // the inherited type is either itself a class, or when it is a subclass
    // existential via the existential type path above.
    if (inheritedTy->getClassOrBoundGenericClass()) {
      // First, check if we already had a superclass.
      if (superclassTy) {
        // FIXME: Check for shadowed protocol names, i.e., NSObject?

        if (superclassTy->isEqual(inheritedTy)) {
          // Duplicate superclass.
          auto removeRange = getRemovalRange(i);
          diags.diagnose(inherited.getSourceRange().Start,
                         diag::duplicate_inheritance, inheritedTy)
            .fixItRemoveChars(removeRange.Start, removeRange.End);
        } else {
          // Complain about multiple inheritance.
          // Don't emit a Fix-It here. The user has to think harder about this.
          diags.diagnose(inherited.getSourceRange().Start,
                         diag::multiple_inheritance, superclassTy, inheritedTy)
            .highlight(superclassRange);
        }
        continue;
      }

      // If this is not the first entry in the inheritance clause, complain.
      if (isa<ClassDecl>(decl) && i > 0) {
        auto removeRange = getRemovalRange(i);
        diags.diagnose(inherited.getSourceRange().Start,
                       diag::superclass_not_first, inheritedTy)
          .fixItRemoveChars(removeRange.Start, removeRange.End)
          .fixItInsert(inheritedClause[0].getSourceRange().Start,
                       inheritedTy.getString() + ", ");

        // Fall through to record the superclass.
      }

      if (canHaveSuperclass) {
        // Record the superclass.
        superclassTy = inheritedTy;
        superclassRange = inherited.getSourceRange();
        continue;
      }
    }

    // We can't inherit from a non-class, non-protocol type.
    decl->diagnose(canHaveSuperclass
                   ? diag::inheritance_from_non_protocol_or_class
                   : diag::inheritance_from_non_protocol,
                   inheritedTy);
    // FIXME: Note pointing to the declaration 'inheritedTy' references?
  }
}

/// Check the inheritance clauses generic parameters along with any
/// requirements stored within the generic parameter list.
static void checkGenericParams(GenericParamList *genericParams,
                               DeclContext *owningDC, TypeChecker &tc) {
  if (!genericParams)
    return;

  for (auto gp : *genericParams) {
    tc.checkDeclAttributes(gp);
    checkInheritanceClause(gp);
  }

  // Force visitation of each of the requirements here.
  WhereClauseOwner(owningDC, genericParams)
      .visitRequirements(TypeResolutionStage::Interface,
                         [](Requirement, RequirementRepr *) { return false; });
}

/// Retrieve the set of protocols the given protocol inherits.
static llvm::TinyPtrVector<ProtocolDecl *>
getInheritedForCycleCheck(TypeChecker &tc,
                          ProtocolDecl *proto,
                          ProtocolDecl **scratch) {
  TinyPtrVector<ProtocolDecl *> result;

  bool anyObject = false;
  for (const auto &found :
         getDirectlyInheritedNominalTypeDecls(proto, anyObject)) {
    if (auto protoDecl = dyn_cast<ProtocolDecl>(found.second))
      result.push_back(protoDecl);
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
      tc.diagnose(*i, diag::kind_declname_declared_here,
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

/// Expose TypeChecker's handling of GenericParamList to SIL parsing.
GenericEnvironment *
TypeChecker::handleSILGenericParams(GenericParamList *genericParams,
                                    DeclContext *DC) {
  if (genericParams == nullptr)
    return nullptr;

  SmallVector<GenericParamList *, 2> nestedList;
  for (; genericParams; genericParams = genericParams->getOuterParameters()) {
    nestedList.push_back(genericParams);
  }

  std::reverse(nestedList.begin(), nestedList.end());

  for (unsigned i = 0, e = nestedList.size(); i < e; ++i) {
    auto genericParams = nestedList[i];
    genericParams->setDepth(i);
  }

  auto *sig = TypeChecker::checkGenericSignature(
             nestedList.back(), DC,
             /*parentSig=*/nullptr,
             /*allowConcreteGenericParams=*/true);
  return (sig ? sig->getGenericEnvironment() : nullptr);
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
  bool isCascading = (current->getFormalAccess() > AccessLevel::FilePrivate);

  // Find other potential definitions.
  SmallVector<ValueDecl *, 4> otherDefinitions;
  if (currentDC->isTypeContext()) {
    // Look within a type context.
    if (auto nominal = currentDC->getSelfNominalTypeDecl()) {
      auto found = nominal->lookupDirect(current->getBaseName());
      otherDefinitions.append(found.begin(), found.end());
      if (tracker)
        tracker->addUsedMember({nominal, current->getBaseName()}, isCascading);
    }
  } else {
    // Look within a module context.
    currentFile->getParentModule()->lookupValue(current->getBaseName(),
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
            (currentInit->isFailable() !=
             otherInit->isFailable())) {
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
          Optional<llvm::VersionTuple> introduced;
          Optional<llvm::VersionTuple> obsoleted;

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
        const auto *otherInit = dyn_cast<ConstructorDecl>(other);
        // Provide a better description for implicit initializers.
        if (otherInit && otherInit->isImplicit()) {
          // Skip conflicts with inherited initializers, which only happen
          // when the current declaration is within an extension. The override
          // checker should have already taken care of emitting a more
          // productive diagnostic.
          if (!other->getOverriddenDecl())
            tc.diagnose(current, diag::invalid_redecl_init,
                        current->getFullName(),
                        otherInit->isMemberwiseInitializer());
        } else {
          tc.diagnose(current, diag::invalid_redecl, current->getFullName());
          tc.diagnose(other, diag::invalid_redecl_prev, other->getFullName());
        }
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

static bool inferFinalAndDiagnoseIfNeeded(ValueDecl *D, ClassDecl *cls,
                                          StaticSpellingKind staticSpelling) {
  // Are there any reasons to infer 'final'? Prefer 'static' over the class
  // being final for the purposes of diagnostics.
  Optional<ImplicitlyFinalReason> reason;
  if (staticSpelling == StaticSpellingKind::KeywordStatic) {
    reason = ImplicitlyFinalReason::Static;

    if (auto finalAttr = D->getAttrs().getAttribute<FinalAttr>()) {
      auto finalRange = finalAttr->getRange();
      if (finalRange.isValid()) {
        auto &context = D->getASTContext();
        context.Diags.diagnose(finalRange.Start, diag::static_decl_already_final)
        .fixItRemove(finalRange);
      }
    }
  } else if (cls->isFinal()) {
    reason = ImplicitlyFinalReason::FinalClass;
  }

  if (!reason)
    return false;

  if (D->getFormalAccess() == AccessLevel::Open) {
    auto &context = D->getASTContext();
    auto diagID = diag::implicitly_final_cannot_be_open;
    if (!context.isSwiftVersionAtLeast(5))
      diagID = diag::implicitly_final_cannot_be_open_swift4;
    auto inFlightDiag = context.Diags.diagnose(D, diagID,
                                    static_cast<unsigned>(reason.getValue()));
    fixItAccess(inFlightDiag, D, AccessLevel::Public);
  }

  return true;
}

/// Runtime-replacable accessors are dynamic when their storage declaration
/// is dynamic and they were explicitly defined or they are implicitly defined
/// getter/setter because no accessor was defined.
static bool doesAccessorNeedDynamicAttribute(AccessorDecl *accessor) {
  auto kind = accessor->getAccessorKind();
  auto storage = accessor->getStorage();
  bool isObjC = storage->isObjC();

  switch (kind) {
  case AccessorKind::Get: {
    auto readImpl = storage->getReadImpl();
    if (!isObjC &&
        (readImpl == ReadImplKind::Read || readImpl == ReadImplKind::Address))
      return false;
    return storage->isDynamic();
  }
  case AccessorKind::Set: {
    auto writeImpl = storage->getWriteImpl();
    if (!isObjC && (writeImpl == WriteImplKind::Modify ||
                    writeImpl == WriteImplKind::MutableAddress ||
                    writeImpl == WriteImplKind::StoredWithObservers))
      return false;
    return storage->isDynamic();
  }
  case AccessorKind::Read:
    if (!isObjC && storage->getReadImpl() == ReadImplKind::Read)
      return storage->isDynamic();
    return false;
  case AccessorKind::Modify: {
    if (!isObjC && storage->getWriteImpl() == WriteImplKind::Modify)
      return storage->isDynamic();
    return false;
  }
  case AccessorKind::MutableAddress: {
    if (!isObjC && storage->getWriteImpl() == WriteImplKind::MutableAddress)
      return storage->isDynamic();
    return false;
  }
  case AccessorKind::Address: {
    if (!isObjC && storage->getReadImpl() == ReadImplKind::Address)
      return storage->isDynamic();
    return false;
  }
  case AccessorKind::DidSet:
  case AccessorKind::WillSet:
    if (!isObjC &&
        storage->getWriteImpl() == WriteImplKind::StoredWithObservers)
      return storage->isDynamic();
    return false;
  }
  llvm_unreachable("covered switch");
}

llvm::Expected<CtorInitializerKind>
InitKindRequest::evaluate(Evaluator &evaluator, ConstructorDecl *decl) const {
  auto &diags = decl->getASTContext().Diags;

  // Convenience inits are only allowed on classes and in extensions thereof.
  if (decl->getAttrs().hasAttribute<ConvenienceAttr>()) {
    if (auto nominal = decl->getDeclContext()->getSelfNominalTypeDecl()) {
      auto classDecl = dyn_cast<ClassDecl>(nominal);

      // Forbid convenience inits on Foreign CF types, as Swift does not yet
      // support user-defined factory inits.
      if (classDecl &&
          classDecl->getForeignClassKind() == ClassDecl::ForeignKind::CFType) {
        diags.diagnose(decl->getLoc(), diag::cfclass_convenience_init);
      }

      if (!classDecl) {
        auto ConvenienceLoc =
          decl->getAttrs().getAttribute<ConvenienceAttr>()->getLocation();

        // Produce a tailored diagnostic for structs and enums.
        bool isStruct = dyn_cast<StructDecl>(nominal) != nullptr;
        if (isStruct || dyn_cast<EnumDecl>(nominal)) {
          diags.diagnose(decl->getLoc(), diag::enumstruct_convenience_init,
                         isStruct ? "structs" : "enums")
            .fixItRemove(ConvenienceLoc);
        } else {
          diags.diagnose(decl->getLoc(), diag::nonclass_convenience_init,
                         nominal->getName())
            .fixItRemove(ConvenienceLoc);
        }
        return CtorInitializerKind::Designated;
      }
    }

    return CtorInitializerKind::Convenience;

  } else if (auto nominal = decl->getDeclContext()->getSelfNominalTypeDecl()) {
    // A designated init for a class must be written within the class itself.
    //
    // This is because designated initializers of classes get a vtable entry,
    // and extensions cannot add vtable entries to the extended type.
    //
    // If we implement the ability for extensions defined in the same module
    // (or the same file) to add vtable entries, we can re-evaluate this
    // restriction.
    if (dyn_cast<ClassDecl>(nominal) &&
        !decl->isSynthesized() && isa<ExtensionDecl>(decl->getDeclContext()) &&
        !(decl->getAttrs().hasAttribute<DynamicReplacementAttr>())) {
      diags.diagnose(decl->getLoc(), diag::designated_init_in_extension,
                     nominal->getName())
        .fixItInsert(decl->getLoc(), "convenience ");
      return CtorInitializerKind::Convenience;
    }

    if (decl->getDeclContext()->getExtendedProtocolDecl()) {
      return CtorInitializerKind::Convenience;
    }
  }

  return CtorInitializerKind::Designated;
}

llvm::Expected<bool>
ProtocolRequiresClassRequest::evaluate(Evaluator &evaluator,
                                       ProtocolDecl *decl) const {
  // Quick check: @objc protocols require a class.
  if (decl->isObjC())
    return true;

  // Determine the set of nominal types that this protocol inherits.
  bool anyObject = false;
  auto allInheritedNominals =
    getDirectlyInheritedNominalTypeDecls(decl, anyObject);

  // Quick check: do we inherit AnyObject?
  if (anyObject)
    return true;

  // Look through all of the inherited nominals for a superclass or a
  // class-bound protocol.
  for (const auto found : allInheritedNominals) {
    // Superclass bound.
    if (isa<ClassDecl>(found.second))
      return true;

    // A protocol that might be class-constrained.
    if (auto proto = dyn_cast<ProtocolDecl>(found.second)) {
      if (proto->requiresClass())
        return true;
    }
  }

  return false;
}

llvm::Expected<bool>
ExistentialConformsToSelfRequest::evaluate(Evaluator &evaluator,
                                           ProtocolDecl *decl) const {
  // If it's not @objc, it conforms to itself only if it has a self-conformance
  // witness table.
  if (!decl->isObjC())
    return decl->requiresSelfConformanceWitnessTable();

  // Check whether this protocol conforms to itself.
  for (auto member : decl->getMembers()) {
    if (member->isInvalid()) continue;

    if (auto vd = dyn_cast<ValueDecl>(member)) {
      // A protocol cannot conform to itself if it has static members.
      if (!vd->isInstanceMember())
        return false;
    }
  }

  // Check whether any of the inherited protocols fail to conform to themselves.
  for (auto proto : decl->getInheritedProtocols()) {
    if (!proto->existentialConformsToSelf())
      return false;
  }

  return true;
}

llvm::Expected<bool>
ExistentialTypeSupportedRequest::evaluate(Evaluator &evaluator,
                                          ProtocolDecl *decl) const {
  // ObjC protocols can always be existential.
  if (decl->isObjC())
    return true;

  for (auto member : decl->getMembers()) {
    // Existential types cannot be used if the protocol has an associated type.
    if (isa<AssociatedTypeDecl>(member))
      return false;

    // For value members, look at their type signatures.
    if (auto valueMember = dyn_cast<ValueDecl>(member)) {
      if (!valueMember->hasInterfaceType())
        if (auto *resolver = decl->getASTContext().getLazyResolver())
          resolver->resolveDeclSignature(valueMember);

      if (!decl->isAvailableInExistential(valueMember))
        return false;
    }
  }

  // Check whether all of the inherited protocols support existential types.
  for (auto proto : decl->getInheritedProtocols()) {
    if (!proto->existentialTypeSupported())
      return false;
  }

  return true;
}

llvm::Expected<bool>
IsFinalRequest::evaluate(Evaluator &evaluator, ValueDecl *decl) const {
  if (isa<ClassDecl>(decl))
    return decl->getAttrs().hasAttribute<FinalAttr>();

  auto cls = decl->getDeclContext()->getSelfClassDecl();
  if (!cls)
    return false;

  switch (decl->getKind()) {
    case DeclKind::Var: {
      // Properties are final if they are declared 'static' or a 'let'
      auto *VD = cast<VarDecl>(decl);

      // Backing storage for 'lazy' or property wrappers is always final.
      if (VD->isLazyStorageProperty() ||
          VD->getOriginalWrappedProperty())
        return true;

      if (auto *nominalDecl = VD->getDeclContext()->getSelfClassDecl()) {
        // If this variable is a class member, mark it final if the
        // class is final, or if it was declared with 'let'.
        auto *PBD = VD->getParentPatternBinding();
        if (PBD && inferFinalAndDiagnoseIfNeeded(decl, cls, PBD->getStaticSpelling()))
          return true;

        if (VD->isLet()) {
          if (VD->getFormalAccess() == AccessLevel::Open) {
            auto &context = decl->getASTContext();
            auto diagID = diag::implicitly_final_cannot_be_open;
            if (!context.isSwiftVersionAtLeast(5))
              diagID = diag::implicitly_final_cannot_be_open_swift4;
            auto inFlightDiag =
              context.Diags.diagnose(decl, diagID,
                                     static_cast<unsigned>(ImplicitlyFinalReason::Let));
            fixItAccess(inFlightDiag, decl, AccessLevel::Public);
          }

          return true;
        }
      }

      break;
    }

    case DeclKind::Func: {
      // Methods declared 'static' are final.
      auto staticSpelling = cast<FuncDecl>(decl)->getStaticSpelling();
      if (inferFinalAndDiagnoseIfNeeded(decl, cls, staticSpelling))
        return true;
      break;
    }

    case DeclKind::Accessor:
      if (auto accessor = dyn_cast<AccessorDecl>(decl)) {
        switch (accessor->getAccessorKind()) {
          case AccessorKind::DidSet:
          case AccessorKind::WillSet:
            // Observing accessors are marked final if in a class.
            return true;

          case AccessorKind::Read:
          case AccessorKind::Modify:
          case AccessorKind::Get:
          case AccessorKind::Set: {
            // Coroutines and accessors are final if their storage is.
            auto storage = accessor->getStorage();
            if (storage->isFinal())
              return true;
            break;
          }

          default:
            break;
        }
      }
      break;

    case DeclKind::Subscript: {
      // Member subscripts.
      auto staticSpelling = cast<SubscriptDecl>(decl)->getStaticSpelling();
      if (inferFinalAndDiagnoseIfNeeded(decl, cls, staticSpelling))
        return true;
      break;
    }

    default:
      break;
  }

  if (decl->getAttrs().hasAttribute<FinalAttr>())
    return true;

  return false;
}

llvm::Expected<bool>
IsDynamicRequest::evaluate(Evaluator &evaluator, ValueDecl *decl) const {
  // If we can't infer dynamic here, don't.
  if (!DeclAttribute::canAttributeAppearOnDecl(DAK_Dynamic, decl))
    return false;

  // Add dynamic if -enable-implicit-dynamic was requested.
  TypeChecker::addImplicitDynamicAttribute(decl);

  // If 'dynamic' was explicitly specified, check it.
  if (decl->getAttrs().hasAttribute<DynamicAttr>()) {
    return true;
  }

  if (auto accessor = dyn_cast<AccessorDecl>(decl)) {
    // Runtime-replacable accessors are dynamic when their storage declaration
    // is dynamic and they were explicitly defined or they are implicitly defined
    // getter/setter because no accessor was defined.
    return doesAccessorNeedDynamicAttribute(accessor);
  }

  // The 'NSManaged' attribute implies 'dynamic'.
  // FIXME: Use a semantic check for NSManaged rather than looking for the
  // attribute (which could be ill-formed).
  if (decl->getAttrs().hasAttribute<NSManagedAttr>())
    return true;

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
  if (isa<ExtensionDecl>(dc) && dc->getSelfClassDecl())
    return true;

  // If any of the declarations overridden by this declaration are dynamic
  // or were imported from Objective-C, this declaration is dynamic.
  // Don't do this if the declaration is not exposed to Objective-C; that's
  // currently the (only) manner in which one can make an override of a
  // dynamic declaration non-dynamic.
  auto overriddenDecls = evaluateOrDefault(evaluator,
    OverriddenDeclsRequest{decl}, {});
  for (auto overridden : overriddenDecls) {
    if (overridden->isDynamic() || overridden->hasClangNode())
      return true;
  }

  return false;
}

llvm::Expected<ArrayRef<Requirement>>
RequirementSignatureRequest::evaluate(Evaluator &evaluator,
                                      ProtocolDecl *proto) const {
  ASTContext &ctx = proto->getASTContext();

  // First check if we have a deserializable requirement signature.
  if (proto->hasLazyRequirementSignature()) {
    ++NumLazyRequirementSignaturesLoaded;
    // FIXME: (transitional) increment the redundant "always-on" counter.
    if (ctx.Stats)
      ctx.Stats->getFrontendCounters().NumLazyRequirementSignaturesLoaded++;

    auto contextData = static_cast<LazyProtocolData *>(
        ctx.getOrCreateLazyContextData(proto, nullptr));

    SmallVector<Requirement, 8> requirements;
    contextData->loader->loadRequirementSignature(
        proto, contextData->requirementSignatureData, requirements);
    if (requirements.empty())
      return None;
    return ctx.AllocateCopy(requirements);
  }

  GenericSignatureBuilder builder(proto->getASTContext());

  // Add all of the generic parameters.
  for (auto gp : *proto->getGenericParams())
    builder.addGenericParameter(gp);

  // Add the conformance of 'self' to the protocol.
  auto selfType =
    proto->getSelfInterfaceType()->castTo<GenericTypeParamType>();
  auto requirement =
    Requirement(RequirementKind::Conformance, selfType,
              proto->getDeclaredInterfaceType());

  builder.addRequirement(
          requirement,
          GenericSignatureBuilder::RequirementSource::forRequirementSignature(
                                                      builder, selfType, proto),
          nullptr);

  auto reqSignature = std::move(builder).computeGenericSignature(
                        SourceLoc(),
                        /*allowConcreteGenericPArams=*/false,
                        /*allowBuilderToMove=*/false);
  return reqSignature->getRequirements();
}

llvm::Expected<Type>
DefaultDefinitionTypeRequest::evaluate(Evaluator &evaluator,
                                       AssociatedTypeDecl *assocType) const {
  if (assocType->Resolver) {
    auto defaultType = assocType->Resolver->loadAssociatedTypeDefault(
                                    assocType, assocType->ResolverContextData);
    assocType->Resolver = nullptr;
    return defaultType;
  }

  TypeRepr *defaultDefinition = assocType->getDefaultDefinitionTypeRepr();
  if (defaultDefinition) {
    auto resolution = TypeResolution::forInterface(assocType->getDeclContext());
    return resolution.resolveType(defaultDefinition, None);
  }

  return Type();
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
    if (!prevValue->getType())
      return nullptr;

    if (auto intLit = dyn_cast<IntegerLiteralExpr>(prevValue)) {
      APInt nextVal = intLit->getValue().sextOrSelf(128) + 1;
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
      return TypeChecker::conformsToProtocol(rawTy, proto,
                                             ED->getDeclContext(), None);
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

const ConstructorDecl *
swift::findNonImplicitRequiredInit(const ConstructorDecl *CD) {
  while (CD->isImplicit()) {
    auto *overridden = CD->getOverriddenDecl();
    if (!overridden || !overridden->isRequired())
      break;
    CD = overridden;
  }
  return CD;
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

static NominalTypeDecl *resolveSingleNominalTypeDecl(
    DeclContext *DC, SourceLoc loc, Identifier ident, TypeChecker &tc,
    TypeResolutionFlags flags = TypeResolutionFlags(0)) {
  auto *TyR = new (tc.Context) SimpleIdentTypeRepr(loc, ident);
  TypeLoc typeLoc = TypeLoc(TyR);

  TypeResolutionOptions options = TypeResolverContext::TypeAliasDecl;
  options |= flags;
  if (TypeChecker::validateType(tc.Context, typeLoc,
                                TypeResolution::forInterface(DC), options))
    return nullptr;

  return typeLoc.getType()->getAnyNominal();
}

static bool checkDesignatedTypes(OperatorDecl *OD,
                                 ArrayRef<Identifier> identifiers,
                                 ArrayRef<SourceLoc> identifierLocs,
                                 TypeChecker &TC) {
  assert(identifiers.size() == identifierLocs.size());

  SmallVector<NominalTypeDecl *, 1> designatedNominalTypes;
  auto *DC = OD->getDeclContext();

  for (auto index : indices(identifiers)) {
    auto *decl = resolveSingleNominalTypeDecl(DC, identifierLocs[index],
                                              identifiers[index], TC);

    if (!decl)
      return true;

    designatedNominalTypes.push_back(decl);
  }

  auto &ctx = TC.Context;
  OD->setDesignatedNominalTypes(ctx.AllocateCopy(designatedNominalTypes));
  return false;
}

/// Validate the given operator declaration.
///
/// This establishes key invariants, such as an InfixOperatorDecl's
/// reference to its precedence group and the transitive validity of that
/// group.
void TypeChecker::validateDecl(OperatorDecl *OD) {
  checkDeclAttributes(OD);

  auto IOD = dyn_cast<InfixOperatorDecl>(OD);

  auto enableOperatorDesignatedTypes =
      getLangOpts().EnableOperatorDesignatedTypes;

  // Pre- or post-fix operator?
  if (!IOD) {
    auto nominalTypes = OD->getDesignatedNominalTypes();
    if (nominalTypes.empty() && enableOperatorDesignatedTypes) {
      auto identifiers = OD->getIdentifiers();
      auto identifierLocs = OD->getIdentifierLocs();
      if (checkDesignatedTypes(OD, identifiers, identifierLocs, *this))
        OD->setInvalid();
    }
    return;
  }

  if (!IOD->getPrecedenceGroup()) {
    PrecedenceGroupDecl *group = nullptr;

    auto identifiers = IOD->getIdentifiers();
    auto identifierLocs = IOD->getIdentifierLocs();

    if (!identifiers.empty()) {
      group = lookupPrecedenceGroupPrimitive(IOD->getDeclContext(),
                                             identifiers[0], identifierLocs[0]);
      if (group) {
        identifiers = identifiers.slice(1);
        identifierLocs = identifierLocs.slice(1);
      } else {
        // If we're either not allowing types, or we are allowing them
        // and this identifier is not a type, emit an error as if it's
        // a precedence group.
        auto *DC = OD->getDeclContext();
        if (!(enableOperatorDesignatedTypes &&
              resolveSingleNominalTypeDecl(
                  DC, identifierLocs[0], identifiers[0], *this,
                  TypeResolutionFlags::SilenceErrors))) {
          diagnose(identifierLocs[0], diag::unknown_precedence_group,
                   identifiers[0]);
          identifiers = identifiers.slice(1);
          identifierLocs = identifierLocs.slice(1);
        }
      }
    }

    if (!identifiers.empty() && !enableOperatorDesignatedTypes) {
      assert(!group);
      diagnose(identifierLocs[0], diag::unknown_precedence_group,
               identifiers[0]);
      identifiers = identifiers.slice(1);
      identifierLocs = identifierLocs.slice(1);
      assert(identifiers.empty() && identifierLocs.empty());
    }

    if (!group) {
      group = lookupPrecedenceGroupPrimitive(
          IOD->getDeclContext(), Context.Id_DefaultPrecedence, SourceLoc());
    }

    if (group) {
      validateDecl(group);
      IOD->setPrecedenceGroup(group);
    } else {
      diagnose(IOD->getLoc(), diag::missing_builtin_precedence_group,
               Context.Id_DefaultPrecedence);
    }

    auto nominalTypes = IOD->getDesignatedNominalTypes();
    if (nominalTypes.empty() && enableOperatorDesignatedTypes) {
      if (checkDesignatedTypes(IOD, identifiers, identifierLocs, *this)) {
        IOD->setInvalid();
        return;
      }
    }
  }
}

llvm::Expected<SelfAccessKind>
SelfAccessKindRequest::evaluate(Evaluator &evaluator, FuncDecl *FD) const {
  if (FD->getAttrs().getAttribute<MutatingAttr>(true)) {
    if (!FD->isInstanceMember() || !FD->getDeclContext()->hasValueSemantics()) {
      return SelfAccessKind::NonMutating;
    }
    return SelfAccessKind::Mutating;
  } else if (FD->getAttrs().hasAttribute<NonMutatingAttr>()) {
    return SelfAccessKind::NonMutating;
  } else if (FD->getAttrs().hasAttribute<ConsumingAttr>()) {
    return SelfAccessKind::Consuming;
  }

  if (auto *AD = dyn_cast<AccessorDecl>(FD)) {
    // Non-static set/willSet/didSet/mutableAddress default to mutating.
    // get/address default to non-mutating.
    switch (AD->getAccessorKind()) {
    case AccessorKind::Address:
    case AccessorKind::Get:
    case AccessorKind::Read:
      break;

    case AccessorKind::MutableAddress:
    case AccessorKind::Set:
    case AccessorKind::Modify:
      if (AD->isInstanceMember() && AD->getDeclContext()->hasValueSemantics())
        return SelfAccessKind::Mutating;
      break;

    case AccessorKind::WillSet:
    case AccessorKind::DidSet: {
      auto *storage =AD->getStorage();
      if (storage->isSetterMutating())
        return SelfAccessKind::Mutating;

      break;
    }
    }
  }

  return SelfAccessKind::NonMutating;
}

/// Check the requirements in the where clause of the given \c source
/// to ensure that they don't introduce additional 'Self' requirements.
static void checkProtocolSelfRequirements(ProtocolDecl *proto,
                                          TypeDecl *source) {
  WhereClauseOwner(source).visitRequirements(
      TypeResolutionStage::Interface,
      [&](const Requirement &req, RequirementRepr *reqRepr) {
        switch (req.getKind()) {
        case RequirementKind::Conformance:
        case RequirementKind::Layout:
        case RequirementKind::Superclass:
          if (reqRepr &&
              req.getFirstType()->isEqual(proto->getSelfInterfaceType())) {
            auto &diags = proto->getASTContext().Diags;
            diags.diagnose(reqRepr->getSubjectLoc().getLoc(),
                           diag::protocol_where_clause_self_requirement);
          }

          return false;

        case RequirementKind::SameType:
          return false;
        }
        llvm_unreachable("unhandled kind");
      });
}

/// For now, DynamicSelfType can only appear at the top level of a
/// function result type, possibly wrapped in an optional type.
///
/// In the future, we could generalize it to allow it in any
/// covariant position, so that for example a class method could
/// return '() -> Self'.
static void checkDynamicSelfType(ValueDecl *decl, Type type) {
  if (!type->hasDynamicSelfType())
    return;

  if (auto objectTy = type->getOptionalObjectType())
    type = objectTy;

  if (type->is<DynamicSelfType>())
    return;

  if (isa<FuncDecl>(decl))
    decl->diagnose(diag::dynamic_self_invalid_method);
  else if (isa<VarDecl>(decl))
    decl->diagnose(diag::dynamic_self_invalid_property);
  else {
    assert(isa<SubscriptDecl>(decl));
    decl->diagnose(diag::dynamic_self_invalid_subscript);
  }
}

namespace {
class DeclChecker : public DeclVisitor<DeclChecker> {
public:
  TypeChecker &TC;

  explicit DeclChecker(TypeChecker &TC) : TC(TC) {}

  void visit(Decl *decl) {
    if (TC.Context.Stats)
      TC.Context.Stats->getFrontendCounters().NumDeclsTypechecked++;

    FrontendStatsTracer StatsTracer(TC.Context.Stats, "typecheck-decl", decl);
    PrettyStackTraceDecl StackTrace("type-checking", decl);
    
    DeclVisitor<DeclChecker>::visit(decl);

    TC.checkUnsupportedProtocolType(decl);

    if (auto VD = dyn_cast<ValueDecl>(decl)) {
      checkRedeclaration(TC, VD);

      // Force some requests, which can produce diagnostics.

      // Compute access level.
      (void) VD->getFormalAccess();

      // Compute overrides.
      (void) VD->getOverriddenDecls();

      // Check whether the member is @objc or dynamic.
      (void) VD->isObjC();
      (void) VD->isDynamic();

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
    TC.checkDeclAttributes(ID);
  }

  void visitOperatorDecl(OperatorDecl *OD) {
    TC.validateDecl(OD);
    checkAccessControl(TC, OD);
  }

  void visitPrecedenceGroupDecl(PrecedenceGroupDecl *PGD) {
    TC.validateDecl(PGD);
    checkAccessControl(TC, PGD);
  }

  void visitMissingMemberDecl(MissingMemberDecl *MMD) {
    llvm_unreachable("should always be type-checked already");
  }

  void visitBoundVariable(VarDecl *VD) {
    // WARNING: Anything you put in this function will only be run when the
    // VarDecl is fully type-checked within its own file. It will NOT be run
    // when the VarDecl is merely used from another file.
    TC.validateDecl(VD);

    // Compute these requests in case they emit diagnostics.
    (void) VD->isGetterMutating();
    (void) VD->isSetterMutating();
    (void) VD->getPropertyWrapperBackingProperty();
    (void) VD->getImplInfo();

    // Add the '@_hasStorage' attribute if this property is stored.
    if (VD->hasStorage() && !VD->getAttrs().hasAttribute<HasStorageAttr>())
      VD->getAttrs().add(new (TC.Context) HasStorageAttr(/*isImplicit=*/true));

    // Reject cases where this is a variable that has storage but it isn't
    // allowed.
    if (VD->hasStorage()) {
      // Note: Stored properties in protocols, enums, etc are diagnosed in
      // finishStorageImplInfo().

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

        // Stored type variables in a generic context need to logically
        // occur once per instantiation, which we don't yet handle.
        if (DC->getExtendedProtocolDecl()) {
          unimplementedStatic(ProtocolExtensions);
        } else if (DC->isGenericContext()
               && !DC->getGenericSignatureOfContext()->areAllParamsConcrete()) {
          unimplementedStatic(GenericTypes);
        } else if (DC->getSelfClassDecl()) {
          auto StaticSpelling = PBD->getStaticSpelling();
          if (StaticSpelling != StaticSpellingKind::KeywordStatic)
            unimplementedStatic(Classes);
        }
      }
    }

    TC.checkDeclAttributes(VD);

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

    if (VD->getDeclContext()->getSelfClassDecl()) {
      checkDynamicSelfType(VD, VD->getValueInterfaceType());

      if (VD->getValueInterfaceType()->hasDynamicSelfType()) {
        if (VD->hasStorage())
          VD->diagnose(diag::dynamic_self_in_stored_property);
        else if (VD->isSettable(nullptr))
          VD->diagnose(diag::dynamic_self_in_mutable_property);
      }
    }

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

    // Now check all the accessors.
    VD->visitEmittedAccessors([&](AccessorDecl *accessor) {
      visit(accessor);
    });
  }

  void visitBoundVars(Pattern *P) {
    P->forEachVariable([&] (VarDecl *VD) { this->visitBoundVariable(VD); });
  }

  void visitPatternBindingDecl(PatternBindingDecl *PBD) {
    DeclContext *DC = PBD->getDeclContext();

    // Check all the pattern/init pairs in the PBD.
    validatePatternBindingEntries(TC, PBD);

    TC.checkDeclAttributes(PBD);

    for (unsigned i = 0, e = PBD->getNumPatternEntries(); i != e; ++i) {
      // Type check each VarDecl that this PatternBinding handles.
      visitBoundVars(PBD->getPattern(i));

      // If we have a type but no initializer, check whether the type is
      // default-initializable. If so, do it.
      if (PBD->getPattern(i)->hasType() &&
          !PBD->isInitialized(i) &&
          PBD->isDefaultInitializable(i) &&
          PBD->getPattern(i)->hasStorage() &&
          !PBD->getPattern(i)->getType()->hasError()) {
        auto type = PBD->getPattern(i)->getType();
        if (auto defaultInit = TC.buildDefaultInitializer(type)) {
          // If we got a default initializer, install it and re-type-check it
          // to make sure it is properly coerced to the pattern type.
          PBD->setInit(i, defaultInit);
        }
      }

      if (PBD->isInitialized(i)) {
        // Add the attribute that preserves the "has an initializer" value across
        // module generation, as required for TBDGen.
        PBD->getPattern(i)->forEachVariable([&](VarDecl *VD) {
          if (VD->hasStorage() &&
              !VD->getAttrs().hasAttribute<HasInitialValueAttr>()) {
            auto *attr = new (TC.Context) HasInitialValueAttr(
                /*IsImplicit=*/true);
            VD->getAttrs().add(attr);
          }
        });
      }
    }

    bool isInSILMode = false;
    if (auto sourceFile = DC->getParentSourceFile())
      isInSILMode = sourceFile->Kind == SourceFileKind::SIL;
    bool isTypeContext = DC->isTypeContext();

    // If this is a declaration without an initializer, reject code if
    // uninitialized vars are not allowed.
    for (unsigned i = 0, e = PBD->getNumPatternEntries(); i != e; ++i) {
      auto entry = PBD->getPatternList()[i];
    
      if (entry.isInitialized() || isInSILMode) continue;
      
      entry.getPattern()->forEachVariable([&](VarDecl *var) {
        // If the variable has no storage, it never needs an initializer.
        if (!var->hasStorage())
          return;

        if (var->isInvalid() || PBD->isInvalid())
          return;

        auto markVarAndPBDInvalid = [PBD, var] {
          PBD->setInvalid();
          var->setInvalid();
          if (!var->hasType())
            var->markInvalid();
        };
        
        // Properties with an opaque return type need an initializer to
        // determine their underlying type.
        if (var->getOpaqueResultTypeDecl()) {
          TC.diagnose(var->getLoc(), diag::opaque_type_var_no_init);
        }

        // Non-member observing properties need an initializer.
        if (var->getWriteImpl() == WriteImplKind::StoredWithObservers &&
            !isTypeContext) {
          TC.diagnose(var->getLoc(), diag::observingprop_requires_initializer);
          markVarAndPBDInvalid();
          return;
        }

        // Static/class declarations require an initializer unless in a
        // protocol.
        if (var->isStatic() && !isa<ProtocolDecl>(DC)) {
          // ...but don't enforce this for SIL or module interface files.
          switch (DC->getParentSourceFile()->Kind) {
          case SourceFileKind::Interface:
          case SourceFileKind::SIL:
            return;
          case SourceFileKind::Main:
          case SourceFileKind::REPL:
          case SourceFileKind::Library:
            break;
          }

          TC.diagnose(var->getLoc(), diag::static_requires_initializer,
                      var->getCorrectStaticSpelling());
          markVarAndPBDInvalid();
          return;
        }

        // Global variables require an initializer in normal source files.
        if (DC->isModuleScopeContext()) {
          switch (DC->getParentSourceFile()->Kind) {
          case SourceFileKind::Main:
          case SourceFileKind::REPL:
          case SourceFileKind::Interface:
          case SourceFileKind::SIL:
            return;
          case SourceFileKind::Library:
            break;
          }

          TC.diagnose(var->getLoc(), diag::global_requires_initializer,
                      var->isLet());
          markVarAndPBDInvalid();
          return;
        }
      });
    }

    TC.checkDeclAttributes(PBD);

    checkAccessControl(TC, PBD);

    // If the initializers in the PBD aren't checked yet, do so now.
    for (unsigned i = 0, e = PBD->getNumPatternEntries(); i != e; ++i) {
      if (!PBD->isInitialized(i))
        continue;

      if (!PBD->isInitializerChecked(i)) {
        TC.typeCheckPatternBinding(PBD, i);
      }

      if (!PBD->isInvalid()) {
        auto &entry = PBD->getPatternList()[i];
        auto *init = PBD->getInit(i);

        // If we're performing an binding to a weak or unowned variable from a
        // constructor call, emit a warning that the instance will be immediately
        // deallocated.
        diagnoseUnownedImmediateDeallocation(TC, PBD->getPattern(i),
                                              entry.getEqualLoc(),
                                              init);

        // If we entered an initializer context, contextualize any
        // auto-closures we might have created.
        // Note that we don't contextualize the initializer for a property
        // with a wrapper, because the initializer will have been subsumed
        // by the backing storage property.
        if (!DC->isLocalContext() &&
            !(PBD->getSingleVar() &&
              PBD->getSingleVar()->hasAttachedPropertyWrapper())) {
          auto *initContext = cast_or_null<PatternBindingInitializer>(
              entry.getInitContext());
          if (initContext) {
            // Check safety of error-handling in the declaration, too.
            TC.checkInitializerErrorHandling(initContext, init);
            (void) TC.contextualizeInitializer(initContext, init);
          }
        }
      }
    }
  }

  void visitSubscriptDecl(SubscriptDecl *SD) {
    TC.validateDecl(SD);

    // Force creation of the generic signature.
    (void)SD->getGenericSignature();
    if (!SD->isInvalid()) {
      TC.checkReferencedGenericParams(SD);
      checkGenericParams(SD->getGenericParams(), SD, TC);
      TC.checkProtocolSelfRequirements(SD);
    }

    TC.checkDeclAttributes(SD);

    checkAccessControl(TC, SD);

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

    // Compute these requests in case they emit diagnostics.
    (void) SD->isGetterMutating();
    (void) SD->isSetterMutating();
    (void) SD->getImplInfo();

    TC.checkParameterAttributes(SD->getIndices());
    TC.checkDefaultArguments(SD->getIndices(), SD);

    if (SD->getDeclContext()->getSelfClassDecl()) {
      checkDynamicSelfType(SD, SD->getValueInterfaceType());

      if (SD->getValueInterfaceType()->hasDynamicSelfType() &&
          SD->supportsMutation()) {
        SD->diagnose(diag::dynamic_self_in_mutable_subscript);
      }
    }

    // Now check all the accessors.
    SD->visitEmittedAccessors([&](AccessorDecl *accessor) {
      visit(accessor);
    });
  }

  void visitTypeAliasDecl(TypeAliasDecl *TAD) {

    TC.validateDecl(TAD);
    TC.checkDeclAttributes(TAD);

    // Force the generic signature to be computed in case it emits diagnostics.
    (void)TAD->getGenericSignature();
    // Force computing the underlying type in case it emits diagnostics.
    (void)TAD->getUnderlyingType();

    checkAccessControl(TC, TAD);

  }
  
  void visitOpaqueTypeDecl(OpaqueTypeDecl *OTD) {
    TC.validateDecl(OTD);
    TC.checkDeclAttributes(OTD);
    
    // Force the generic signature to be computed in case it emits diagnostics.
    (void)OTD->getGenericSignature();
    
    checkAccessControl(TC, OTD);
  }
  
  void visitAssociatedTypeDecl(AssociatedTypeDecl *AT) {
    TC.validateDecl(AT);
    TC.checkDeclAttributes(AT);

    checkInheritanceClause(AT);
    auto *proto = AT->getProtocol();

    checkProtocolSelfRequirements(proto, AT);

    if (proto->isObjC()) {
      TC.diagnose(AT->getLoc(),
                  diag::associated_type_objc,
                  AT->getName(),
                  proto->getName());
    }

    checkAccessControl(TC, AT);

    // Trigger the checking for overridden declarations.
    (void)AT->getOverriddenDecls();

    auto defaultType = AT->getDefaultDefinitionType();
    if (defaultType && !defaultType->hasError()) {
      // associatedtype X = X is invalid
      auto mentionsItself =
        defaultType.findIf([&](Type defaultType) {
          if (auto DMT = defaultType->getAs<DependentMemberType>()) {
            return DMT->getAssocType() == AT;
          }
          return false;
        });

      if (mentionsItself) {
        TC.diagnose(AT->getDefaultDefinitionTypeRepr()->getLoc(),
                    diag::recursive_decl_reference,
                    AT->getDescriptiveKind(), AT->getName());
        AT->diagnose(diag::kind_declared_here, DescriptiveDeclKind::Type);
      }
    }
  }

  void checkUnsupportedNestedType(NominalTypeDecl *NTD) {
    TC.diagnoseInlinableLocalType(NTD);

    // We don't support protocols outside the top level of a file.
    if (isa<ProtocolDecl>(NTD) &&
        !NTD->getParent()->isModuleScopeContext()) {
      TC.diagnose(NTD->getLoc(),
                  diag::unsupported_nested_protocol,
                  NTD->getName());
      NTD->setInvalid();
      return;
    }

    // We don't support nested types in generics yet.
    if (NTD->isGenericContext()) {
      auto DC = NTD->getDeclContext();
      if (auto proto = DC->getSelfProtocolDecl()) {
        if (DC->getExtendedProtocolDecl()) {
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
    checkUnsupportedNestedType(ED);
    TC.validateDecl(ED);
    checkGenericParams(ED->getGenericParams(), ED, TC);

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

    checkInheritanceClause(ED);

    checkAccessControl(TC, ED);

    if (ED->hasRawType() && !ED->isObjC()) {
      // ObjC enums have already had their raw values checked, but pure Swift
      // enums haven't.
      checkEnumRawValues(TC, ED);
    }

    checkExplicitAvailability(ED);

    TC.checkDeclCircularity(ED);
    TC.ConformanceContexts.push_back(ED);
  }

  void visitStructDecl(StructDecl *SD) {
    checkUnsupportedNestedType(SD);

    TC.validateDecl(SD);
    checkGenericParams(SD->getGenericParams(), SD, TC);

    // Force lowering of stored properties.
    (void) SD->getStoredProperties();

    TC.addImplicitConstructors(SD);

    for (Decl *Member : SD->getMembers())
      visit(Member);

    TC.checkPatternBindingCaptures(SD);

    TC.checkDeclAttributes(SD);

    checkInheritanceClause(SD);

    checkAccessControl(TC, SD);

    checkExplicitAvailability(SD);

    TC.checkDeclCircularity(SD);
    TC.ConformanceContexts.push_back(SD);
  }

  /// Check whether the given properties can be @NSManaged in this class.
  static bool propertiesCanBeNSManaged(ClassDecl *classDecl,
                                       ArrayRef<VarDecl *> vars) {
    // Check whether we have an Objective-C-defined class in our
    // inheritance chain.
    if (!classDecl->checkAncestry(AncestryFlags::ClangImported))
      return false;

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
    checkUnsupportedNestedType(CD);

    TC.validateDecl(CD);
    // Force creation of the generic signature.
    (void)CD->getGenericSignature();
    checkGenericParams(CD->getGenericParams(), CD, TC);

    {
      // Check for circular inheritance.
      SmallVector<ClassDecl *, 8> path;
      path.push_back(CD);
      checkCircularity(TC, CD, diag::circular_class_inheritance,
                       DescriptiveDeclKind::Class, path);
    }

    // Force lowering of stored properties.
    (void) CD->getStoredProperties();

    // Force creation of an implicit destructor, if any.
    (void) CD->getDestructor();

    for (Decl *Member : CD->getEmittedMembers())
      visit(Member);

    TC.checkPatternBindingCaptures(CD);

    // If this class requires all of its stored properties to have
    // in-class initializers, diagnose this now.
    if (CD->requiresStoredPropertyInits())
      checkRequiredInClassInits(CD);

    // Compute @objc for each superclass member, to catch selector
    // conflicts resulting from unintended overrides.
    //
    // FIXME: This should be a request so we can measure how much work
    // we're doing here.
    CD->walkSuperclasses(
      [&](ClassDecl *superclass) {
        if (!superclass->getParentSourceFile())
          return TypeWalker::Action::Stop;

        for (auto *member : superclass->getMembers()) {
          if (auto *vd = dyn_cast<ValueDecl>(member)) {
            if (vd->isPotentiallyOverridable()) {
              (void) vd->isObjC();
            }
          }
        }

        return TypeWalker::Action::Continue;
      });

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

      if (!isInvalidSuperclass && Super->hasMissingVTableEntries() &&
          !Super->isResilient(CD->getParentModule(),
                              ResilienceExpansion::Minimal)) {
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

      if (!TC.Context.isAccessControlDisabled()) {
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
    }

    TC.checkDeclAttributes(CD);

    checkInheritanceClause(CD);

    checkAccessControl(TC, CD);

    checkExplicitAvailability(CD);

    TC.checkDeclCircularity(CD);
    TC.ConformanceContexts.push_back(CD);
  }

  void visitProtocolDecl(ProtocolDecl *PD) {
    checkUnsupportedNestedType(PD);

    TC.validateDecl(PD);
    if (!PD->hasInterfaceType())
      return;

    auto *SF = PD->getParentSourceFile();
    {
      // Check for circular inheritance within the protocol.
      SmallVector<ProtocolDecl *, 8> path;
      path.push_back(PD);
      checkCircularity(TC, PD, diag::circular_protocol_def,
                       DescriptiveDeclKind::Protocol, path);

      if (SF) {
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

    checkInheritanceClause(PD);

    TC.checkDeclCircularity(PD);
    if (PD->isResilient())
      if (!SF || SF->Kind != SourceFileKind::Interface)
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
    (void) PD->existentialTypeSupported();

    // Explicity compute the requirement signature to detect errors.
    (void) PD->getRequirementSignature();

    checkExplicitAvailability(PD);
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

    // Declarations in SIL and module interface files don't require
    // definitions.
    if (auto sourceFile = decl->getDeclContext()->getParentSourceFile()) {
      switch (sourceFile->Kind) {
      case SourceFileKind::SIL:
      case SourceFileKind::Interface:
        return false;
      case SourceFileKind::Library:
      case SourceFileKind::Main:
      case SourceFileKind::REPL:
        break;
      }
    }

    // Everything else requires a definition.
    return true;
  }

  void visitFuncDecl(FuncDecl *FD) {
    TC.validateDecl(FD);

    if (!FD->isInvalid()) {
      checkGenericParams(FD->getGenericParams(), FD, TC);
      TC.checkReferencedGenericParams(FD);
      TC.checkProtocolSelfRequirements(FD);
    }

    checkAccessControl(TC, FD);

    TC.checkParameterAttributes(FD->getParameters());
    TC.checkDeclAttributes(FD);

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

    if (requiresDefinition(FD) && !FD->hasBody()) {
      // Complain if we should have a body.
      TC.diagnose(FD->getLoc(), diag::func_decl_without_brace);
    } else if (FD->getDeclContext()->isLocalContext()) {
      // Check local function bodies right away.
      TC.typeCheckAbstractFunctionBody(FD);
    } else {
      // Record the body.
      TC.definedFunctions.push_back(FD);
    }

    checkExplicitAvailability(FD);

    if (FD->getDeclContext()->getSelfClassDecl())
      checkDynamicSelfType(FD, FD->getResultInterfaceType());
  }

  void visitModuleDecl(ModuleDecl *) { }

  void visitEnumCaseDecl(EnumCaseDecl *ECD) {
    // The type-checker doesn't care about how these are grouped.
  }

  void visitEnumElementDecl(EnumElementDecl *EED) {
    TC.validateDecl(EED);

    TC.checkDeclAttributes(EED);

    if (auto *PL = EED->getParameterList()) {
      TC.checkParameterAttributes(PL);
      TC.checkDefaultArguments(PL, EED);
    }

    checkAccessControl(TC, EED);
  }

  void visitExtensionDecl(ExtensionDecl *ED) {
    // Produce any diagnostics for the extended type.
    auto extType = ED->getExtendedType();

    auto nominal = ED->getExtendedNominal();
    if (nominal == nullptr) {
      const bool wasAlreadyInvalid = ED->isInvalid();
      ED->setInvalid();
      if (extType && !extType->hasError() && extType->getAnyNominal()) {
        // If we've got here, then we have some kind of extension of a prima
        // fascie non-nominal type.  This can come up when we're projecting
        // typealiases out of bound generic types.
        //
        // struct Array<T> { typealias Indices = Range<Int> }
        // extension Array.Indices.Bound {}
        //
        // Offer to rewrite it to the underlying nominal type.
        auto canExtType = extType->getCanonicalType();
        ED->diagnose(diag::invalid_nominal_extension, extType, canExtType)
          .highlight(ED->getExtendedTypeRepr()->getSourceRange());
        ED->diagnose(diag::invalid_nominal_extension_rewrite, canExtType)
          .fixItReplace(ED->getExtendedTypeRepr()->getSourceRange(),
                        canExtType->getString());
      } else if (!wasAlreadyInvalid) {
        // If nothing else applies, fall back to a generic diagnostic.
        ED->diagnose(diag::non_nominal_extension, extType);
      }
      return;
    }

    // Validate the nominal type declaration being extended.
    TC.validateDecl(nominal);
    // Don't bother computing the generic signature if the extended nominal
    // type didn't pass validation so we don't crash.
    if (!nominal->isInvalid())
      (void)ED->getGenericSignature();
    ED->setValidationToChecked();

    if (extType && !extType->hasError()) {
      // The first condition catches syntactic forms like
      //     protocol A & B { ... } // may be protocols or typealiases
      // The second condition also looks through typealiases and catches
      //    typealias T = P1 & P2 // P2 is a refined protocol of P1
      //    extension T { ... }
      // However, it is trickier to catch cases like
      //    typealias T = P2 & P1 // P2 is a refined protocol of P1
      //    extension T { ... }
      // so we don't do that here.
      auto extTypeRepr = ED->getExtendedTypeRepr();
      auto *extTypeNominal = extType->getAnyNominal();
      bool firstNominalIsNotMostSpecific =
        extTypeNominal && extTypeNominal != nominal;
      if (isa<CompositionTypeRepr>(extTypeRepr)
          || firstNominalIsNotMostSpecific) {
        auto firstNominalType = nominal->getDeclaredType();
        auto diag = ED->diagnose(diag::composition_in_extended_type,
                                 firstNominalType);
        diag.highlight(extTypeRepr->getSourceRange());
        if (firstNominalIsNotMostSpecific) {
          diag.flush();
          Type mostSpecificProtocol = extTypeNominal->getDeclaredType();
          ED->diagnose(diag::composition_in_extended_type_alternative,
                       mostSpecificProtocol)
            .fixItReplace(extTypeRepr->getSourceRange(),
                          mostSpecificProtocol->getString());
        } else {
          diag.fixItReplace(extTypeRepr->getSourceRange(),
                            firstNominalType->getString());
        }
      }
    }

    checkInheritanceClause(ED);

    // Check the raw values of an enum, since we might synthesize
    // RawRepresentable while checking conformances on this extension.
    if (auto enumDecl = dyn_cast<EnumDecl>(nominal)) {
      if (enumDecl->hasRawType())
        checkEnumRawValues(TC, enumDecl);
    }

    // Only generic and protocol types are permitted to have
    // trailing where clauses.
    if (auto trailingWhereClause = ED->getTrailingWhereClause()) {
      if (!ED->getGenericParams() && !ED->isInvalid()) {
        ED->diagnose(diag::extension_nongeneric_trailing_where,
                     nominal->getFullName())
          .highlight(trailingWhereClause->getSourceRange());
      }
    }

    checkGenericParams(ED->getGenericParams(), ED, TC);

    for (Decl *Member : ED->getMembers())
      visit(Member);

    TC.ConformanceContexts.push_back(ED);

    TC.checkDeclAttributes(ED);
    checkAccessControl(TC, ED);

    checkExplicitAvailability(ED);
  }

  void visitTopLevelCodeDecl(TopLevelCodeDecl *TLCD) {
    // See swift::performTypeChecking for TopLevelCodeDecl handling.
    llvm_unreachable("TopLevelCodeDecls are handled elsewhere");
  }
  
  void visitIfConfigDecl(IfConfigDecl *ICD) {
    // The active members of the #if block will be type checked along with
    // their enclosing declaration.
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

    // Compute these requests in case they emit diagnostics.
    (void) CD->getInitKind();

    if (!CD->isInvalid()) {
      checkGenericParams(CD->getGenericParams(), CD, TC);
      TC.checkReferencedGenericParams(CD);
      TC.checkProtocolSelfRequirements(CD);
    }

    TC.checkDeclAttributes(CD);
    TC.checkParameterAttributes(CD->getParameters());

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
        } else if (overrideRequiresKeyword(CD->getOverriddenDecl())
                     == OverrideRequiresKeyword::Never) {
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
      if (CD->isFailable() &&
          CD->getOverriddenDecl() &&
          !CD->getOverriddenDecl()->isFailable()) {
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
      if (auto nominal = CD->getDeclContext()->getSelfNominalTypeDecl()) {
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

    checkAccessControl(TC, CD);

    if (requiresDefinition(CD) && !CD->hasBody()) {
      // Complain if we should have a body.
      TC.diagnose(CD->getLoc(), diag::missing_initializer_def);
    } else if (CD->getDeclContext()->isLocalContext()) {
      // Check local function bodies right away.
      TC.typeCheckAbstractFunctionBody(CD);
    } else {
      TC.definedFunctions.push_back(CD);
    }
  }

  void visitDestructorDecl(DestructorDecl *DD) {
    TC.validateDecl(DD);

    TC.checkDeclAttributes(DD);

    if (DD->getDeclContext()->isLocalContext()) {
      // Check local function bodies right away.
      TC.typeCheckAbstractFunctionBody(DD);
    } else {
      TC.definedFunctions.push_back(DD);
    }
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

  NominalTypeDecl *conformingDecl = dc->getSelfNominalTypeDecl();
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

llvm::Expected<bool>
IsImplicitlyUnwrappedOptionalRequest::evaluate(Evaluator &evaluator,
                                               ValueDecl *decl) const {
  TypeRepr *TyR = nullptr;

  switch (decl->getKind()) {
  case DeclKind::Func: {
    TyR = cast<FuncDecl>(decl)->getBodyResultTypeLoc().getTypeRepr();
    break;
  }

  case DeclKind::Accessor: {
    auto *accessor = cast<AccessorDecl>(decl);
    if (!accessor->isGetter())
      break;

    auto *storage = accessor->getStorage();
    if (auto *subscript = dyn_cast<SubscriptDecl>(storage))
      TyR = subscript->getElementTypeLoc().getTypeRepr();
    else
      TyR = cast<VarDecl>(storage)->getTypeLoc().getTypeRepr();
    break;
  }

  case DeclKind::Subscript:
    TyR = cast<SubscriptDecl>(decl)->getElementTypeLoc().getTypeRepr();
    break;

  case DeclKind::Param: {
    auto *param = cast<ParamDecl>(decl);
    if (param->isSelfParameter())
      return false;

    // FIXME: This "which accessor parameter am I" dance will come up in
    // other requests too. Factor it out when needed.
    if (auto *accessor = dyn_cast<AccessorDecl>(param->getDeclContext())) {
      auto *storage = accessor->getStorage();
      auto *accessorParams = accessor->getParameters();
      unsigned startIndex = 0;

      switch (accessor->getAccessorKind()) {
      case AccessorKind::DidSet:
      case AccessorKind::WillSet:
      case AccessorKind::Set:
        if (param == accessorParams->get(0)) {
          // This is the 'newValue' parameter.
          return storage->isImplicitlyUnwrappedOptional();
        }

        startIndex = 1;
        break;

      default:
        startIndex = 0;
        break;
      }

      // If the parameter is not the 'newValue' parameter to a setter, it
      // must be a subscript index parameter (or we have an invalid AST).
      auto *subscript = dyn_cast<SubscriptDecl>(storage);
      if (!subscript)
        return false;
      auto *subscriptParams = subscript->getIndices();

      auto where = llvm::find_if(*accessorParams,
                                  [param](ParamDecl *other) {
                                    return other == param;
                                  });
      assert(where != accessorParams->end());
      unsigned index = where - accessorParams->begin();

      auto *subscriptParam = subscriptParams->get(index - startIndex);

      if (param != subscriptParam) {
        // This is the 'subscript(...) { get { ... } set { ... } }' case.
        // This means we cloned the parameter list for each accessor.
        // Delegate to the original parameter.
        return subscriptParam->isImplicitlyUnwrappedOptional();
      }

      // This is the 'subscript(...) { <<body of getter>> }' case.
      // The subscript and the getter share their ParamDecls.
      // Fall through.
    }

    // Handle eg, 'inout Int!' or '__owned NSObject!'.
    TyR = param->getTypeLoc().getTypeRepr();
    if (auto *STR = dyn_cast_or_null<SpecifierTypeRepr>(TyR))
      TyR = STR->getBase();
    break;
  }

  case DeclKind::Var:
    // FIXME: See the comment in validateTypedPattern().
    break;

  default:
    break;
  }

  return (TyR && TyR->getKind() == TypeReprKind::ImplicitlyUnwrappedOptional);
}

/// Validate the underlying type of the given typealias.
llvm::Expected<Type>
UnderlyingTypeRequest::evaluate(Evaluator &evaluator,
                                TypeAliasDecl *typeAlias) const {
  TypeResolutionOptions options((typeAlias->getGenericParams()
                                     ? TypeResolverContext::GenericTypeAliasDecl
                                     : TypeResolverContext::TypeAliasDecl));

  if (!typeAlias->getDeclContext()->isCascadingContextForLookup(
          /*functionsAreNonCascading*/ true)) {
    options |= TypeResolutionFlags::KnownNonCascadingDependency;
  }

  // This can happen when code completion is attempted inside
  // of typealias underlying type e.g. `typealias F = () -> Int#^TOK^#`
  auto *underlyingRepr = typeAlias->getUnderlyingTypeRepr();
  if (!underlyingRepr) {
    typeAlias->setInvalid();
    return ErrorType::get(typeAlias->getASTContext());
  }

  auto underlyingLoc = TypeLoc(typeAlias->getUnderlyingTypeRepr());
  if (TypeChecker::validateType(typeAlias->getASTContext(), underlyingLoc,
                                TypeResolution::forInterface(typeAlias),
                                options)) {
    typeAlias->setInvalid();
    return ErrorType::get(typeAlias->getASTContext());
  }
  return underlyingLoc.getType();
}

/// Bind the given function declaration, which declares an operator, to the corresponding operator declaration.
llvm::Expected<OperatorDecl *>
FunctionOperatorRequest::evaluate(Evaluator &evaluator, FuncDecl *FD) const {  
  auto &C = FD->getASTContext();
  auto &diags = C.Diags;
  auto operatorName = FD->getFullName().getBaseIdentifier();

  // Check for static/final/class when we're in a type.
  auto dc = FD->getDeclContext();
  if (dc->isTypeContext()) {
    if (!FD->isStatic()) {
      FD->diagnose(diag::nonstatic_operator_in_type,
                   operatorName, dc->getDeclaredInterfaceType())
        .fixItInsert(FD->getAttributeInsertionLoc(/*forModifier=*/true),
                     "static ");

      FD->setStatic();
    } else if (auto classDecl = dc->getSelfClassDecl()) {
      // For a class, we also need the function or class to be 'final'.
      if (!classDecl->isFinal() && !FD->isFinal() &&
          FD->getStaticSpelling() != StaticSpellingKind::KeywordStatic) {
        FD->diagnose(diag::nonfinal_operator_in_class,
                     operatorName, dc->getDeclaredInterfaceType())
          .fixItInsert(FD->getAttributeInsertionLoc(/*forModifier=*/true),
                       "final ");
        FD->getAttrs().add(new (C) FinalAttr(/*IsImplicit=*/true));
      }
    }
  } else if (!dc->isModuleScopeContext()) {
    FD->diagnose(diag::operator_in_local_scope);
  }

  OperatorDecl *op = nullptr;
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
        diags.diagnose(FD, diag::declared_unary_op_without_attribute);

        // If we found both, point at them.
        if (prefixOp) {
          diags.diagnose(prefixOp, diag::unary_operator_declaration_here, false)
            .fixItInsert(FD->getLoc(), "prefix ");
          diags.diagnose(postfixOp, diag::unary_operator_declaration_here, true)
            .fixItInsert(FD->getLoc(), "postfix ");
        } else {
          // FIXME: Introduce a Fix-It that adds the operator declaration?
        }

        // FIXME: Errors could cascade here, because name lookup for this
        // operator won't find this declaration.
        return nullptr;
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
      diags.diagnose(FD->getFuncLoc(), diag::unary_op_missing_prepos_attribute,
                  static_cast<bool>(postfixOp))
        .fixItInsert(FD->getFuncLoc(), insertionText);
      diags.diagnose(op, diag::unary_operator_declaration_here,
                  static_cast<bool>(postfixOp));
    }
  } else if (FD->isBinaryOperator()) {
    op = SF.lookupInfixOperator(operatorName,
                                FD->isCascadingContextForLookup(false),
                                FD->getLoc());
  } else {
    diags.diagnose(FD, diag::invalid_arg_count_for_operator);
    return nullptr;
  }

  if (!op) {
    SourceLoc insertionLoc;
    if (isa<SourceFile>(FD->getParent())) {
      // Parent context is SourceFile, insertion location is start of func
      // declaration or unary operator
      if (FD->isUnaryOperator()) {
        insertionLoc = FD->getAttrs().getStartLoc();
      } else {
        insertionLoc = FD->getStartLoc();
      }
    } else {
      // Find the topmost non-file decl context and insert there.
      for (DeclContext *CurContext = FD->getLocalContext();
           !isa<SourceFile>(CurContext);
           CurContext = CurContext->getParent()) {
        // Skip over non-decl contexts (e.g. closure expresssions)
        if (auto *D = CurContext->getAsDecl())
            insertionLoc = D->getStartLoc();
      }
    }

    SmallString<128> insertion;
    {
      llvm::raw_svector_ostream str(insertion);
      assert(FD->isUnaryOperator() || FD->isBinaryOperator());
      if (FD->isUnaryOperator()) {
        if (FD->getAttrs().hasAttribute<PrefixAttr>())
          str << "prefix operator ";
        else
          str << "postfix operator ";
      } else {
        str << "infix operator ";
      }

       str << operatorName.str() << " : <# Precedence Group #>\n";
    }
    InFlightDiagnostic opDiagnostic =
        diags.diagnose(FD, diag::declared_operator_without_operator_decl);
    if (insertionLoc.isValid())
      opDiagnostic.fixItInsert(insertionLoc, insertion);
    return nullptr;
  }

  return op;
}

bool swift::isMemberOperator(FuncDecl *decl, Type type) {
  // Check that member operators reference the type of 'Self'.
  if (decl->isInvalid())
    return true;

  auto *DC = decl->getDeclContext();
  auto selfNominal = DC->getSelfNominalTypeDecl();

  // Check the parameters for a reference to 'Self'.
  bool isProtocol = selfNominal && isa<ProtocolDecl>(selfNominal);
  for (auto param : *decl->getParameters()) {
    auto paramType = param->getInterfaceType();
    if (!paramType) break;

    // Look through a metatype reference, if there is one.
    paramType = paramType->getMetatypeInstanceType();

    auto nominal = paramType->getAnyNominal();
    if (type.isNull()) {
      // Is it the same nominal type?
      if (selfNominal && nominal == selfNominal)
        return true;
    } else {
      // Is it the same nominal type? Or a generic (which may or may not match)?
      if (paramType->is<GenericTypeParamType>() ||
          nominal == type->getAnyNominal())
        return true;
    }

    if (isProtocol) {
      // For a protocol, is it the 'Self' type parameter?
      if (auto genericParam = paramType->getAs<GenericTypeParamType>())
        if (genericParam->isEqual(DC->getSelfInterfaceType()))
          return true;
    }
  }

  return false;
}

static Type buildAddressorResultType(TypeChecker &TC,
                                     AccessorDecl *addressor,
                                     Type valueType) {
  assert(addressor->getAccessorKind() == AccessorKind::Address ||
         addressor->getAccessorKind() == AccessorKind::MutableAddress);

  Type pointerType =
    (addressor->getAccessorKind() == AccessorKind::Address)
      ? TC.getUnsafePointerType(addressor->getLoc(), valueType)
      : TC.getUnsafeMutablePointerType(addressor->getLoc(), valueType);
  return pointerType;
}


static void validateResultType(TypeChecker &TC,
                               ValueDecl *decl, ParameterList *params,
                               TypeLoc &resultTyLoc,
                               TypeResolution resolution) {
  // Nothing to do if there's no result type loc to set into.
  if (resultTyLoc.isNull())
    return;

  // Check the result type. It is allowed to be opaque.
  if (auto opaqueTy =
          dyn_cast_or_null<OpaqueReturnTypeRepr>(resultTyLoc.getTypeRepr())) {
    // Create the decl and type for it.
    resultTyLoc.setType(
        TC.getOrCreateOpaqueResultType(resolution, decl, opaqueTy));
  } else {
    TypeChecker::validateType(TC.Context, resultTyLoc, resolution,
                              TypeResolverContext::FunctionResult);
  }
}

void TypeChecker::validateDecl(ValueDecl *D) {
  // Generic parameters are validated as part of their context.
  if (isa<GenericTypeParamDecl>(D))
    return;

  // Handling validation failure due to re-entrancy is left
  // up to the caller, who must call hasInterfaceType() to
  // check that validateDecl() returned a fully-formed decl.
  if (D->hasValidationStarted()) {
    // If this isn't reentrant (i.e. D has already been validated), the
    // signature better be valid.
    assert(D->isBeingValidated() || D->hasInterfaceType());
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
  FrontendStatsTracer StatsTracer(Context.Stats, "validate-decl", D);

  if (hasEnabledForbiddenTypecheckPrefix())
    checkForForbiddenPrefix(D);

  // Validate the context.
  auto dc = D->getDeclContext();
  if (auto nominal = dyn_cast<NominalTypeDecl>(dc)) {
    validateDecl(nominal);
    if (!nominal->hasInterfaceType())
      return;
  } else if (auto ext = dyn_cast<ExtensionDecl>(dc)) {
    // If we're currently validating, or have already validated this extension,
    // there's nothing more to do now.
    if (!ext->hasValidationStarted()) {
      DeclValidationRAII IBV(ext);

      if (auto *nominal = ext->getExtendedNominal()) {
        // Validate the nominal type declaration being extended.
        validateDecl(nominal);
        
        // Eagerly validate the generic signature of the extension.
        if (!nominal->isInvalid())
          (void)ext->getGenericSignature();
      }
    }
    if (ext->getValidationState() == Decl::ValidationState::Checking)
      return;
  }

  // Validating the parent may have triggered validation of this declaration,
  // so just return if that was the case.
  if (D->hasValidationStarted()) {
    assert(D->hasInterfaceType());
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

    // Finally, set the interface type.
    if (!assocType->hasInterfaceType())
      assocType->computeType();

    break;
  }

  case DeclKind::TypeAlias: {
    auto typeAlias = cast<TypeAliasDecl>(D);

    DeclValidationRAII IBV(typeAlias);

    // Finally, set the interface type.
    if (!typeAlias->hasInterfaceType())
      typeAlias->computeType();
    
    break;
  }
      
  case DeclKind::OpaqueType: {
    auto opaque = cast<OpaqueTypeDecl>(D);
    opaque->setValidationToChecked();
    break;
  }

  case DeclKind::Enum:
  case DeclKind::Struct:
  case DeclKind::Class: {
    auto nominal = cast<NominalTypeDecl>(D);
    nominal->computeType();
    nominal->setValidationToChecked();

    if (auto *ED = dyn_cast<EnumDecl>(nominal)) {
      // @objc enums use their raw values as the value representation, so we
      // need to force the values to be checked.
      if (ED->isObjC())
        checkEnumRawValues(*this, ED);
    }

    break;
  }

  case DeclKind::Protocol: {
    auto proto = cast<ProtocolDecl>(D);
    if (!proto->hasInterfaceType())
      proto->computeType();
    proto->setValidationToChecked();

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
    D->setValidationToChecked();

    if (VD->getOpaqueResultTypeDecl()) {
      if (auto SF = VD->getInnermostDeclContext()->getParentSourceFile()) {
        SF->markDeclWithOpaqueResultTypeAsValidated(VD);
      }
    }

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
      if (!storage->hasInterfaceType())
        return;
    }

    DeclValidationRAII IBV(FD);

    // Force computing the operator decl in case it emits diagnostics.
    (void) FD->getOperatorDecl();
    
    // Validate 'static'/'class' on functions in extensions.
    auto StaticSpelling = FD->getStaticSpelling();
    if (StaticSpelling != StaticSpellingKind::None &&
        isa<ExtensionDecl>(FD->getDeclContext())) {
      if (auto *NTD = FD->getDeclContext()->getSelfNominalTypeDecl()) {
        if (!isa<ClassDecl>(NTD)) {
          if (StaticSpelling == StaticSpellingKind::KeywordClass) {
            diagnose(FD, diag::class_func_not_in_class, false)
                .fixItReplace(FD->getStaticLoc(), "static");
            diagnose(NTD, diag::extended_type_declared_here);
          }
        }
      }
    }

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
      Type valueIfaceTy = storage->getValueInterfaceType();
      if (auto SD = dyn_cast<SubscriptDecl>(storage)) {
        // Copy the index types instead of re-validating them.
        auto indices = SD->getIndices();
        for (size_t i = 0, e = indices->size(); i != e; ++i) {
          auto subscriptParam = indices->get(i);
          if (!subscriptParam->hasInterfaceType())
            continue;

          Type paramIfaceTy = subscriptParam->getInterfaceType();

          auto accessorParam = valueParams->get(valueParams->size() - e + i);
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
      case AccessorKind::Set: {
        auto newValueParam = valueParams->get(0);
        newValueParam->setInterfaceType(valueIfaceTy);
        newValueParam->getTypeLoc().setType(valueIfaceTy);
        accessor->getBodyResultTypeLoc().setType(TupleType::getEmpty(Context));
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
      case AccessorKind::Read:
      case AccessorKind::Modify:
        accessor->getBodyResultTypeLoc().setType(TupleType::getEmpty(Context));
        break;
      }
    }
    
    // We want the function to be available for name lookup as soon
    // as it has a valid interface type.
    auto resolution = TypeResolution::forInterface(FD,
                                                   FD->getGenericSignature());
    typeCheckParameterList(FD->getParameters(), resolution,
                           TypeResolverContext::AbstractFunctionDecl);
    validateResultType(*this, FD, FD->getParameters(),
                       FD->getBodyResultTypeLoc(), resolution);
    // FIXME: Roll all of this interface type computation into a request.
    FD->computeType();

    // Member functions need some special validation logic.
    if (FD->getDeclContext()->isTypeContext()) {
      if (FD->isOperator() && !isMemberOperator(FD, nullptr)) {
        auto selfNominal = FD->getDeclContext()->getSelfNominalTypeDecl();
        auto isProtocol = selfNominal && isa<ProtocolDecl>(selfNominal);
        // We did not find 'Self'. Complain.
        diagnose(FD, diag::operator_in_unrelated_type,
                 FD->getDeclContext()->getDeclaredInterfaceType(), isProtocol,
                 FD->getFullName());
      }
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

    // Mark the opaque result type as validated, if there is one.
    if (FD->getOpaqueResultTypeDecl()) {
      if (auto sf = FD->getDeclContext()->getParentSourceFile()) {
        sf->markDeclWithOpaqueResultTypeAsValidated(FD);
      }
    }
    
    break;
  }

  case DeclKind::Constructor: {
    auto *CD = cast<ConstructorDecl>(D);

    DeclValidationRAII IBV(CD);

    auto res = TypeResolution::forInterface(CD, CD->getGenericSignature());
    typeCheckParameterList(CD->getParameters(), res,
                           TypeResolverContext::AbstractFunctionDecl);
    CD->computeType();
    break;
  }

  case DeclKind::Destructor: {
    auto *DD = cast<DestructorDecl>(D);

    DeclValidationRAII IBV(DD);

    auto res = TypeResolution::forInterface(DD, DD->getGenericSignature());
    typeCheckParameterList(DD->getParameters(), res,
                           TypeResolverContext::AbstractFunctionDecl);
    DD->computeType();
    break;
  }

  case DeclKind::Subscript: {
    auto *SD = cast<SubscriptDecl>(D);

    DeclValidationRAII IBV(SD);

    auto res = TypeResolution::forInterface(SD, SD->getGenericSignature());
    typeCheckParameterList(SD->getIndices(), res,
                           TypeResolverContext::SubscriptDecl);
    validateResultType(*this, SD, SD->getIndices(),
                       SD->getElementTypeLoc(), res);
    SD->computeType();

    if (SD->getOpaqueResultTypeDecl()) {
      if (auto SF = SD->getInnermostDeclContext()->getParentSourceFile()) {
        SF->markDeclWithOpaqueResultTypeAsValidated(SD);
      }
    }

    break;
  }

  case DeclKind::EnumElement: {
    auto *EED = cast<EnumElementDecl>(D);
    EnumDecl *ED = EED->getParentEnum();

    DeclValidationRAII IBV(EED);

    if (auto *PL = EED->getParameterList()) {
      typeCheckParameterList(PL,
                             TypeResolution::forInterface(
                                                    EED->getParentEnum(),
                                                    ED->getGenericSignature()),
                             TypeResolverContext::EnumElementDecl);
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
    EED->computeType();

    if (auto argTy = EED->getArgumentInterfaceType()) {
      assert(argTy->isMaterializable());
      (void) argTy;
    }

    break;
  }
  }

  assert(D->hasInterfaceType());
}

llvm::Expected<DeclRange>
EmittedMembersRequest::evaluate(Evaluator &evaluator,
                                ClassDecl *CD) const {
  if (!CD->getParentSourceFile())
    return CD->getMembers();

  auto &Context = CD->getASTContext();

  // FIXME: Remove TypeChecker dependencies below
  auto &TC = *(TypeChecker *) Context.getLazyResolver();

  // We need to add implicit initializers because they
  // affect vtable layout.
  TC.addImplicitConstructors(CD);

  auto forceConformance = [&](ProtocolDecl *protocol) {
    if (auto ref = TypeChecker::conformsToProtocol(
          CD->getDeclaredInterfaceType(), protocol, CD,
          ConformanceCheckFlags::SkipConditionalRequirements,
          SourceLoc())) {
      auto conformance = ref->getConcrete();
      if (conformance->getDeclContext() == CD &&
          conformance->getState() == ProtocolConformanceState::Incomplete) {
        TC.checkConformance(conformance->getRootNormalConformance());
      }
    }
  };

  // If the class is Encodable, Decodable or Hashable, force those
  // conformances to ensure that the synthesized members appear in the vtable.
  //
  // FIXME: Generalize this to other protocols for which
  // we can derive conformances.
  forceConformance(Context.getProtocol(KnownProtocolKind::Decodable));
  forceConformance(Context.getProtocol(KnownProtocolKind::Encodable));
  forceConformance(Context.getProtocol(KnownProtocolKind::Hashable));

  return CD->getMembers();
}

bool TypeChecker::isPassThroughTypealias(TypeAliasDecl *typealias,
                                         Type underlyingType,
                                         NominalTypeDecl *nominal) {
  // Pass-through only makes sense when the typealias refers to a nominal
  // type.
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

static bool isNonGenericTypeAliasType(Type type) {
  // A non-generic typealias can extend a specialized type.
  if (auto *aliasType = dyn_cast<TypeAliasType>(type.getPointer()))
    return aliasType->getDecl()->getGenericContextDepth() == (unsigned)-1;

  return false;
}

llvm::Expected<Type>
ExtendedTypeRequest::evaluate(Evaluator &eval, ExtensionDecl *ext) const {
  auto error = [&ext]() {
    ext->setInvalid();
    return ErrorType::get(ext->getASTContext());
  };

  // If we didn't parse a type, fill in an error type and bail out.
  auto *extendedRepr = ext->getExtendedTypeRepr();
  if (!extendedRepr)
    return error();

  // Compute the extended type.
  TypeResolutionOptions options(TypeResolverContext::ExtensionBinding);
  options |= TypeResolutionFlags::AllowUnboundGenerics;
  auto tr = TypeResolution::forStructural(ext->getDeclContext());
  auto extendedType = tr.resolveType(extendedRepr, options);

  if (extendedType->hasError())
    return error();

  // Hack to allow extending a generic typealias.
  if (auto *unboundGeneric = extendedType->getAs<UnboundGenericType>()) {
    if (auto *aliasDecl = dyn_cast<TypeAliasDecl>(unboundGeneric->getDecl())) {
      // Nested Hack to break cycles if this is called before validation has
      // finished.
      if (aliasDecl->hasInterfaceType()) {
        auto extendedNominal =
            aliasDecl->getDeclaredInterfaceType()->getAnyNominal();
        if (extendedNominal)
          return TypeChecker::isPassThroughTypealias(
                     aliasDecl, aliasDecl->getUnderlyingType(), extendedNominal)
                     ? extendedType
                     : extendedNominal->getDeclaredType();
      } else {
        if (auto ty = aliasDecl->getStructuralType()
                          ->getAs<NominalOrBoundGenericNominalType>())
          return TypeChecker::isPassThroughTypealias(aliasDecl, ty,
                                                     ty->getDecl())
                     ? extendedType
                     : ty->getDecl()->getDeclaredType();
      }
    }
  }

  auto &diags = ext->getASTContext().Diags;

  // Cannot extend a metatype.
  if (extendedType->is<AnyMetatypeType>()) {
    diags.diagnose(ext->getLoc(), diag::extension_metatype, extendedType)
         .highlight(extendedRepr->getSourceRange());
    return error();
  }

  // Cannot extend function types, tuple types, etc.
  if (!extendedType->getAnyNominal()) {
    diags.diagnose(ext->getLoc(), diag::non_nominal_extension, extendedType)
         .highlight(extendedRepr->getSourceRange());
    return error();
  }

  // Cannot extend a bound generic type, unless it's referenced via a
  // non-generic typealias type.
  if (extendedType->isSpecialized() &&
      !isNonGenericTypeAliasType(extendedType)) {
    diags.diagnose(ext->getLoc(), diag::extension_specialization,
                   extendedType->getAnyNominal()->getName())
         .highlight(extendedRepr->getSourceRange());
    return error();
  }

  return extendedType;
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
    if (auto ref = TypeChecker::conformsToProtocol(superclassType, decodableProto,
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

  // Lazily construct a mapping from backing storage properties to the
  // declared properties.
  bool computedBackingToOriginalVars = false;
  llvm::SmallDenseMap<VarDecl *, VarDecl *> backingToOriginalVars;
  auto getOriginalVar = [&](VarDecl *var) -> VarDecl * {
    // If we haven't computed the mapping yet, do so now.
    if (!computedBackingToOriginalVars) {
      for (auto member : classDecl->getMembers()) {
        if (auto var = dyn_cast<VarDecl>(member)) {
          if (auto backingVar = var->getPropertyWrapperBackingProperty()) {
            backingToOriginalVars[backingVar] = var;
          }
        }
      }

      computedBackingToOriginalVars = true;
    }

    auto known = backingToOriginalVars.find(var);
    if (known == backingToOriginalVars.end())
      return nullptr;

    return known->second;
  };

  for (auto member : classDecl->getMembers()) {
    auto pbd = dyn_cast<PatternBindingDecl>(member);
    if (!pbd)
      continue;

    if (pbd->isStatic() || !pbd->hasStorage() ||
        pbd->isDefaultInitializable() || pbd->isInvalid())
      continue;
   
    for (auto entry : pbd->getPatternList()) {
      if (entry.isInitialized()) continue;
      
      SmallVector<VarDecl *, 4> vars;
      entry.getPattern()->collectVariables(vars);
      if (vars.empty()) continue;

      // Replace the variables we found with the originals for diagnostic
      // purposes.
      for (auto &var : vars) {
        if (auto originalVar = getOriginalVar(var))
          var = originalVar;
      }

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
  if (auto *SF = classDecl->getParentSourceFile()) {
    // Allow classes without initializers in SIL and module interface files.
    switch (SF->Kind) {
    case SourceFileKind::SIL:
    case SourceFileKind::Interface:
      return;
    case SourceFileKind::Library:
    case SourceFileKind::Main:
    case SourceFileKind::REPL:
      break;
    }
  }

  // Some heuristics to skip emitting a diagnostic if the class is already
  // irreperably busted.
  if (classDecl->isInvalid() ||
      classDecl->inheritsSuperclassInitializers())
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
