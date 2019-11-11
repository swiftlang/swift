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
#include "swift/AST/NameLookupRequests.h"
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

// Check for static properties that produce empty option sets
// using a rawValue initializer with a value of '0'
static void checkForEmptyOptionSet(const VarDecl *VD) {
  // Check if property is a 'static let'
  if (!VD->isStatic() || !VD->isLet())
    return;
  
  auto DC = VD->getDeclContext();
  
  // Make sure property is of same type as the type it is declared in
  if (!VD->getType()->isEqual(DC->getSelfTypeInContext()))
    return;
  
  // Make sure this type conforms to OptionSet
  auto *optionSetProto = VD->getASTContext().getProtocol(KnownProtocolKind::OptionSet);
  bool conformsToOptionSet = (bool)TypeChecker::containsProtocol(
                                                  DC->getSelfTypeInContext(),
                                                  optionSetProto,
                                                  DC,
                                                  /*Flags*/None);
  
  if (!conformsToOptionSet)
    return;
  
  auto PBD = VD->getParentPatternBinding();
  if (!PBD)
    return;
  
  auto initIndex = PBD->getPatternEntryIndexForVarDecl(VD);
  auto init = PBD->getInit(initIndex);

  // Make sure property is being set with a constructor
  auto ctor = dyn_cast_or_null<CallExpr>(init);
  if (!ctor)
    return;
  auto ctorCalledVal = ctor->getCalledValue();
  if (!ctorCalledVal)
    return;
  if (!isa<ConstructorDecl>(ctorCalledVal))
    return;
  
  // Make sure it is calling the rawValue constructor
  if (ctor->getNumArguments() != 1)
    return;
  if (ctor->getArgumentLabels().front() != VD->getASTContext().Id_rawValue)
    return;
  
  // Make sure the rawValue parameter is a '0' integer literal
  auto *args = cast<TupleExpr>(ctor->getArg());
  auto intArg = dyn_cast<IntegerLiteralExpr>(args->getElement(0));
  if (!intArg)
    return;
  if (intArg->getValue() != 0)
    return;
  
  VD->diagnose(diag::option_set_zero_constant, VD->getName());
  VD->diagnose(diag::option_set_empty_set_init)
    .fixItReplace(args->getSourceRange(), "([])");
}


/// Check the inheritance clauses generic parameters along with any
/// requirements stored within the generic parameter list.
static void checkGenericParams(GenericContext *ownerCtx) {
  const auto genericParams = ownerCtx->getGenericParams();
  if (!genericParams)
    return;

  for (auto gp : *genericParams) {
    TypeChecker::checkDeclAttributes(gp);
    checkInheritanceClause(gp);
  }

  // Force visitation of each of the requirements here.
  WhereClauseOwner(ownerCtx)
      .visitRequirements(TypeResolutionStage::Interface,
                         [](Requirement, RequirementRepr *) { return false; });
}

static bool canSkipCircularityCheck(NominalTypeDecl *decl) {
  // Don't bother checking imported or deserialized decls.
  return decl->hasClangNode() || decl->wasDeserialized();
}

llvm::Expected<bool>
HasCircularInheritanceRequest::evaluate(Evaluator &evaluator,
                                        ClassDecl *decl) const {
  if (canSkipCircularityCheck(decl) || !decl->hasSuperclass())
    return false;

  auto *superclass = decl->getSuperclassDecl();
  auto result = evaluator(HasCircularInheritanceRequest{superclass});

  // If we have a cycle, handle it and return true.
  if (!result) {
    using Error = CyclicalRequestError<HasCircularInheritanceRequest>;
    llvm::handleAllErrors(result.takeError(), [](const Error &E) {});
    return true;
  }
  return result;
}

llvm::Expected<bool>
HasCircularInheritedProtocolsRequest::evaluate(Evaluator &evaluator,
                                               ProtocolDecl *decl) const {
  if (canSkipCircularityCheck(decl))
    return false;

  bool anyObject = false;
  auto inherited = getDirectlyInheritedNominalTypeDecls(decl, anyObject);
  for (auto &found : inherited) {
    auto *protoDecl = dyn_cast<ProtocolDecl>(found.second);
    if (!protoDecl)
      continue;

    // If we have a cycle, handle it and return true.
    auto result = evaluator(HasCircularInheritedProtocolsRequest{protoDecl});
    if (!result) {
      using Error = CyclicalRequestError<HasCircularInheritedProtocolsRequest>;
      llvm::handleAllErrors(result.takeError(), [](const Error &E) {});
      return true;
    }

    // If the underlying request handled a cycle and returned true, bail.
    if (*result)
      return true;
  }
  return false;
}

llvm::Expected<bool>
HasCircularRawValueRequest::evaluate(Evaluator &evaluator,
                                     EnumDecl *decl) const {
  if (canSkipCircularityCheck(decl) || !decl->hasRawType())
    return false;

  auto *inherited = decl->getRawType()->getEnumOrBoundGenericEnum();
  if (!inherited)
    return false;

  // If we have a cycle, handle it and return true.
  auto result = evaluator(HasCircularRawValueRequest{inherited});
  if (!result) {
    using Error = CyclicalRequestError<HasCircularRawValueRequest>;
    llvm::handleAllErrors(result.takeError(), [](const Error &E) {});
    return true;
  }
  return result;
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

  auto sig = TypeChecker::checkGenericSignature(
             nestedList.back(), DC,
             /*parentSig=*/nullptr,
             /*allowConcreteGenericParams=*/true);
  return (sig ? sig->getGenericEnvironment() : nullptr);
}

/// Check whether \c current is a redeclaration.
static void checkRedeclaration(ASTContext &ctx, ValueDecl *current) {
  // If we've already checked this declaration, don't do it again.
  if (current->alreadyCheckedRedeclaration())
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
    //
    // FIXME: Breaking a cycle here with hasInterfaceType() is bogus.
    if (current == other || (other->hasInterfaceType() && other->isInvalid()))
      continue;

    // Skip declarations in other modules.
    if (currentModule != other->getModuleContext())
      continue;

    // If both declarations are in the same file, only diagnose the second one.
    if (currentFile == other->getDeclContext()->getParentSourceFile())
      if (current->getLoc().isValid() &&
          ctx.SourceMgr.isBeforeInBuffer(
            current->getLoc(), other->getLoc()))
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

    // Skip invalid declarations.
    if (other->isInvalid())
      continue;

    // Skip declarations in other files.
    // In practice, this means we will warn on a private declaration that
    // shadows a non-private one, but only in the file where the shadowing
    // happens. We will warn on conflicting non-private declarations in both
    // files.
    if (!other->isAccessibleFrom(currentDC))
      continue;

    // Thwart attempts to override the same declaration more than once.
    const auto *currentOverride = current->getOverriddenDecl();
    const auto *otherOverride = other->getOverriddenDecl();
    if (currentOverride && currentOverride == otherOverride) {
      current->diagnose(diag::multiple_override, current->getFullName());
      other->diagnose(diag::multiple_override_prev, other->getFullName());
      current->setInvalid();
      break;
    }

    // Get the overload signature type.
    CanType otherSigType = other->getOverloadSignatureType();

    bool wouldBeSwift5Redeclaration = false;
    auto isRedeclaration = conflicting(ctx, currentSig, currentSigType,
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
            ctx.SourceMgr.isBeforeInBuffer(current->getLoc(),
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
        current->diagnose(diag::invalid_redecl_swift5_warning,
                          current->getFullName());
        other->diagnose(diag::invalid_redecl_prev, other->getFullName());
      } else {
        const auto *otherInit = dyn_cast<ConstructorDecl>(other);
        // Provide a better description for implicit initializers.
        if (otherInit && otherInit->isImplicit()) {
          // Skip conflicts with inherited initializers, which only happen
          // when the current declaration is within an extension. The override
          // checker should have already taken care of emitting a more
          // productive diagnostic.
          if (!other->getOverriddenDecl())
            current->diagnose(diag::invalid_redecl_init,
                              current->getFullName(),
                              otherInit->isMemberwiseInitializer());
        } else {
          ctx.Diags.diagnoseWithNotes(
            current->diagnose(diag::invalid_redecl,
                              current->getFullName()), [&]() {
            other->diagnose(diag::invalid_redecl_prev, other->getFullName());
          });
        }
        current->setInvalid();
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
    if (isa<ClassDecl>(nominal) && !decl->isSynthesized() &&
        isa<ExtensionDecl>(decl->getDeclContext()) &&
        !(decl->getAttrs().hasAttribute<DynamicReplacementAttr>())) {
      if (cast<ClassDecl>(nominal)->getForeignClassKind() == ClassDecl::ForeignKind::CFType) {
        diags.diagnose(decl->getLoc(),
                       diag::cfclass_designated_init_in_extension,
                       nominal->getName());
        return CtorInitializerKind::Designated;
      } else {
        diags.diagnose(decl->getLoc(),
                       diag::designated_init_in_extension,
                       nominal->getName())
            .fixItInsert(decl->getLoc(), "convenience ");
        return CtorInitializerKind::Convenience;
      }
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
          VD->getOriginalWrappedProperty(PropertyWrapperSynthesizedPropertyKind::Backing))
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
IsStaticRequest::evaluate(Evaluator &evaluator, FuncDecl *decl) const {
  if (auto *accessor = dyn_cast<AccessorDecl>(decl))
    return accessor->getStorage()->isStatic();

  bool result = (decl->getStaticLoc().isValid() ||
                 decl->getStaticSpelling() != StaticSpellingKind::None);
  auto *dc = decl->getDeclContext();
  if (!result &&
      decl->isOperator() &&
      dc->isTypeContext()) {
    auto operatorName = decl->getFullName().getBaseIdentifier();
    decl->diagnose(diag::nonstatic_operator_in_type,
                   operatorName, dc->getDeclaredInterfaceType())
        .fixItInsert(decl->getAttributeInsertionLoc(/*forModifier=*/true),
                     "static ");
    result = true;
  }

  return result;
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

llvm::Expected<bool>
NeedsNewVTableEntryRequest::evaluate(Evaluator &evaluator,
                                     AbstractFunctionDecl *decl) const {
  auto *dc = decl->getDeclContext();
  if (!isa<ClassDecl>(dc))
    return true;

  assert(isa<FuncDecl>(decl) || isa<ConstructorDecl>(decl));

  // Final members are always be called directly.
  // Dynamic methods are always accessed by objc_msgSend().
  if (decl->isFinal() || decl->isObjCDynamic() || decl->hasClangNode())
    return false;

  auto &ctx = dc->getASTContext();

  // Initializers are not normally inherited, but required initializers can
  // be overridden for invocation from dynamic types, and convenience initializers
  // are conditionally inherited when all designated initializers are available,
  // working by dynamically invoking the designated initializer implementation
  // from the subclass. Convenience initializers can also override designated
  // initializer implementations from their superclass.
  if (auto ctor = dyn_cast<ConstructorDecl>(decl)) {
    if (!ctor->isRequired() && !ctor->isDesignatedInit()) {
      return false;
    }

    // Stub constructors don't appear in the vtable.
    if (ctor->hasStubImplementation())
      return false;
  }

  if (auto *accessor = dyn_cast<AccessorDecl>(decl)) {
    // Check to see if it's one of the opaque accessors for the declaration.
    auto storage = accessor->getStorage();
    if (!storage->requiresOpaqueAccessor(accessor->getAccessorKind()))
      return false;
  }

  auto base = decl->getOverriddenDecl();

  if (!base || base->hasClangNode() || base->isObjCDynamic())
    return true;

  // As above, convenience initializers are not formally overridable in Swift
  // vtables, although same-named initializers are modeled as overriding for
  // various QoI and objc interop reasons. Even if we "override" a non-required
  // convenience init, we still need a distinct vtable entry.
  if (auto baseCtor = dyn_cast<ConstructorDecl>(base)) {
    if (!baseCtor->isRequired() && !baseCtor->isDesignatedInit()) {
      return true;
    }
  }

  // If the base is less visible than the override, we might need a vtable
  // entry since callers of the override might not be able to see the base
  // at all.
  if (decl->isEffectiveLinkageMoreVisibleThan(base))
    return true;

  using Direction = ASTContext::OverrideGenericSignatureReqCheck;
  if (!ctx.overrideGenericSignatureReqsSatisfied(
          base, decl, Direction::BaseReqSatisfiedByDerived)) {
    return true;
  }

  // If this method is an ABI compatible override, then we don't need a new
  // vtable entry. Otherwise, if it's not ABI compatible, for example if the
  // base has a more general AST type, then we need a new entry. Note that an
  // abstraction change is OK; we don't want to add a whole new vtable entry
  // just because an @in parameter becomes @owned, or whatever.
  auto isABICompatibleOverride =
      evaluateOrDefault(evaluator, IsABICompatibleOverrideRequest{decl}, false);
  return !isABICompatibleOverride;
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
static LiteralExpr *getAutomaticRawValueExpr(AutomaticEnumValueKind valueKind,
                                             EnumElementDecl *forElt,
                                             LiteralExpr *prevValue) {
  auto &Ctx = forElt->getASTContext();
  switch (valueKind) {
  case AutomaticEnumValueKind::None:
    Ctx.Diags.diagnose(forElt->getLoc(),
                       diag::enum_non_integer_convertible_raw_type_no_value);
    return nullptr;

  case AutomaticEnumValueKind::String:
    return new (Ctx) StringLiteralExpr(forElt->getNameStr(), SourceLoc(),
                                              /*Implicit=*/true);

  case AutomaticEnumValueKind::Integer:
    // If there was no previous value, start from zero.
    if (!prevValue) {
      return new (Ctx) IntegerLiteralExpr("0", SourceLoc(),
                                                 /*Implicit=*/true);
    }

    if (auto intLit = dyn_cast<IntegerLiteralExpr>(prevValue)) {
      APInt nextVal = intLit->getRawValue().sextOrSelf(128) + 1;
      bool negative = nextVal.slt(0);
      if (negative)
        nextVal = -nextVal;

      llvm::SmallString<10> nextValStr;
      nextVal.toStringSigned(nextValStr);
      auto expr = new (Ctx)
        IntegerLiteralExpr(Ctx.AllocateCopy(StringRef(nextValStr)),
                           forElt->getLoc(), /*Implicit=*/true);
      if (negative)
        expr->setNegative(forElt->getLoc());

      return expr;
    }

    Ctx.Diags.diagnose(forElt->getLoc(),
                       diag::enum_non_integer_raw_value_auto_increment);
    return nullptr;
  }

  llvm_unreachable("Unhandled AutomaticEnumValueKind in switch.");
}

static Optional<AutomaticEnumValueKind>
computeAutomaticEnumValueKind(EnumDecl *ED) {
  Type rawTy = ED->getRawType();
  assert(rawTy && "Cannot compute value kind without raw type!");
  
  if (ED->getGenericEnvironmentOfContext() != nullptr)
    rawTy = ED->mapTypeIntoContext(rawTy);
  
  // Swift enums require that the raw type is convertible from one of the
  // primitive literal protocols.
  auto conformsToProtocol = [&](KnownProtocolKind protoKind) {
    ProtocolDecl *proto = ED->getASTContext().getProtocol(protoKind);
    return TypeChecker::conformsToProtocol(rawTy, proto, ED->getDeclContext(),
                                           None);
  };

  static auto otherLiteralProtocolKinds = {
    KnownProtocolKind::ExpressibleByFloatLiteral,
    KnownProtocolKind::ExpressibleByUnicodeScalarLiteral,
    KnownProtocolKind::ExpressibleByExtendedGraphemeClusterLiteral,
  };
  
  if (conformsToProtocol(KnownProtocolKind::ExpressibleByIntegerLiteral)) {
    return AutomaticEnumValueKind::Integer;
  } else if (conformsToProtocol(KnownProtocolKind::ExpressibleByStringLiteral)){
    return AutomaticEnumValueKind::String;
  } else if (std::any_of(otherLiteralProtocolKinds.begin(),
                         otherLiteralProtocolKinds.end(),
                         conformsToProtocol)) {
    return AutomaticEnumValueKind::None;
  } else {
    return None;
  }
}

llvm::Expected<bool>
EnumRawValuesRequest::evaluate(Evaluator &eval, EnumDecl *ED,
                               TypeResolutionStage stage) const {
  Type rawTy = ED->getRawType();
  if (!rawTy) {
    return true;
  }

  if (ED->getGenericEnvironmentOfContext() != nullptr)
    rawTy = ED->mapTypeIntoContext(rawTy);
  if (rawTy->hasError())
    return true;

  // Check the raw values of the cases.
  LiteralExpr *prevValue = nullptr;
  EnumElementDecl *lastExplicitValueElt = nullptr;

  // Keep a map we can use to check for duplicate case values.
  llvm::SmallDenseMap<RawValueKey, RawValueSource, 8> uniqueRawValues;

  // Make the raw member accesses explicit.
  auto uncheckedRawValueOf = [](EnumElementDecl *EED) -> LiteralExpr * {
    return EED->RawValueExpr;
  };
  
  Optional<AutomaticEnumValueKind> valueKind;
  for (auto elt : ED->getAllElements()) {
    // If the element has been diagnosed up to now, skip it.
    if (elt->isInvalid())
      continue;

    if (uncheckedRawValueOf(elt)) {
      if (!uncheckedRawValueOf(elt)->isImplicit())
        lastExplicitValueElt = elt;
    } else if (!ED->LazySemanticInfo.hasFixedRawValues()) {
      // Try to pull out the automatic enum value kind.  If that fails, bail.
      if (!valueKind) {
        valueKind = computeAutomaticEnumValueKind(ED);
        if (!valueKind) {
          elt->setInvalid();
          return true;
        }
      }
      
      // If the enum element has no explicit raw value, try to
      // autoincrement from the previous value, or start from zero if this
      // is the first element.
      auto nextValue = getAutomaticRawValueExpr(*valueKind, elt, prevValue);
      if (!nextValue) {
        elt->setInvalid();
        break;
      }
      elt->setRawValueExpr(nextValue);
    }
    prevValue = uncheckedRawValueOf(elt);
    assert(prevValue && "continued without setting raw value of enum case");

    switch (stage) {
    case TypeResolutionStage::Structural:
      // We're only interested in computing the complete set of raw values,
      // so we can skip type checking.
      continue;
    default:
      // Continue on to type check the raw value.
      break;
    }

    
    {
      auto *TC = ED->getASTContext().getLegacyGlobalTypeChecker();
      assert(TC && "Must have a global type checker set");
      Expr *exprToCheck = prevValue;
      if (TC->typeCheckExpression(exprToCheck, ED, TypeLoc::withoutLoc(rawTy),
                                  CTP_EnumCaseRawValue)) {
        TypeChecker::checkEnumElementErrorHandling(elt, exprToCheck);
      }
    }

    // If we didn't find a valid initializer (maybe the initial value was
    // incompatible with the raw value type) mark the entry as being erroneous.
    if (!prevValue->getType() || prevValue->getType()->hasError()) {
      elt->setInvalid();
      continue;
    }

    // If the raw values of the enum case are fixed, then we trust our callers
    // to have set things up correctly.  This comes up with imported enums
    // and deserialized @objc enums which always have their raw values setup
    // beforehand.
    if (ED->LazySemanticInfo.hasFixedRawValues())
      continue;

    // Check that the raw value is unique.
    RawValueKey key{prevValue};
    RawValueSource source{elt, lastExplicitValueElt};

    auto insertIterPair = uniqueRawValues.insert({key, source});
    if (insertIterPair.second)
      continue;

    // Diagnose the duplicate value.
    auto &Diags = ED->getASTContext().Diags;
    SourceLoc diagLoc = uncheckedRawValueOf(elt)->isImplicit()
        ? elt->getLoc() : uncheckedRawValueOf(elt)->getLoc();
    Diags.diagnose(diagLoc, diag::enum_raw_value_not_unique);
    assert(lastExplicitValueElt &&
           "should not be able to have non-unique raw values when "
           "relying on autoincrement");
    if (lastExplicitValueElt != elt &&
        valueKind == AutomaticEnumValueKind::Integer) {
      Diags.diagnose(uncheckedRawValueOf(lastExplicitValueElt)->getLoc(),
                     diag::enum_raw_value_incrementing_from_here);
    }

    RawValueSource prevSource = insertIterPair.first->second;
    auto foundElt = prevSource.sourceElt;
    diagLoc = uncheckedRawValueOf(foundElt)->isImplicit()
        ? foundElt->getLoc() : uncheckedRawValueOf(foundElt)->getLoc();
    Diags.diagnose(diagLoc, diag::enum_raw_value_used_here);
    if (foundElt != prevSource.lastExplicitValueElt &&
        valueKind == AutomaticEnumValueKind::Integer) {
      if (prevSource.lastExplicitValueElt)
        Diags.diagnose(uncheckedRawValueOf(prevSource.lastExplicitValueElt)
                         ->getLoc(),
                       diag::enum_raw_value_incrementing_from_here);
      else
        Diags.diagnose(ED->getAllElements().front()->getLoc(),
                       diag::enum_raw_value_incrementing_from_zero);
    }
  }
  return true;
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
static void buildHigherThanPath(
    PrecedenceGroupDecl *last,
    const llvm::DenseMap<PrecedenceGroupDecl *, PrecedenceGroupDecl *>
        &visitedFrom,
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
                               PrecedenceGroupDecl *target, raw_ostream &out) {
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

static void checkPrecedenceCircularity(DiagnosticEngine &D,
                                       PrecedenceGroupDecl *PGD) {
  // Don't diagnose if this group is already marked invalid.
  if (PGD->isInvalid())
    return;

  // The cycle doesn't necessarily go through this specific group,
  // so we need a proper visited set to avoid infinite loops.  We
  // also record a back-reference so that we can easily reconstruct
  // the cycle.
  llvm::DenseMap<PrecedenceGroupDecl *, PrecedenceGroupDecl *> visitedFrom;
  SmallVector<PrecedenceGroupDecl *, 4> stack;

  // Fill out the targets set.
  llvm::SmallPtrSet<PrecedenceGroupDecl *, 4> targets;
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
      if (!rel.Group)
        continue;

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
      if (!rel.Group)
        continue;

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

      D.diagnose(PGD->getHigherThanLoc(),
                 diag::higher_than_precedence_group_cycle, path);
      PGD->setInvalid();
      return;
    }
  } while (!stack.empty());
}

static PrecedenceGroupDecl *
lookupPrecedenceGroup(const PrecedenceGroupDescriptor &descriptor) {
  auto *dc = descriptor.dc;
  if (auto sf = dc->getParentSourceFile()) {
    bool cascading = dc->isCascadingContextForLookup(false);
    return sf->lookupPrecedenceGroup(descriptor.ident, cascading,
                                     descriptor.nameLoc);
  } else {
    return dc->getParentModule()->lookupPrecedenceGroup(descriptor.ident,
                                                        descriptor.nameLoc);
  }
}

static void validatePrecedenceGroup(PrecedenceGroupDecl *PGD) {
  assert(PGD && "Cannot validate a null precedence group!");
  if (PGD->isInvalid())
    return;

  auto &Diags = PGD->getASTContext().Diags;

  // Validate the higherThan relationships.
  bool addedHigherThan = false;
  for (auto &rel : PGD->getMutableHigherThan()) {
    if (rel.Group)
      continue;

    PrecedenceGroupDescriptor desc{PGD->getDeclContext(), rel.Name, rel.NameLoc,
                                   PrecedenceGroupDescriptor::HigherThan};
    auto group = evaluateOrDefault(PGD->getASTContext().evaluator,
                                   LookupPrecedenceGroupRequest{desc}, nullptr);
    if (group) {
      rel.Group = group;
      addedHigherThan = true;
    } else {
      if (!lookupPrecedenceGroup(desc))
        Diags.diagnose(rel.NameLoc, diag::unknown_precedence_group, rel.Name);
      PGD->setInvalid();
    }
  }

  // Validate the lowerThan relationships.
  for (auto &rel : PGD->getMutableLowerThan()) {
    if (rel.Group)
      continue;

    auto dc = PGD->getDeclContext();
    PrecedenceGroupDescriptor desc{PGD->getDeclContext(), rel.Name, rel.NameLoc,
                                   PrecedenceGroupDescriptor::LowerThan};
    auto group = evaluateOrDefault(PGD->getASTContext().evaluator,
                                   LookupPrecedenceGroupRequest{desc}, nullptr);
    bool hadError = false;
    if (group) {
      rel.Group = group;
    } else {
      hadError = true;
      if (auto *rawGroup = lookupPrecedenceGroup(desc)) {
        // We already know the lowerThan path is errant, try to use the results
        // of a raw lookup to enforce the same-module restriction.
        group = rawGroup;
      } else {
        Diags.diagnose(rel.NameLoc, diag::unknown_precedence_group, rel.Name);
      }
    }

    if (group &&
        group->getDeclContext()->getParentModule() == dc->getParentModule()) {
      if (!PGD->isInvalid()) {
        Diags.diagnose(rel.NameLoc, diag::precedence_group_lower_within_module);
        Diags.diagnose(group->getNameLoc(), diag::kind_declared_here,
                       DescriptiveDeclKind::PrecedenceGroup);
      }
      hadError = true;
    }

    if (hadError)
      PGD->setInvalid();
  }

  // Try to diagnose trickier cycles that request evaluation alone can't catch.
  if (addedHigherThan)
    checkPrecedenceCircularity(Diags, PGD);
}

llvm::Expected<PrecedenceGroupDecl *> LookupPrecedenceGroupRequest::evaluate(
    Evaluator &eval, PrecedenceGroupDescriptor descriptor) const {
  if (auto *group = lookupPrecedenceGroup(descriptor)) {
    validatePrecedenceGroup(group);
    return group;
  }

  return nullptr;
}

static Optional<unsigned>
getParamIndex(const ParameterList *paramList, const ParamDecl *decl) {
  ArrayRef<ParamDecl *> params = paramList->getArray();
  for (unsigned i = 0; i < params.size(); ++i) {
    if (params[i] == decl) return i;
  }
  return None;
}

static void checkInheritedDefaultValueRestrictions(ParamDecl *PD) {
  if (PD->getDefaultArgumentKind() != DefaultArgumentKind::Inherited)
    return;

  auto *DC = PD->getInnermostDeclContext();
  const SourceFile *SF = DC->getParentSourceFile();
  assert((SF && SF->Kind == SourceFileKind::Interface || PD->isImplicit()) &&
         "explicit inherited default argument outside of a module interface?");

  // The containing decl should be a designated initializer.
  auto ctor = dyn_cast<ConstructorDecl>(DC);
  if (!ctor || ctor->isConvenienceInit()) {
    PD->diagnose(diag::inherited_default_value_not_in_designated_constructor);
    return;
  }

  // The decl it overrides should also be a designated initializer.
  auto overridden = ctor->getOverriddenDecl();
  if (!overridden || overridden->isConvenienceInit()) {
    PD->diagnose(
        diag::inherited_default_value_used_in_non_overriding_constructor);
    if (overridden)
      overridden->diagnose(diag::overridden_here);
    return;
  }

  // The corresponding parameter should have a default value.
  Optional<unsigned> idx = getParamIndex(ctor->getParameters(), PD);
  assert(idx && "containing decl does not contain param?");
  ParamDecl *equivalentParam = overridden->getParameters()->get(*idx);
  if (equivalentParam->getDefaultArgumentKind() == DefaultArgumentKind::None) {
    PD->diagnose(diag::corresponding_param_not_defaulted);
    equivalentParam->diagnose(diag::inherited_default_param_here);
  }
}

/// Check the default arguments that occur within this pattern.
static void checkDefaultArguments(ParameterList *params) {
  for (auto *param : *params) {
    checkInheritedDefaultValueRestrictions(param);
    if (!param->getDefaultValue() ||
        !param->hasInterfaceType() ||
        param->getInterfaceType()->hasError())
      continue;

    Expr *e = param->getDefaultValue();
    auto *initContext = param->getDefaultArgumentInitContext();

    auto resultTy =
        TypeChecker::typeCheckParameterDefault(e, initContext, param->getType(),
                                               param->isAutoClosure());

    if (resultTy) {
      param->setDefaultValue(e);
    }

    TypeChecker::checkInitializerErrorHandling(initContext, e);

    // Walk the checked initializer and contextualize any closures
    // we saw there.
    (void)TypeChecker::contextualizeInitializer(initContext, e);
  }
}

PrecedenceGroupDecl *TypeChecker::lookupPrecedenceGroup(DeclContext *dc,
                                                        Identifier name,
                                                        SourceLoc nameLoc) {
  return evaluateOrDefault(
      dc->getASTContext().evaluator,
      LookupPrecedenceGroupRequest({dc, name, nameLoc, None}), nullptr);
}

static NominalTypeDecl *resolveSingleNominalTypeDecl(
    DeclContext *DC, SourceLoc loc, Identifier ident, ASTContext &Ctx,
    TypeResolutionFlags flags = TypeResolutionFlags(0)) {
  auto *TyR = new (Ctx) SimpleIdentTypeRepr(loc, ident);
  TypeLoc typeLoc = TypeLoc(TyR);

  TypeResolutionOptions options = TypeResolverContext::TypeAliasDecl;
  options |= flags;
  if (TypeChecker::validateType(Ctx, typeLoc,
                                TypeResolution::forInterface(DC), options))
    return nullptr;

  return typeLoc.getType()->getAnyNominal();
}

static bool checkDesignatedTypes(OperatorDecl *OD,
                                 ArrayRef<Identifier> identifiers,
                                 ArrayRef<SourceLoc> identifierLocs,
                                 ASTContext &ctx) {
  assert(identifiers.size() == identifierLocs.size());

  SmallVector<NominalTypeDecl *, 1> designatedNominalTypes;
  auto *DC = OD->getDeclContext();

  for (auto index : indices(identifiers)) {
    auto *decl = resolveSingleNominalTypeDecl(DC, identifierLocs[index],
                                              identifiers[index], ctx);

    if (!decl)
      return true;

    designatedNominalTypes.push_back(decl);
  }

  OD->setDesignatedNominalTypes(ctx.AllocateCopy(designatedNominalTypes));
  return false;
}

/// Validate the given operator declaration.
///
/// This establishes key invariants, such as an InfixOperatorDecl's
/// reference to its precedence group and the transitive validity of that
/// group.
llvm::Expected<PrecedenceGroupDecl *>
OperatorPrecedenceGroupRequest::evaluate(Evaluator &evaluator,
                                         InfixOperatorDecl *IOD) const {
  auto enableOperatorDesignatedTypes =
      IOD->getASTContext().LangOpts.EnableOperatorDesignatedTypes;

  auto &Diags = IOD->getASTContext().Diags;
  PrecedenceGroupDecl *group = nullptr;

  auto identifiers = IOD->getIdentifiers();
  auto identifierLocs = IOD->getIdentifierLocs();

  if (!identifiers.empty()) {
    group = TypeChecker::lookupPrecedenceGroup(
        IOD->getDeclContext(), identifiers[0], identifierLocs[0]);

    if (group) {
      identifiers = identifiers.slice(1);
      identifierLocs = identifierLocs.slice(1);
    } else {
      // If we're either not allowing types, or we are allowing them
      // and this identifier is not a type, emit an error as if it's
      // a precedence group.
      auto *DC = IOD->getDeclContext();
      if (!(enableOperatorDesignatedTypes &&
            resolveSingleNominalTypeDecl(DC, identifierLocs[0], identifiers[0],
                                         IOD->getASTContext(),
                                         TypeResolutionFlags::SilenceErrors))) {
        Diags.diagnose(identifierLocs[0], diag::unknown_precedence_group,
                       identifiers[0]);
        identifiers = identifiers.slice(1);
        identifierLocs = identifierLocs.slice(1);
      }
    }
  }

  if (!identifiers.empty() && !enableOperatorDesignatedTypes) {
    assert(!group);
    Diags.diagnose(identifierLocs[0], diag::unknown_precedence_group,
                   identifiers[0]);
    identifiers = identifiers.slice(1);
    identifierLocs = identifierLocs.slice(1);
    assert(identifiers.empty() && identifierLocs.empty());
  }

  if (!group) {
    group = TypeChecker::lookupPrecedenceGroup(
        IOD->getDeclContext(), IOD->getASTContext().Id_DefaultPrecedence,
        SourceLoc());
  }

  if (!group) {
    Diags.diagnose(IOD->getLoc(), diag::missing_builtin_precedence_group,
                   IOD->getASTContext().Id_DefaultPrecedence);
  }

  auto nominalTypes = IOD->getDesignatedNominalTypes();
  if (nominalTypes.empty() && enableOperatorDesignatedTypes) {
    if (checkDesignatedTypes(IOD, identifiers, identifierLocs,
                             IOD->getASTContext())) {
      IOD->setInvalid();
    }
  }
  return group;
}

llvm::Expected<SelfAccessKind>
SelfAccessKindRequest::evaluate(Evaluator &evaluator, FuncDecl *FD) const {
  if (FD->getAttrs().getAttribute<MutatingAttr>(true)) {
    if (!FD->isInstanceMember() || !FD->getDeclContext()->hasValueSemantics()) {
      // If this decl is on a class-constrained protocol extension, then
      // respect the explicit mutatingness. Otherwise, we would throw an
      // error.
      if (FD->getDeclContext()->isClassConstrainedProtocolExtension())
        return SelfAccessKind::Mutating;
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

/// Check the requirements in the where clause of the given \c atd
/// to ensure that they don't introduce additional 'Self' requirements.
static void checkProtocolSelfRequirements(ProtocolDecl *proto,
                                          AssociatedTypeDecl *atd) {
  WhereClauseOwner(atd).visitRequirements(
      TypeResolutionStage::Interface,
      [proto](const Requirement &req, RequirementRepr *reqRepr) {
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
  ASTContext &Ctx;

  explicit DeclChecker(ASTContext &ctx) : Ctx(ctx) {}

  ASTContext &getASTContext() const { return Ctx; }

  void visit(Decl *decl) {
    if (getASTContext().Stats)
      getASTContext().Stats->getFrontendCounters().NumDeclsTypechecked++;

    FrontendStatsTracer StatsTracer(getASTContext().Stats, "typecheck-decl",
                                    decl);
    PrettyStackTraceDecl StackTrace("type-checking", decl);
    
    DeclVisitor<DeclChecker>::visit(decl);

    TypeChecker::checkUnsupportedProtocolType(decl);

    if (auto VD = dyn_cast<ValueDecl>(decl)) {
      auto &Context = getASTContext();
      TypeChecker::checkForForbiddenPrefix(Context, VD->getBaseName());
      
      checkRedeclaration(Context, VD);

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
      if (VD->getDeclContext()->isTypeContext() &&
          (VD->getFullName().isSimpleName(Context.Id_Type) ||
           VD->getFullName().isSimpleName(Context.Id_Protocol)) &&
          VD->getNameLoc().isValid() &&
          Context.SourceMgr.extractText({VD->getNameLoc(), 1}) != "`") {
        auto &DE = getASTContext().Diags;
        DE.diagnose(VD->getNameLoc(), diag::reserved_member_name,
                    VD->getFullName(), VD->getBaseName().getIdentifier().str());
        DE.diagnose(VD->getNameLoc(), diag::backticks_to_escape)
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
    TypeChecker::checkDeclAttributes(ID);
  }

  void visitOperatorDecl(OperatorDecl *OD) {
    TypeChecker::checkDeclAttributes(OD);
    auto &Ctx = OD->getASTContext();
    if (auto *IOD = dyn_cast<InfixOperatorDecl>(OD)) {
      (void)IOD->getPrecedenceGroup();
    } else {
      auto nominalTypes = OD->getDesignatedNominalTypes();
      if (nominalTypes.empty() && Ctx.LangOpts.EnableOperatorDesignatedTypes) {
        auto identifiers = OD->getIdentifiers();
        auto identifierLocs = OD->getIdentifierLocs();
        if (checkDesignatedTypes(OD, identifiers, identifierLocs, Ctx))
          OD->setInvalid();
      }
      return;
    }
    checkAccessControl(OD);
  }

  void visitPrecedenceGroupDecl(PrecedenceGroupDecl *PGD) {
    TypeChecker::checkDeclAttributes(PGD);
    validatePrecedenceGroup(PGD);
    checkAccessControl(PGD);
  }

  void visitMissingMemberDecl(MissingMemberDecl *MMD) {
    llvm_unreachable("should always be type-checked already");
  }

  void visitBoundVariable(VarDecl *VD) {
    // WARNING: Anything you put in this function will only be run when the
    // VarDecl is fully type-checked within its own file. It will NOT be run
    // when the VarDecl is merely used from another file.

    // Compute these requests in case they emit diagnostics.
    (void) VD->getInterfaceType();
    (void) VD->isGetterMutating();
    (void) VD->isSetterMutating();
    (void) VD->getPropertyWrapperBackingProperty();
    (void) VD->getImplInfo();

    // Add the '@_hasStorage' attribute if this property is stored.
    if (VD->hasStorage() && !VD->getAttrs().hasAttribute<HasStorageAttr>())
      VD->getAttrs().add(new (getASTContext())
                             HasStorageAttr(/*isImplicit=*/true));

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
          VD->diagnose(diag::unimplemented_static_var, diagSel,
                       PBD->getStaticSpelling(), diagSel == Classes)
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

    TypeChecker::checkDeclAttributes(VD);

    if (!checkOverrides(VD)) {
      // If a property has an override attribute but does not override
      // anything, complain.
      auto overridden = VD->getOverriddenDecl();
      if (auto *OA = VD->getAttrs().getAttribute<OverrideAttr>()) {
        if (!overridden) {
          VD->diagnose(diag::property_does_not_override)
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
    
    checkForEmptyOptionSet(VD);

    // Under the Swift 3 inference rules, if we have @IBInspectable or
    // @GKInspectable but did not infer @objc, warn that the attribute is
    auto &DE = getASTContext().Diags;
    if (!VD->isObjC() &&
        VD->getASTContext().LangOpts.EnableSwift3ObjCInference) {
      if (auto attr = VD->getAttrs().getAttribute<IBInspectableAttr>()) {
        DE.diagnose(attr->getLocation(),
                    diag::attribute_meaningless_when_nonobjc,
                    attr->getAttrName())
            .fixItRemove(attr->getRange());
      }

      if (auto attr = VD->getAttrs().getAttribute<GKInspectableAttr>()) {
        DE.diagnose(attr->getLocation(),
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


  void visitPatternBindingDecl(PatternBindingDecl *PBD) {
    DeclContext *DC = PBD->getDeclContext();

    TypeChecker::checkDeclAttributes(PBD);

    bool isInSILMode = false;
    if (auto sourceFile = DC->getParentSourceFile())
      isInSILMode = sourceFile->Kind == SourceFileKind::SIL;
    bool isTypeContext = DC->isTypeContext();

    auto &Ctx = getASTContext();
    for (auto i : range(PBD->getNumPatternEntries())) {
      const auto *entry = evaluateOrDefault(
          Ctx.evaluator, PatternBindingEntryRequest{PBD, i}, nullptr);
      assert(entry && "No pattern binding entry?");

      PBD->getPattern(i)->forEachVariable([&](VarDecl *var) {
        this->visitBoundVariable(var);

        if (PBD->isInitialized(i)) {
          // Add the attribute that preserves the "has an initializer" value
          // across module generation, as required for TBDGen.
          if (var->hasStorage() &&
              !var->getAttrs().hasAttribute<HasInitialValueAttr>()) {
            var->getAttrs().add(new (Ctx)
                                    HasInitialValueAttr(/*IsImplicit=*/true));
          }
          return;
        }

        // If this is a declaration without an initializer, reject code if
        // uninitialized vars are not allowed.
        if (isInSILMode) return;

        // If the variable has no storage, it never needs an initializer.
        if (!var->hasStorage())
          return;

        if (var->isInvalid() || PBD->isInvalid())
          return;

        auto markVarAndPBDInvalid = [PBD, var] {
          PBD->setInvalid();
          var->setInvalid();
        };
        
        // Properties with an opaque return type need an initializer to
        // determine their underlying type.
        if (var->getOpaqueResultTypeDecl()) {
          var->diagnose(diag::opaque_type_var_no_init);
        }

        // Non-member observing properties need an initializer.
        if (var->getWriteImpl() == WriteImplKind::StoredWithObservers &&
            !isTypeContext) {
          var->diagnose(diag::observingprop_requires_initializer);
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

          var->diagnose(diag::static_requires_initializer,
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

          var->diagnose(diag::global_requires_initializer, var->isLet());
          markVarAndPBDInvalid();
          return;
        }
      });
    }

    TypeChecker::checkDeclAttributes(PBD);

    checkAccessControl(PBD);

    // If the initializers in the PBD aren't checked yet, do so now.
    for (auto i : range(PBD->getNumPatternEntries())) {
      if (!PBD->isInitialized(i))
        continue;

      if (!PBD->isInitializerChecked(i)) {
        TypeChecker::typeCheckPatternBinding(PBD, i);
      }

      if (!PBD->isInvalid()) {
        auto *init = PBD->getInit(i);

        // If we're performing an binding to a weak or unowned variable from a
        // constructor call, emit a warning that the instance will be immediately
        // deallocated.
        diagnoseUnownedImmediateDeallocation(Ctx, PBD->getPattern(i),
                                             PBD->getEqualLoc(i),
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
              PBD->getInitContext(i));
          if (initContext) {
            // Check safety of error-handling in the declaration, too.
            TypeChecker::checkInitializerErrorHandling(initContext, init);
            (void)TypeChecker::contextualizeInitializer(initContext, init);
          }
        }
      }
    }
  }

  void visitSubscriptDecl(SubscriptDecl *SD) {
    // Force requests that can emit diagnostics.
    (void) SD->getInterfaceType();
    (void) SD->getGenericSignature();

    if (!SD->isInvalid()) {
      TypeChecker::checkReferencedGenericParams(SD);
      checkGenericParams(SD);
      TypeChecker::checkProtocolSelfRequirements(SD);
    }

    TypeChecker::checkDeclAttributes(SD);

    checkAccessControl(SD);

    if (!checkOverrides(SD)) {
      // If a subscript has an override attribute but does not override
      // anything, complain.
      if (auto *OA = SD->getAttrs().getAttribute<OverrideAttr>()) {
        if (!SD->getOverriddenDecl()) {
          SD->diagnose(diag::subscript_does_not_override)
              .highlight(OA->getLocation());
          OA->setInvalid();
        }
      }
    }

    // Compute these requests in case they emit diagnostics.
    (void) SD->isGetterMutating();
    (void) SD->isSetterMutating();
    (void) SD->getImplInfo();

    TypeChecker::checkParameterAttributes(SD->getIndices());

    checkDefaultArguments(SD->getIndices());

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
    // Force requests that can emit diagnostics.
    (void) TAD->getGenericSignature();
    (void) TAD->getUnderlyingType();

    TypeChecker::checkDeclAttributes(TAD);
    checkAccessControl(TAD);
  }
  
  void visitOpaqueTypeDecl(OpaqueTypeDecl *OTD) {
    // Force requests that can emit diagnostics.
    (void) OTD->getGenericSignature();

    TypeChecker::checkDeclAttributes(OTD);
    checkAccessControl(OTD);
  }
  
  void visitAssociatedTypeDecl(AssociatedTypeDecl *AT) {
    TypeChecker::checkDeclAttributes(AT);

    checkInheritanceClause(AT);
    auto *proto = AT->getProtocol();

    checkProtocolSelfRequirements(proto, AT);

    if (proto->isObjC()) {
      AT->diagnose(diag::associated_type_objc, AT->getName(), proto->getName());
    }

    checkAccessControl(AT);

    // Trigger the checking for overridden declarations.
    (void) AT->getOverriddenDecls();

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
        auto &DE = getASTContext().Diags;
        DE.diagnose(AT->getDefaultDefinitionTypeRepr()->getLoc(),
                    diag::recursive_decl_reference, AT->getDescriptiveKind(),
                    AT->getName());
        AT->diagnose(diag::kind_declared_here, DescriptiveDeclKind::Type);
      }
    }
  }

  void checkUnsupportedNestedType(NominalTypeDecl *NTD) {
    auto *DC = NTD->getDeclContext();
    if (DC->getResilienceExpansion() == ResilienceExpansion::Minimal) {
      auto kind = TypeChecker::getFragileFunctionKind(DC);
      NTD->diagnose(diag::local_type_in_inlinable_function, NTD->getFullName(),
                    static_cast<unsigned>(kind.first));
    }

    // We don't support protocols outside the top level of a file.
    if (isa<ProtocolDecl>(NTD) &&
        !NTD->getParent()->isModuleScopeContext()) {
      NTD->diagnose(diag::unsupported_nested_protocol, NTD->getName());
      NTD->setInvalid();
      return;
    }

    // We don't support nested types in generics yet.
    if (NTD->isGenericContext()) {
      auto DC = NTD->getDeclContext();
      if (auto proto = DC->getSelfProtocolDecl()) {
        if (DC->getExtendedProtocolDecl()) {
          NTD->diagnose(diag::unsupported_type_nested_in_protocol_extension,
                        NTD->getName(), proto->getName());
        } else {
          NTD->diagnose(diag::unsupported_type_nested_in_protocol,
                        NTD->getName(), proto->getName());
        }
      }

      if (DC->isLocalContext() && DC->isGenericContext()) {
        // A local generic context is a generic function.
        if (auto AFD = dyn_cast<AbstractFunctionDecl>(DC)) {
          NTD->diagnose(diag::unsupported_type_nested_in_generic_function,
                        NTD->getName(), AFD->getFullName());
        } else {
          NTD->diagnose(diag::unsupported_type_nested_in_generic_closure,
                        NTD->getName());
        }
      }
    }
  }

  void visitEnumDecl(EnumDecl *ED) {
    checkUnsupportedNestedType(ED);

    // FIXME: Remove this once we clean up the mess involving raw values.
    (void) ED->getInterfaceType();

    checkGenericParams(ED);

    // Check for circular inheritance of the raw type.
    (void)ED->hasCircularRawValue();

    for (Decl *member : ED->getMembers())
      visit(member);

    TypeChecker::checkDeclAttributes(ED);

    checkInheritanceClause(ED);

    checkAccessControl(ED);

    TypeChecker::checkPatternBindingCaptures(ED);

    auto &DE = getASTContext().Diags;
    if (auto rawTy = ED->getRawType()) {
      // The raw type must be one of the blessed literal convertible types.
      if (!computeAutomaticEnumValueKind(ED)) {
        DE.diagnose(ED->getInherited().front().getSourceRange().Start,
                    diag::raw_type_not_literal_convertible, rawTy);
        ED->getInherited().front().setInvalidType(getASTContext());
      }
      
      // We need at least one case to have a raw value.
      if (ED->getAllElements().empty()) {
        DE.diagnose(ED->getInherited().front().getSourceRange().Start,
                    diag::empty_enum_raw_type);
      }
    }

    checkExplicitAvailability(ED);

    TypeChecker::checkDeclCircularity(ED);

    TypeChecker::checkConformancesInContext(ED, ED);
  }

  void visitStructDecl(StructDecl *SD) {
    checkUnsupportedNestedType(SD);

    checkGenericParams(SD);

    // Force lowering of stored properties.
    (void) SD->getStoredProperties();

    TypeChecker::addImplicitConstructors(SD);

    for (Decl *Member : SD->getMembers())
      visit(Member);

    TypeChecker::checkPatternBindingCaptures(SD);

    TypeChecker::checkDeclAttributes(SD);

    checkInheritanceClause(SD);

    checkAccessControl(SD);

    checkExplicitAvailability(SD);

    TypeChecker::checkDeclCircularity(SD);

    TypeChecker::checkConformancesInContext(SD, SD);
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
      for (auto idx : range(pbd->getNumPatternEntries()))
        pbd->getPattern(idx)->collectVariables(vars);
      bool suggestNSManaged = propertiesCanBeNSManaged(cd, vars);
      switch (vars.size()) {
      case 0:
        llvm_unreachable("should have been marked invalid");

      case 1:
        pbd->diagnose(diag::missing_in_class_init_1, vars[0]->getName(),
                      suggestNSManaged);
        break;

      case 2:
        pbd->diagnose(diag::missing_in_class_init_2, vars[0]->getName(),
                      vars[1]->getName(), suggestNSManaged);
        break;

      case 3:
        pbd->diagnose(diag::missing_in_class_init_3plus, vars[0]->getName(),
                      vars[1]->getName(), vars[2]->getName(), false,
                      suggestNSManaged);
        break;

      default:
        pbd->diagnose(diag::missing_in_class_init_3plus, vars[0]->getName(),
                      vars[1]->getName(), vars[2]->getName(), true,
                      suggestNSManaged);
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
      source->diagnose(diag::requires_stored_property_inits_here,
                       source->getDeclaredType(), cd == source,
                       suggestNSManaged);
    }
  }


  void visitClassDecl(ClassDecl *CD) {
    checkUnsupportedNestedType(CD);

    // Force creation of the generic signature.
    (void) CD->getGenericSignature();

    checkGenericParams(CD);

    // Check for circular inheritance.
    (void)CD->hasCircularInheritance();

    // Force lowering of stored properties.
    (void) CD->getStoredProperties();

    // Force creation of an implicit destructor, if any.
    (void) CD->getDestructor();

    for (Decl *Member : CD->getEmittedMembers())
      visit(Member);

    TypeChecker::checkPatternBindingCaptures(CD);

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
        CD->diagnose(diag::inheritance_from_final_class, Super->getName());
        // FIXME: should this really be skipping the rest of decl-checking?
        return;
      }

      if (Super->hasClangNode() && Super->getGenericParams()
          && superclassTy->hasTypeParameter()) {
        CD->diagnose(diag::inheritance_from_unspecialized_objc_generic_class,
                     Super->getName());
      }

      switch (Super->getForeignClassKind()) {
      case ClassDecl::ForeignKind::Normal:
        break;
      case ClassDecl::ForeignKind::CFType:
        CD->diagnose(diag::inheritance_from_cf_class, Super->getName());
        isInvalidSuperclass = true;
        break;
      case ClassDecl::ForeignKind::RuntimeOnly:
        CD->diagnose(diag::inheritance_from_objc_runtime_visible_class,
                     Super->getName());
        isInvalidSuperclass = true;
        break;
      }

      if (!isInvalidSuperclass && Super->hasMissingVTableEntries() &&
          !Super->isResilient(CD->getParentModule(),
                              ResilienceExpansion::Minimal)) {
        auto *superFile = Super->getModuleScopeContext();
        if (auto *serialized = dyn_cast<SerializedASTFile>(superFile)) {
          const auto effVersion =
              CD->getASTContext().LangOpts.EffectiveLanguageVersion;
          if (serialized->getLanguageVersionBuiltWith() != effVersion) {
            CD->diagnose(
                diag::
                    inheritance_from_class_with_missing_vtable_entries_versioned,
                Super->getName(), serialized->getLanguageVersionBuiltWith(),
                effVersion);
            isInvalidSuperclass = true;
          }
        }
        if (!isInvalidSuperclass) {
          CD->diagnose(diag::inheritance_from_class_with_missing_vtable_entries,
                       Super->getName());
          isInvalidSuperclass = true;
        }
      }

      if (!getASTContext().isAccessControlDisabled()) {
        // Require the superclass to be open if this is outside its
        // defining module.  But don't emit another diagnostic if we
        // already complained about the class being inherently
        // un-subclassable.
        if (!isInvalidSuperclass &&
            !Super->hasOpenAccess(CD->getDeclContext()) &&
            Super->getModuleContext() != CD->getModuleContext()) {
          CD->diagnose(diag::superclass_not_open, superclassTy);
          isInvalidSuperclass = true;
        }

        // Require superclasses to be open if the subclass is open.
        // This is a restriction we can consider lifting in the future,
        // e.g. to enable a "sealed" superclass whose subclasses are all
        // of one of several alternatives.
        if (!isInvalidSuperclass &&
            CD->getFormalAccess() == AccessLevel::Open &&
            Super->getFormalAccess() != AccessLevel::Open) {
          CD->diagnose(diag::superclass_of_open_not_open, superclassTy);
          Super->diagnose(diag::superclass_here);
        }
      }
    }

    TypeChecker::checkDeclAttributes(CD);

    checkInheritanceClause(CD);

    checkAccessControl(CD);

    checkExplicitAvailability(CD);

    TypeChecker::checkDeclCircularity(CD);

    TypeChecker::checkConformancesInContext(CD, CD);

    TypeChecker::maybeDiagnoseClassWithoutInitializers(CD);
  }

  void visitProtocolDecl(ProtocolDecl *PD) {
    checkUnsupportedNestedType(PD);

    // Check for circular inheritance within the protocol.
    (void)PD->hasCircularInheritedProtocols();

    auto *SF = PD->getParentSourceFile();
    if (SF) {
      if (auto *tracker = SF->getReferencedNameTracker()) {
        bool isNonPrivate = (PD->getFormalAccess() > AccessLevel::FilePrivate);
        for (auto *parentProto : PD->getInheritedProtocols())
          tracker->addUsedMember({parentProto, Identifier()}, isNonPrivate);
      }
    }

    // Check the members.
    for (auto Member : PD->getMembers())
      visit(Member);

    TypeChecker::checkDeclAttributes(PD);

    checkAccessControl(PD);

    checkInheritanceClause(PD);

    TypeChecker::checkDeclCircularity(PD);
    if (PD->isResilient())
      if (!SF || SF->Kind != SourceFileKind::Interface)
        TypeChecker::inferDefaultWitnesses(PD);

    if (PD->getASTContext().LangOpts.DebugGenericSignatures) {
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
        CanGenericSignature::getCanonical(requirementsSig->getGenericParams(),
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


  bool shouldSkipBodyTypechecking(const AbstractFunctionDecl *AFD) {
    // FIXME: Remove TypeChecker dependency.
    auto &TC = *Ctx.getLegacyGlobalTypeChecker();

    // Make sure we're in the mode that's skipping function bodies.
    if (!TC.canSkipNonInlinableBodies())
      return false;

    // Make sure there even _is_ a body that we can skip.
    if (!AFD->getBodySourceRange().isValid())
      return false;

    // If we're gonna serialize the body, we can't skip it.
    if (AFD->getResilienceExpansion() == ResilienceExpansion::Minimal)
      return false;

    return true;
  }

  void visitFuncDecl(FuncDecl *FD) {
    // Force these requests in case they emit diagnostics.
    (void) FD->getInterfaceType();
    (void) FD->getOperatorDecl();

    if (!FD->isInvalid()) {
      checkGenericParams(FD);
      TypeChecker::checkReferencedGenericParams(FD);
      TypeChecker::checkProtocolSelfRequirements(FD);
    }

    checkAccessControl(FD);

    TypeChecker::checkParameterAttributes(FD->getParameters());
    TypeChecker::checkDeclAttributes(FD);

    if (!checkOverrides(FD)) {
      // If a method has an 'override' keyword but does not
      // override anything, complain.
      if (auto *OA = FD->getAttrs().getAttribute<OverrideAttr>()) {
        if (!FD->getOverriddenDecl()) {
          FD->diagnose(diag::method_does_not_override)
              .highlight(OA->getLocation());
          OA->setInvalid();
        }
      }
    }

    // FIXME: Remove TypeChecker dependencies below.
    auto &TC = *Ctx.getLegacyGlobalTypeChecker();
    if (requiresDefinition(FD) && !FD->hasBody()) {
      // Complain if we should have a body.
      FD->diagnose(diag::func_decl_without_brace);
    } else if (FD->getDeclContext()->isLocalContext()) {
      // Check local function bodies right away.
      TypeChecker::typeCheckAbstractFunctionBody(FD);
    } else if (shouldSkipBodyTypechecking(FD)) {
      FD->setBodySkipped(FD->getBodySourceRange());
    } else {
      // Record the body.
      TC.definedFunctions.push_back(FD);
    }

    checkExplicitAvailability(FD);

    if (FD->getDeclContext()->getSelfClassDecl())
      checkDynamicSelfType(FD, FD->getResultInterfaceType());

    checkDefaultArguments(FD->getParameters());

    // Validate 'static'/'class' on functions in extensions.
    auto StaticSpelling = FD->getStaticSpelling();
    if (StaticSpelling != StaticSpellingKind::None &&
        isa<ExtensionDecl>(FD->getDeclContext())) {
      if (auto *NTD = FD->getDeclContext()->getSelfNominalTypeDecl()) {
        if (!isa<ClassDecl>(NTD)) {
          if (StaticSpelling == StaticSpellingKind::KeywordClass) {
            FD->diagnose(diag::class_func_not_in_class, false)
                .fixItReplace(FD->getStaticLoc(), "static");
            NTD->diagnose(diag::extended_type_declared_here);
          }
        }
      }
    }

    // Member functions need some special validation logic.
    if (FD->getDeclContext()->isTypeContext()) {
      if (FD->isOperator() && !isMemberOperator(FD, nullptr)) {
        auto selfNominal = FD->getDeclContext()->getSelfNominalTypeDecl();
        auto isProtocol = selfNominal && isa<ProtocolDecl>(selfNominal);
        // We did not find 'Self'. Complain.
        FD->diagnose(diag::operator_in_unrelated_type,
                     FD->getDeclContext()->getDeclaredInterfaceType(), isProtocol,
                     FD->getFullName());
      }
    }

    // If the function is exported to C, it must be representable in (Obj-)C.
    // FIXME: This needs to be moved to its own request if we want to
    // productize @_cdecl.
    if (auto CDeclAttr = FD->getAttrs().getAttribute<swift::CDeclAttr>()) {
      Optional<ForeignErrorConvention> errorConvention;
      if (isRepresentableInObjC(FD, ObjCReason::ExplicitlyCDecl,
                                errorConvention)) {
        if (FD->hasThrows()) {
          FD->setForeignErrorConvention(*errorConvention);
          getASTContext().Diags.diagnose(CDeclAttr->getLocation(),
                                         diag::cdecl_throws);
        }
      }
    }
  }

  void visitModuleDecl(ModuleDecl *) { }

  void visitEnumCaseDecl(EnumCaseDecl *ECD) {
    // The type-checker doesn't care about how these are grouped.
  }

  void visitEnumElementDecl(EnumElementDecl *EED) {
    (void) EED->getInterfaceType();
    auto *ED = EED->getParentEnum();

    TypeChecker::checkDeclAttributes(EED);

    if (auto *PL = EED->getParameterList()) {
      TypeChecker::checkParameterAttributes(PL);

      checkDefaultArguments(PL);
    }

    auto &DE = getASTContext().Diags;
    // We don't yet support raw values on payload cases.
    if (EED->hasAssociatedValues()) {
      if (auto rawTy = ED->getRawType()) {
        EED->diagnose(diag::enum_with_raw_type_case_with_argument);
        DE.diagnose(ED->getInherited().front().getSourceRange().Start,
                    diag::enum_raw_type_here, rawTy);
        EED->setInvalid();
      }
    }

    // Force the raw value expr then yell if our parent doesn't have a raw type.
    Expr *RVE = EED->getRawValueExpr();
    if (RVE && !ED->hasRawType()) {
      DE.diagnose(RVE->getLoc(), diag::enum_raw_value_without_raw_type);
      EED->setInvalid();
    }

    checkAccessControl(EED);
  }

  void visitExtensionDecl(ExtensionDecl *ED) {
    // Produce any diagnostics for the extended type.
    auto extType = ED->getExtendedType();

    auto nominal = ED->computeExtendedNominal();
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

    // Produce any diagnostics for the generic signature.
    (void) ED->getGenericSignature();

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

    // Only generic and protocol types are permitted to have
    // trailing where clauses.
    if (auto trailingWhereClause = ED->getTrailingWhereClause()) {
      if (!ED->getGenericParams() && !ED->isInvalid()) {
        ED->diagnose(diag::extension_nongeneric_trailing_where,
                     nominal->getFullName())
          .highlight(trailingWhereClause->getSourceRange());
      }
    }

    checkGenericParams(ED);

    for (Decl *Member : ED->getMembers())
      visit(Member);

    TypeChecker::checkConformancesInContext(ED, ED);

    TypeChecker::checkDeclAttributes(ED);
    checkAccessControl(ED);

    checkExplicitAvailability(ED);
  }

  void visitTopLevelCodeDecl(TopLevelCodeDecl *TLCD) {
    // See swift::performTypeChecking for TopLevelCodeDecl handling.
    llvm_unreachable("TopLevelCodeDecls are handled elsewhere");
  }
  
  void visitIfConfigDecl(IfConfigDecl *ICD) {
    // The active members of the #if block will be type checked along with
    // their enclosing declaration.
    TypeChecker::checkDeclAttributes(ICD);
  }

  void visitPoundDiagnosticDecl(PoundDiagnosticDecl *PDD) {
    if (PDD->hasBeenEmitted()) { return; }
    PDD->markEmitted();
    getASTContext()
        .Diags
        .diagnose(PDD->getMessage()->getStartLoc(),
                  PDD->isError() ? diag::pound_error : diag::pound_warning,
                  PDD->getMessage()->getValue())
        .highlight(PDD->getMessage()->getSourceRange());
  }

  void visitConstructorDecl(ConstructorDecl *CD) {
    (void) CD->getInterfaceType();

    // Compute these requests in case they emit diagnostics.
    (void) CD->getInitKind();

    if (!CD->isInvalid()) {
      checkGenericParams(CD);
      TypeChecker::checkReferencedGenericParams(CD);
      TypeChecker::checkProtocolSelfRequirements(CD);
    }

    TypeChecker::checkDeclAttributes(CD);
    TypeChecker::checkParameterAttributes(CD->getParameters());

    // Check whether this initializer overrides an initializer in its
    // superclass.
    if (!checkOverrides(CD)) {
      // If an initializer has an override attribute but does not override
      // anything or overrides something that doesn't need an 'override'
      // keyword (e.g., a convenience initializer), complain.
      // anything, or overrides something that complain.
      if (auto *attr = CD->getAttrs().getAttribute<OverrideAttr>()) {
        if (!CD->getOverriddenDecl()) {
          CD->diagnose(diag::initializer_does_not_override)
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
              CD->diagnose(diag::required_initializer_override_keyword)
                  .fixItRemove(attr->getLocation());
            } else {
              CD->diagnose(diag::required_initializer_override_wrong_keyword)
                  .fixItReplace(attr->getLocation(), "required");
              CD->getAttrs().add(new (getASTContext())
                                     RequiredAttr(/*IsImplicit=*/true));
            }

            auto *reqInit =
                findNonImplicitRequiredInit(CD->getOverriddenDecl());
            reqInit->diagnose(diag::overridden_required_initializer_here);
          } else {
            // We tried to override a convenience initializer.
            CD->diagnose(diag::initializer_does_not_override)
                .highlight(attr->getLocation());
            CD->getOverriddenDecl()->diagnose(
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
        CD->diagnose(diag::failable_initializer_override, CD->getFullName());
        auto *OD = CD->getOverriddenDecl();
        OD->diagnose(diag::nonfailable_initializer_override_here,
                     OD->getFullName());
      }
    }

    // If this initializer overrides a 'required' initializer, it must itself
    // be marked 'required'.
    if (!CD->getAttrs().hasAttribute<RequiredAttr>()) {
      if (CD->getOverriddenDecl() && CD->getOverriddenDecl()->isRequired()) {
        CD->diagnose(diag::required_initializer_missing_keyword)
            .fixItInsert(CD->getLoc(), "required ");

        auto *reqInit = findNonImplicitRequiredInit(CD->getOverriddenDecl());
        reqInit->diagnose(diag::overridden_required_initializer_here);

        CD->getAttrs().add(new (getASTContext())
                               RequiredAttr(/*IsImplicit=*/true));
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
          auto diag = CD->diagnose(diag::required_initializer_not_accessible,
                                   nominal->getFullName());
          fixItAccess(diag, CD, requiredAccess);
        }
      }
    }

    checkAccessControl(CD);

    // FIXME: Remove TypeChecker dependencies below.
    auto &TC = *Ctx.getLegacyGlobalTypeChecker();
    if (requiresDefinition(CD) && !CD->hasBody()) {
      // Complain if we should have a body.
      CD->diagnose(diag::missing_initializer_def);
    } else if (CD->getDeclContext()->isLocalContext()) {
      // Check local function bodies right away.
      TypeChecker::typeCheckAbstractFunctionBody(CD);
    } else if (shouldSkipBodyTypechecking(CD)) {
      CD->setBodySkipped(CD->getBodySourceRange());
    } else {
      TC.definedFunctions.push_back(CD);
    }

    checkDefaultArguments(CD->getParameters());
  }

  void visitDestructorDecl(DestructorDecl *DD) {
    TypeChecker::checkDeclAttributes(DD);

    if (DD->getDeclContext()->isLocalContext()) {
      // Check local function bodies right away.
      TypeChecker::typeCheckAbstractFunctionBody(DD);
    } else if (shouldSkipBodyTypechecking(DD)) {
      DD->setBodySkipped(DD->getBodySourceRange());
    } else {
      // FIXME: Remove TypeChecker dependency.
      auto &TC = *Ctx.getLegacyGlobalTypeChecker();
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

  auto &Context = proto->getASTContext();
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
  DeclChecker(D->getASTContext()).visit(D);
}

// Returns 'nullptr' if this is the setter's 'newValue' parameter;
// otherwise, returns the corresponding parameter of the subscript
// declaration.

static ParamDecl *getOriginalParamFromAccessor(AbstractStorageDecl *storage,
                                               AccessorDecl *accessor,
                                               ParamDecl *param) {
  auto *accessorParams = accessor->getParameters();
  unsigned startIndex = 0;

  switch (accessor->getAccessorKind()) {
  case AccessorKind::DidSet:
  case AccessorKind::WillSet:
  case AccessorKind::Set:
    if (param == accessorParams->get(0)) {
      // This is the 'newValue' parameter.
      return nullptr;
    }

    startIndex = 1;
    break;

  default:
    startIndex = 0;
    break;
  }

  // If the parameter is not the 'newValue' parameter to a setter, it
  // must be a subscript index parameter (or we have an invalid AST).
  auto *subscript = cast<SubscriptDecl>(storage);
  auto *subscriptParams = subscript->getIndices();

  auto where = llvm::find_if(*accessorParams,
                              [param](ParamDecl *other) {
                                return other == param;
                              });
  assert(where != accessorParams->end());
  unsigned index = where - accessorParams->begin();

  return subscriptParams->get(index - startIndex);
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
      TyR = cast<VarDecl>(storage)->getTypeReprOrParentPatternTypeRepr();
    break;
  }

  case DeclKind::Subscript:
    TyR = cast<SubscriptDecl>(decl)->getElementTypeLoc().getTypeRepr();
    break;

  case DeclKind::Param: {
    auto *param = cast<ParamDecl>(decl);
    if (param->isSelfParameter())
      return false;

    if (auto *accessor = dyn_cast<AccessorDecl>(param->getDeclContext())) {
      auto *storage = accessor->getStorage();
      auto *originalParam = getOriginalParamFromAccessor(
        storage, accessor, param);
      if (originalParam == nullptr) {
        // This is the setter's newValue parameter.
        return storage->isImplicitlyUnwrappedOptional();
      }

      if (param != originalParam) {
        // This is the 'subscript(...) { get { ... } set { ... } }' case.
        // This means we cloned the parameter list for each accessor.
        // Delegate to the original parameter.
        return originalParam->isImplicitlyUnwrappedOptional();
      }

      // This is the 'subscript(...) { <<body of getter>> }' case.
      // The subscript and the getter share their ParamDecls.
      // Fall through.
    }

    // Handle eg, 'inout Int!' or '__owned NSObject!'.
    TyR = param->getTypeRepr();
    if (auto *STR = dyn_cast_or_null<SpecifierTypeRepr>(TyR))
      TyR = STR->getBase();
    break;
  }

  case DeclKind::Var:
    TyR = cast<VarDecl>(decl)->getTypeReprOrParentPatternTypeRepr();
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
    if (auto classDecl = dc->getSelfClassDecl()) {
      // For a class, we also need the function or class to be 'final'.
      if (!classDecl->isFinal() && !FD->isFinal() &&
          FD->getStaticLoc().isValid() &&
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

static Type buildAddressorResultType(AccessorDecl *addressor,
                                     Type valueType) {
  assert(addressor->getAccessorKind() == AccessorKind::Address ||
         addressor->getAccessorKind() == AccessorKind::MutableAddress);

  PointerTypeKind pointerKind =
    (addressor->getAccessorKind() == AccessorKind::Address)
      ? PTK_UnsafePointer
      : PTK_UnsafeMutablePointer;
  return valueType->wrapInPointer(pointerKind);
}

llvm::Expected<Type>
ResultTypeRequest::evaluate(Evaluator &evaluator, ValueDecl *decl) const {
  auto &ctx = decl->getASTContext();

  // Accessors always inherit their result type from their storage.
  if (auto *accessor = dyn_cast<AccessorDecl>(decl)) {
    auto *storage = accessor->getStorage();

    switch (accessor->getAccessorKind()) {
    // For getters, set the result type to the value type.
    case AccessorKind::Get:
      return storage->getValueInterfaceType();

    // For setters and observers, set the old/new value parameter's type
    // to the value type.
    case AccessorKind::DidSet:
    case AccessorKind::WillSet:
    case AccessorKind::Set:
      return TupleType::getEmpty(ctx);

    // Addressor result types can get complicated because of the owner.
    case AccessorKind::Address:
    case AccessorKind::MutableAddress:
      return buildAddressorResultType(accessor, storage->getValueInterfaceType());

    // Coroutine accessors don't mention the value type directly.
    // If we add yield types to the function type, we'll need to update this.
    case AccessorKind::Read:
    case AccessorKind::Modify:
      return TupleType::getEmpty(ctx);
    }
  }

  auto *resultTyRepr = getResultTypeLoc().getTypeRepr();

  // Nothing to do if there's no result type.
  if (resultTyRepr == nullptr)
    return TupleType::getEmpty(ctx);

  // Handle opaque types.
  if (decl->getOpaqueResultTypeRepr()) {
    auto *opaqueDecl = decl->getOpaqueResultTypeDecl();
    return (opaqueDecl
            ? opaqueDecl->getDeclaredInterfaceType()
            : ErrorType::get(ctx));
  }

  auto *dc = decl->getInnermostDeclContext();
  auto resolution = TypeResolution::forInterface(dc);
  return resolution.resolveType(
      resultTyRepr, TypeResolverContext::FunctionResult);
}

llvm::Expected<ParamSpecifier>
ParamSpecifierRequest::evaluate(Evaluator &evaluator,
                                ParamDecl *param) const {
  auto *dc = param->getDeclContext();

  if (param->isSelfParameter()) {
    auto selfParam = computeSelfParam(cast<AbstractFunctionDecl>(dc),
                                      /*isInitializingCtor*/true,
                                      /*wantDynamicSelf*/false);
    return (selfParam.getParameterFlags().isInOut()
            ? ParamSpecifier::InOut
            : ParamSpecifier::Default);
  }

  if (auto *accessor = dyn_cast<AccessorDecl>(dc)) {
    auto *storage = accessor->getStorage();
    auto *originalParam = getOriginalParamFromAccessor(
      storage, accessor, param);
    if (originalParam == nullptr) {
      // This is the setter's newValue parameter. Note that even though
      // the AST uses the 'Default' specifier, SIL will lower this to a
      // +1 parameter.
      return ParamSpecifier::Default;
    }

    if (param != originalParam) {
      // This is the 'subscript(...) { get { ... } set { ... } }' case.
      // This means we cloned the parameter list for each accessor.
      // Delegate to the original parameter.
      return originalParam->getSpecifier();
    }

    // This is the 'subscript(...) { <<body of getter>> }' case.
    // The subscript and the getter share their ParamDecls.
    // Fall through.
  }

  auto typeRepr = param->getTypeRepr();
  assert(typeRepr != nullptr && "Should call setSpecifier() on "
         "synthesized parameter declarations");

  auto *nestedRepr = typeRepr;

  // Look through parens here; other than parens, specifiers
  // must appear at the top level of a parameter type.
  while (auto *tupleRepr = dyn_cast<TupleTypeRepr>(nestedRepr)) {
    if (!tupleRepr->isParenType())
      break;
    nestedRepr = tupleRepr->getElementType(0);
  }

  if (isa<InOutTypeRepr>(nestedRepr) &&
      param->isDefaultArgument()) {
    auto &ctx = param->getASTContext();
    ctx.Diags.diagnose(param->getDefaultValue()->getLoc(),
                       swift::diag::cannot_provide_default_value_inout,
                       param->getName());
    return ParamSpecifier::Default;
  }

  if (isa<InOutTypeRepr>(nestedRepr)) {
    return ParamSpecifier::InOut;
  } else if (isa<SharedTypeRepr>(nestedRepr)) {
    return ParamSpecifier::Shared;
  } else if (isa<OwnedTypeRepr>(nestedRepr)) {
    return ParamSpecifier::Owned;
  }

  return ParamSpecifier::Default;
}

static Type validateParameterType(ParamDecl *decl) {
  auto *dc = decl->getDeclContext();
  auto resolution = TypeResolution::forInterface(dc);

  TypeResolutionOptions options(None);
  if (isa<AbstractClosureExpr>(dc)) {
    options = TypeResolutionOptions(TypeResolverContext::ClosureExpr);
    options |= TypeResolutionFlags::AllowUnspecifiedTypes;
    options |= TypeResolutionFlags::AllowUnboundGenerics;
  } else if (isa<AbstractFunctionDecl>(dc)) {
    options = TypeResolutionOptions(TypeResolverContext::AbstractFunctionDecl);
  } else if (isa<SubscriptDecl>(dc)) {
    options = TypeResolutionOptions(TypeResolverContext::SubscriptDecl);
  } else {
    assert(isa<EnumElementDecl>(dc));
    options = TypeResolutionOptions(TypeResolverContext::EnumElementDecl);
  }

  // If the element is a variadic parameter, resolve the parameter type as if
  // it were in non-parameter position, since we want functions to be
  // @escaping in this case.
  options.setContext(decl->isVariadic() ?
                       TypeResolverContext::VariadicFunctionInput :
                       TypeResolverContext::FunctionInput);
  options |= TypeResolutionFlags::Direct;

  auto TL = TypeLoc(decl->getTypeRepr());

  auto &ctx = dc->getASTContext();
  if (TypeChecker::validateType(ctx, TL, resolution, options)) {
    decl->setInvalid();
    return ErrorType::get(ctx);
  }

  Type Ty = TL.getType();
  if (decl->isVariadic()) {
    Ty = TypeChecker::getArraySliceType(decl->getStartLoc(), Ty);
    if (Ty.isNull()) {
      decl->setInvalid();
      return ErrorType::get(ctx);
    }

    // Disallow variadic parameters in enum elements.
    if (options.getBaseContext() == TypeResolverContext::EnumElementDecl) {
      decl->diagnose(diag::enum_element_ellipsis);
      decl->setInvalid();
      return ErrorType::get(ctx);
    }

    return Ty;
  }
  return TL.getType();
}

llvm::Expected<Type>
InterfaceTypeRequest::evaluate(Evaluator &eval, ValueDecl *D) const {
  auto &Context = D->getASTContext();

  TypeChecker::checkForForbiddenPrefix(Context, D->getBaseName());

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
  case DeclKind::Module:
  case DeclKind::OpaqueType:
  case DeclKind::GenericTypeParam:
    llvm_unreachable("should not get here");
    return Type();

  case DeclKind::AssociatedType: {
    auto assocType = cast<AssociatedTypeDecl>(D);
    auto interfaceTy = assocType->getDeclaredInterfaceType();
    return MetatypeType::get(interfaceTy, Context);
  }

  case DeclKind::TypeAlias: {
    auto typeAlias = cast<TypeAliasDecl>(D);

    auto genericSig = typeAlias->getGenericSignature();
    SubstitutionMap subs;
    if (genericSig)
      subs = genericSig->getIdentitySubstitutionMap();

    Type parent;
    auto parentDC = typeAlias->getDeclContext();
    if (parentDC->isTypeContext())
      parent = parentDC->getSelfInterfaceType();
    auto sugaredType = TypeAliasType::get(typeAlias, parent, subs,
                                          typeAlias->getUnderlyingType());
    return MetatypeType::get(sugaredType, Context);
  }

  case DeclKind::Enum:
  case DeclKind::Struct:
  case DeclKind::Class:
  case DeclKind::Protocol: {
    auto nominal = cast<NominalTypeDecl>(D);
    Type declaredInterfaceTy = nominal->getDeclaredInterfaceType();
    return MetatypeType::get(declaredInterfaceTy, Context);
  }

  case DeclKind::Param: {
    auto *PD = cast<ParamDecl>(D);
    if (PD->isSelfParameter()) {
      auto *AFD = cast<AbstractFunctionDecl>(PD->getDeclContext());
      auto selfParam = computeSelfParam(AFD,
                                        /*isInitializingCtor*/true,
                                        /*wantDynamicSelf*/true);
      return selfParam.getPlainType();
    }

    if (auto *accessor = dyn_cast<AccessorDecl>(PD->getDeclContext())) {
      auto *storage = accessor->getStorage();
      auto *originalParam = getOriginalParamFromAccessor(
        storage, accessor, PD);
      if (originalParam == nullptr) {
        return storage->getValueInterfaceType();
      }

      if (originalParam != PD) {
        return originalParam->getInterfaceType();
      }
    }

    if (!PD->getTypeRepr())
      return Type();

    return validateParameterType(PD);
  }

  case DeclKind::Var: {
    auto *VD = cast<VarDecl>(D);
    auto *namingPattern = VD->getNamingPattern();
    if (!namingPattern) {
      return ErrorType::get(Context);
    }

    Type interfaceType = namingPattern->getType();
    if (interfaceType->hasArchetype())
      interfaceType = interfaceType->mapTypeOutOfContext();

    // In SIL mode, VarDecls are written as having reference storage types.
    if (!interfaceType->is<ReferenceStorageType>()) {
      if (auto *attr = VD->getAttrs().getAttribute<ReferenceOwnershipAttr>())
        interfaceType =
            TypeChecker::checkReferenceOwnershipAttr(VD, interfaceType, attr);
    }

    return interfaceType;
  }

  case DeclKind::Func:
  case DeclKind::Accessor:
  case DeclKind::Constructor:
  case DeclKind::Destructor: {
    auto *AFD = cast<AbstractFunctionDecl>(D);

    auto sig = AFD->getGenericSignature();
    bool hasSelf = AFD->hasImplicitSelfDecl();

    AnyFunctionType::ExtInfo info;

    // Result
    Type resultTy;
    if (auto fn = dyn_cast<FuncDecl>(D)) {
      resultTy = fn->getResultInterfaceType();
    } else if (auto ctor = dyn_cast<ConstructorDecl>(D)) {
      resultTy = ctor->getResultInterfaceType();
    } else {
      assert(isa<DestructorDecl>(D));
      resultTy = TupleType::getEmpty(AFD->getASTContext());
    }

    // (Args...) -> Result
    Type funcTy;

    {
      SmallVector<AnyFunctionType::Param, 4> argTy;
      AFD->getParameters()->getParams(argTy);

      // 'throws' only applies to the innermost function.
      info = info.withThrows(AFD->hasThrows());
      // Defer bodies must not escape.
      if (auto fd = dyn_cast<FuncDecl>(D))
        info = info.withNoEscape(fd->isDeferBody());

      if (sig && !hasSelf) {
        funcTy = GenericFunctionType::get(sig, argTy, resultTy, info);
      } else {
        funcTy = FunctionType::get(argTy, resultTy, info);
      }
    }

    // (Self) -> (Args...) -> Result
    if (hasSelf) {
      // Substitute in our own 'self' parameter.
      auto selfParam = computeSelfParam(AFD);
      if (sig)
        funcTy = GenericFunctionType::get(sig, {selfParam}, funcTy);
      else
        funcTy = FunctionType::get({selfParam}, funcTy);
    }

    return funcTy;
  }

  case DeclKind::Subscript: {
    auto *SD = cast<SubscriptDecl>(D);

    auto elementTy = SD->getElementInterfaceType();

    SmallVector<AnyFunctionType::Param, 2> argTy;
    SD->getIndices()->getParams(argTy);

    Type funcTy;
    if (auto sig = SD->getGenericSignature())
      funcTy = GenericFunctionType::get(sig, argTy, elementTy);
    else
      funcTy = FunctionType::get(argTy, elementTy);

    return funcTy;
  }

  case DeclKind::EnumElement: {
    auto *EED = cast<EnumElementDecl>(D);

    auto *ED = EED->getParentEnum();

    // The type of the enum element is either (Self.Type) -> Self
    // or (Self.Type) -> (Args...) -> Self.
    auto resultTy = ED->getDeclaredInterfaceType();

    AnyFunctionType::Param selfTy(MetatypeType::get(resultTy, Context));

    if (auto *PL = EED->getParameterList()) {
      SmallVector<AnyFunctionType::Param, 4> argTy;
      PL->getParams(argTy);

      resultTy = FunctionType::get(argTy, resultTy);
    }

    if (auto genericSig = ED->getGenericSignature())
      resultTy = GenericFunctionType::get(genericSig, {selfTy}, resultTy);
    else
      resultTy = FunctionType::get({selfTy}, resultTy);

    return resultTy;
  }
  }
}

llvm::Expected<NamedPattern *>
NamingPatternRequest::evaluate(Evaluator &evaluator, VarDecl *VD) const {
  auto &Context = VD->getASTContext();
  auto *PBD = VD->getParentPatternBinding();
  // FIXME: In order for this request to properly express its dependencies,
  // all of the places that allow variable bindings need to also use pattern
  // binding decls. Otherwise, we'll have to go digging around in case
  // statements and patterns to find named patterns.
  if (PBD) {
    // FIXME: For now, this works because PatternBindingEntryRequest fills in
    // the naming pattern as a side effect in this case, and TypeCheckStmt
    // and TypeCheckPattern handle the others. But that's all really gross.
    unsigned i = PBD->getPatternEntryIndexForVarDecl(VD);
    (void)evaluateOrDefault(evaluator,
                            PatternBindingEntryRequest{PBD, i},
                            nullptr);
    if (PBD->isInvalid()) {
      VD->getParentPattern()->setType(ErrorType::get(Context));
      setBoundVarsTypeError(VD->getParentPattern(), Context);
      return nullptr;
    }
  } else if (!VD->getParentPatternStmt() && !VD->getParentVarDecl()) {
    // No parent?  That's an error.
    return nullptr;
  }

  // Go digging for the named pattern that declares this variable.
  auto *namingPattern = VD->NamingPattern;
  if (!namingPattern) {
    auto *canVD = VD->getCanonicalVarDecl();
    namingPattern = canVD->NamingPattern;

    // HACK: If no other diagnostic applies, emit a generic diagnostic about
    // a variable being unbound. We can't do better than this at the
    // moment because TypeCheckPattern does not reliably invalidate parts of
    // the pattern AST on failure.
    //
    // Once that's through, this will only fire during circular validation.
    if (!namingPattern) {
      if (VD->hasInterfaceType() &&
          !VD->isInvalid() && !VD->getParentPattern()->isImplicit()) {
        VD->diagnose(diag::variable_bound_by_no_pattern, VD->getName());
      }

      VD->getParentPattern()->setType(ErrorType::get(Context));
      setBoundVarsTypeError(VD->getParentPattern(), Context);
      return nullptr;
    }
  }

  if (!namingPattern->hasType()) {
    namingPattern->setType(ErrorType::get(Context));
    setBoundVarsTypeError(namingPattern, Context);
  }

  return namingPattern;
}

llvm::Expected<DeclRange>
EmittedMembersRequest::evaluate(Evaluator &evaluator,
                                ClassDecl *CD) const {
  if (!CD->getParentSourceFile())
    return CD->getMembers();

  auto &Context = CD->getASTContext();

  // We need to add implicit initializers because they
  // affect vtable layout.
  TypeChecker::addImplicitConstructors(CD);

  auto forceConformance = [&](ProtocolDecl *protocol) {
    auto ref = TypeChecker::conformsToProtocol(
        CD->getDeclaredInterfaceType(), protocol, CD,
        ConformanceCheckFlags::SkipConditionalRequirements, SourceLoc());

    if (ref.isInvalid()) {
      return;
    }

    auto conformance = ref.getConcrete();
    if (conformance->getDeclContext() == CD &&
        conformance->getState() == ProtocolConformanceState::Incomplete) {
      TypeChecker::checkConformance(conformance->getRootNormalConformance());
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
static Optional<std::string> buildDefaultInitializerString(DeclContext *dc,
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
#define CHECK_LITERAL_PROTOCOL(Kind, String)                                   \
  if (auto proto = TypeChecker::getProtocol(                                   \
          type->getASTContext(), SourceLoc(), KnownProtocolKind::Kind)) {      \
    if (TypeChecker::conformsToProtocol(type, proto, dc,                       \
                                        ConformanceCheckFlags::InExpression))  \
      return std::string(String);                                              \
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
            dc, cast<ParenPattern>(pattern)->getSubPattern())) {
      return "(" + *sub + ")";
    }

    return None;
  }

  case PatternKind::Tuple: {
    std::string result = "(";
    bool first = true;
    for (auto elt : cast<TuplePattern>(pattern)->getElements()) {
      if (auto sub = buildDefaultInitializerString(dc, elt.getPattern())) {
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
        dc, cast<TypedPattern>(pattern)->getSubPattern());

  case PatternKind::Var:
    return buildDefaultInitializerString(
        dc, cast<VarPattern>(pattern)->getSubPattern());
  }

  llvm_unreachable("Unhandled PatternKind in switch.");
}

/// Diagnose a class that does not have any initializers.
static void diagnoseClassWithoutInitializers(ClassDecl *classDecl) {
  ASTContext &C = classDecl->getASTContext();
  C.Diags.diagnose(classDecl, diag::class_without_init,
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
    auto *decodableProto = C.getProtocol(KnownProtocolKind::Decodable);
    auto superclassType = superclassDecl->getDeclaredInterfaceType();
    auto ref = TypeChecker::conformsToProtocol(
        superclassType, decodableProto, superclassDecl,
        ConformanceCheckOptions(), SourceLoc());
    if (ref) {
      // super conforms to Decodable, so we've failed to inherit init(from:).
      // Let's suggest overriding it here.
      //
      // We're going to diagnose on the concrete init(from:) decl if it exists
      // and isn't implicit; otherwise, on the subclass itself.
      ValueDecl *diagDest = classDecl;
      auto initFrom = DeclName(C, DeclBaseName::createConstructor(), C.Id_from);
      auto result =
          TypeChecker::lookupMember(superclassDecl, superclassType, initFrom,
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
      auto ref = TypeChecker::conformsToProtocol(
          superclassType, encodableProto, superclassDecl,
          ConformanceCheckOptions(), SourceLoc());
      if (ref) {
        // We only want to produce this version of the diagnostic if the
        // subclass doesn't directly implement encode(to:).
        // The direct lookup here won't see an encode(to:) if it is inherited
        // from the superclass.
        auto encodeTo = DeclName(C, C.Id_encode, C.Id_to);
        if (classDecl->lookupDirect(encodeTo).empty())
          diagName = diag::codable_suggest_overriding_init_here;
      }

      C.Diags.diagnose(diagDest, diagName);
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
   
    for (auto idx : range(pbd->getNumPatternEntries())) {
      if (pbd->isInitialized(idx)) continue;

      auto *pattern = pbd->getPattern(idx);
      SmallVector<VarDecl *, 4> vars;
      pattern->collectVariables(vars);
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
        diag.emplace(C.Diags.diagnose(varLoc, diag::note_no_in_class_init_1,
                                      vars[0]->getName()));
        break;
      case 2:
        diag.emplace(C.Diags.diagnose(varLoc, diag::note_no_in_class_init_2,
                                      vars[0]->getName(), vars[1]->getName()));
        break;
      case 3:
        diag.emplace(C.Diags.diagnose(varLoc, diag::note_no_in_class_init_3plus,
                                      vars[0]->getName(), vars[1]->getName(),
                                      vars[2]->getName(), false));
        break;
      default:
        diag.emplace(C.Diags.diagnose(varLoc, diag::note_no_in_class_init_3plus,
                                      vars[0]->getName(), vars[1]->getName(),
                                      vars[2]->getName(), true));
        break;
      }

      if (auto defaultValueSuggestion =
              buildDefaultInitializerString(classDecl, pattern))
        diag->fixItInsertAfter(pattern->getEndLoc(),
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

  diagnoseClassWithoutInitializers(classDecl);
}
