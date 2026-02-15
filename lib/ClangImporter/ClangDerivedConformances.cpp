//===--- ClangDerivedConformances.cpp -------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "ClangDerivedConformances.h"
#include "ImporterImpl.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/LayoutConstraint.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Basic/Assertions.h"
#include "swift/ClangImporter/ClangImporterRequests.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/CXXInheritance.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/Type.h"
#include "clang/Sema/DelayedDiagnostic.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Overload.h"

using namespace swift;
using namespace swift::importer;

/// Known C++ stdlib types, for which we can assume conformance to the standard
/// (e.g., std::map has template parameters for key and value types, and has
/// members like key_type, size_type, and operator[]).
enum class CxxStdType {
  uncategorized,
  optional,
  set,
  unordered_set,
  multiset,
  pair,
  map,
  unordered_map,
  multimap,
  vector,
  span,
};

static CxxStdType identifyCxxStdTypeByName(StringRef name) {
#define CaseStd(name) Case(#name, CxxStdType::name)
  return llvm::StringSwitch<CxxStdType>(name)
      .CaseStd(optional)
      .CaseStd(set)
      .CaseStd(unordered_set)
      .CaseStd(multiset)
      .CaseStd(pair)
      .CaseStd(map)
      .CaseStd(unordered_map)
      .CaseStd(multimap)
      .CaseStd(vector)
      .CaseStd(span)
      .Default(CxxStdType::uncategorized);
#undef CxxStdCase
}

static const clang::TypeDecl *
lookupCxxTypeMember(clang::Sema &Sema, const clang::CXXRecordDecl *Rec,
                    StringRef name, bool mustBeComplete = false) {
  auto R = clang::LookupResult(Sema, &Sema.PP.getIdentifierTable().get(name),
                               clang::SourceLocation(),
                               clang::Sema::LookupMemberName);
  R.suppressDiagnostics();
  Sema.LookupQualifiedName(R, const_cast<clang::CXXRecordDecl *>(Rec));

  if (!R.isSingleResult())
    return nullptr; // Result was absent or ambiguous

  auto it = R.begin();
  if (it->getAccess() != clang::AS_public)
    return nullptr; // Cannot be access from derived class

  auto *td = dyn_cast<clang::TypeDecl>(it.getDecl());
  if (!td)
    return nullptr; // Was not a clang::TypeDecl

  if (mustBeComplete &&
      !Sema.isCompleteType({}, td->getASTContext().getTypeDeclType(td)))
    return nullptr;

  return td;
}

/// Alternative to `NominalTypeDecl::lookupDirect`.
/// This function does not attempt to load extensions of the nominal decl.
static TinyPtrVector<ValueDecl *>
lookupDirectWithoutExtensions(NominalTypeDecl *decl, Identifier id) {
  ASTContext &ctx = decl->getASTContext();
  auto *importer = static_cast<ClangImporter *>(ctx.getClangModuleLoader());

  TinyPtrVector<ValueDecl *> result;

  if (id.isOperator()) {
    auto underlyingId = getOperatorName(ctx, id);
    TinyPtrVector<ValueDecl *> underlyingFuncs = evaluateOrDefault(
        ctx.evaluator, ClangRecordMemberLookup({decl, underlyingId}), {});
    for (auto it : underlyingFuncs) {
      if (auto synthesizedFunc =
              importer->getCXXSynthesizedOperatorFunc(cast<FuncDecl>(it)))
        result.push_back(synthesizedFunc);
    }
  } else {
    // See if there is a Clang decl with the given name.
    result = evaluateOrDefault(ctx.evaluator,
                               ClangRecordMemberLookup({decl, id}), {});
  }

  // Check if there are any synthesized Swift members that match the name.
  for (auto member : decl->getCurrentMembersWithoutLoading()) {
    if (auto namedMember = dyn_cast<ValueDecl>(member)) {
      if (namedMember->hasName() && !namedMember->getName().isSpecial() &&
          namedMember->getName().getBaseIdentifier().is(id.str()) &&
          // Make sure we don't add duplicate entries, as that would wrongly
          // imply that lookup is ambiguous.
          !llvm::is_contained(result, namedMember)) {
        result.push_back(namedMember);
      }
    }
  }
  return result;
}

template <typename Decl>
static Decl *lookupDirectSingleWithoutExtensions(NominalTypeDecl *decl,
                                                 Identifier id) {
  auto results = lookupDirectWithoutExtensions(decl, id);
  if (results.size() != 1)
    return nullptr;
  return dyn_cast<Decl>(results.front());
}

static ValueDecl *lookupOperator(NominalTypeDecl *decl, Identifier id,
                                 function_ref<bool(ValueDecl *)> isValid) {
  // First look for operator declared as a member.
  auto memberResults = lookupDirectWithoutExtensions(decl, id);
  for (const auto &member : memberResults) {
    if (isValid(member))
      return member;
  }

  // If no member operator was found, look for out-of-class definitions in the
  // same module.
  auto module = decl->getModuleContextForNameLookup();
  SmallVector<ValueDecl *> nonMemberResults;
  module->lookupValue(id, NLKind::UnqualifiedLookup, nonMemberResults);
  for (const auto &nonMember : nonMemberResults) {
    if (isValid(nonMember))
      return nonMember;
  }

  return nullptr;
}

static ValueDecl *getEqualEqualOperator(NominalTypeDecl *decl) {
  auto isValid = [&](ValueDecl *equalEqualOp) -> bool {
    auto equalEqual = dyn_cast<FuncDecl>(equalEqualOp);
    if (!equalEqual)
      return false;
    auto params = equalEqual->getParameters();
    if (params->size() != 2)
      return false;
    auto lhs = params->get(0);
    auto rhs = params->get(1);
    if (lhs->isInOut() || rhs->isInOut())
      return false;
    auto lhsTy = lhs->getTypeInContext();
    auto rhsTy = rhs->getTypeInContext();
    if (!lhsTy || !rhsTy)
      return false;
    auto lhsNominal = lhsTy->getAnyNominal();
    auto rhsNominal = rhsTy->getAnyNominal();
    if (lhsNominal != rhsNominal || lhsNominal != decl)
      return false;
    return true;
  };

  return lookupOperator(decl, decl->getASTContext().Id_EqualsOperator, isValid);
}

static FuncDecl *getMinusOperator(NominalTypeDecl *decl) {
  auto binaryIntegerProto =
      decl->getASTContext().getProtocol(KnownProtocolKind::BinaryInteger);

  auto isValid = [&](ValueDecl *minusOp) -> bool {
    auto minus = dyn_cast<FuncDecl>(minusOp);
    if (!minus)
      return false;
    auto params = minus->getParameters();
    if (params->size() != 2)
      return false;
    auto lhs = params->get(0);
    auto rhs = params->get(1);
    if (lhs->isInOut() || rhs->isInOut())
      return false;
    auto lhsTy = lhs->getTypeInContext();
    auto rhsTy = rhs->getTypeInContext();
    if (!lhsTy || !rhsTy)
      return false;
    auto lhsNominal = lhsTy->getAnyNominal();
    auto rhsNominal = rhsTy->getAnyNominal();
    if (lhsNominal != rhsNominal || lhsNominal != decl)
      return false;
    auto returnTy = minus->getResultInterfaceType();
    if (!checkConformance(returnTy, binaryIntegerProto))
      return false;
    return true;
  };

  ValueDecl *result =
      lookupOperator(decl, decl->getASTContext().getIdentifier("-"), isValid);
  return dyn_cast_or_null<FuncDecl>(result);
}

static FuncDecl *getPlusEqualOperator(NominalTypeDecl *decl, Type distanceTy) {
  auto isValid = [&](ValueDecl *plusEqualOp) -> bool {
    auto plusEqual = dyn_cast<FuncDecl>(plusEqualOp);
    if (!plusEqual)
      return false;
    auto params = plusEqual->getParameters();
    if (params->size() != 2)
      return false;
    auto lhs = params->get(0);
    auto rhs = params->get(1);
    if (rhs->isInOut())
      return false;
    auto lhsTy = lhs->getTypeInContext();
    auto rhsTy = rhs->getTypeInContext();
    if (!lhsTy || !rhsTy)
      return false;
    if (rhsTy->getCanonicalType() != distanceTy->getCanonicalType())
      return false;
    auto lhsNominal = lhsTy->getAnyNominal();
    if (lhsNominal != decl)
      return false;
    auto returnTy = plusEqual->getResultInterfaceType();
    if (!returnTy->isVoid())
      return false;
    return true;
  };

  ValueDecl *result =
      lookupOperator(decl, decl->getASTContext().getIdentifier("+="), isValid);
  return dyn_cast_or_null<FuncDecl>(result);
}

static FuncDecl *getNonMutatingDereferenceOperator(NominalTypeDecl *decl) {
  auto isValid = [](ValueDecl *starOperator) -> bool {
    auto starOp = dyn_cast<FuncDecl>(starOperator);
    if (!starOp || starOp->isMutating())
      return false;
    auto params = starOp->getParameters();
    if (params->size() != 0)
      return false;
    auto returnTy = starOp->getResultInterfaceType();
    if (!returnTy->getAnyPointerElementType())
      return false;
    return true;
  };

  ValueDecl *result = lookupOperator(
      decl, decl->getASTContext().getIdentifier("__operatorStar"), isValid);
  return dyn_cast_or_null<FuncDecl>(result);
}

static clang::FunctionDecl *
instantiateTemplatedOperator(ClangImporter::Implementation &impl,
                             const clang::CXXRecordDecl *classDecl,
                             clang::BinaryOperatorKind operatorKind) {

  clang::ASTContext &clangCtx = impl.getClangASTContext();
  clang::Sema &clangSema = impl.getClangSema();

  clang::UnresolvedSet<1> ops;
  auto qualType = clang::QualType(classDecl->getTypeForDecl(), 0);
  auto arg = clang::CXXThisExpr::Create(clangCtx, clang::SourceLocation(),
                                        qualType, false);
  arg->setType(clang::QualType(classDecl->getTypeForDecl(), 0));

  clang::OverloadedOperatorKind opKind =
      clang::BinaryOperator::getOverloadedOperator(operatorKind);
  clang::OverloadCandidateSet candidateSet(
      classDecl->getLocation(), clang::OverloadCandidateSet::CSK_Operator,
      clang::OverloadCandidateSet::OperatorRewriteInfo(opKind,
                                              clang::SourceLocation(), false));
  std::array<clang::Expr *, 2> args{arg, arg};
  clangSema.LookupOverloadedBinOp(candidateSet, opKind, ops, args, true);

  clang::OverloadCandidateSet::iterator best;
  switch (candidateSet.BestViableFunction(clangSema, clang::SourceLocation(),
                                          best)) {
  case clang::OR_Success: {
    if (auto clangCallee = best->Function) {
      // Declarations inside of a C++ namespace are added into two lookup
      // tables: one for the __ObjC module, one for the actual owning Clang
      // module of the decl. This is a hack that is meant to address the case
      // when a namespace spans across multiple Clang modules. Mimic that
      // behavior for the operator that we just instantiated.
      auto lookupTable1 = impl.findLookupTable(classDecl);
      addEntryToLookupTable(*lookupTable1, clangCallee, impl.getNameImporter());
      auto owningModule = importer::getClangOwningModule(classDecl, clangCtx);
      auto lookupTable2 = impl.findLookupTable(owningModule);
      if (lookupTable1 != lookupTable2)
        addEntryToLookupTable(*lookupTable2, clangCallee, impl.getNameImporter());
      return clangCallee;
    }
    break;
  }
  case clang::OR_No_Viable_Function:
  case clang::OR_Ambiguous:
  case clang::OR_Deleted:
    break;
  }

  return nullptr;
}

/// Warning: This function emits an error and stops compilation if the
/// underlying operator function is unavailable in Swift for the current target
/// (see `clang::Sema::DiagnoseAvailabilityOfDecl`).
static bool synthesizeCXXOperator(ClangImporter::Implementation &impl,
                                  const clang::CXXRecordDecl *classDecl,
                                  clang::BinaryOperatorKind operatorKind,
                                  clang::QualType lhsTy, clang::QualType rhsTy,
                                  clang::QualType returnTy) {
  auto &clangCtx = impl.getClangASTContext();
  auto &clangSema = impl.getClangSema();

  clang::OverloadedOperatorKind opKind =
      clang::BinaryOperator::getOverloadedOperator(operatorKind);
  const char *opSpelling = clang::getOperatorSpelling(opKind);

  auto declName = clang::DeclarationName(&clangCtx.Idents.get(opSpelling));

  // Determine the Clang decl context where the new operator function will be
  // created. We use the translation unit as the decl context of the new
  // operator, otherwise, the operator might get imported as a static member
  // function of a different type (e.g. an operator declared inside of a C++
  // namespace would get imported as a member function of a Swift enum), which
  // would make the operator un-discoverable to Swift name lookup.
  auto declContext =
      const_cast<clang::CXXRecordDecl *>(classDecl)->getDeclContext();
  while (!declContext->isTranslationUnit()) {
    declContext = declContext->getParent();
  }

  auto equalEqualTy = clangCtx.getFunctionType(
      returnTy, {lhsTy, rhsTy}, clang::FunctionProtoType::ExtProtoInfo());

  // Create a `bool operator==(T, T)` function.
  auto equalEqualDecl = clang::FunctionDecl::Create(
      clangCtx, declContext, clang::SourceLocation(), clang::SourceLocation(),
      declName, equalEqualTy, clangCtx.getTrivialTypeSourceInfo(returnTy),
      clang::StorageClass::SC_Static);
  equalEqualDecl->setImplicit();
  equalEqualDecl->setImplicitlyInline();
  // If this is a static member function of a class, it needs to be public.
  equalEqualDecl->setAccess(clang::AccessSpecifier::AS_public);

  // Create the parameters of the function. They are not referenced from source
  // code, so they don't need to have a name.
  auto lhsParamId = nullptr;
  auto lhsTyInfo = clangCtx.getTrivialTypeSourceInfo(lhsTy);
  auto lhsParamDecl = clang::ParmVarDecl::Create(
      clangCtx, equalEqualDecl, clang::SourceLocation(),
      clang::SourceLocation(), lhsParamId, lhsTy, lhsTyInfo,
      clang::StorageClass::SC_None, /*DefArg*/ nullptr);
  auto lhsParamRefExpr = new (clangCtx) clang::DeclRefExpr(
      clangCtx, lhsParamDecl, false, lhsTy, clang::ExprValueKind::VK_LValue,
      clang::SourceLocation());

  auto rhsParamId = nullptr;
  auto rhsTyInfo = clangCtx.getTrivialTypeSourceInfo(rhsTy);
  auto rhsParamDecl = clang::ParmVarDecl::Create(
      clangCtx, equalEqualDecl, clang::SourceLocation(),
      clang::SourceLocation(), rhsParamId, rhsTy, rhsTyInfo,
      clang::StorageClass::SC_None, nullptr);
  auto rhsParamRefExpr = new (clangCtx) clang::DeclRefExpr(
      clangCtx, rhsParamDecl, false, rhsTy, clang::ExprValueKind::VK_LValue,
      clang::SourceLocation());

  equalEqualDecl->setParams({lhsParamDecl, rhsParamDecl});

  // Lookup the `operator==` function that will be called under the hood.
  clang::UnresolvedSet<16> operators;
  clang::sema::DelayedDiagnosticPool diagPool{
      impl.getClangSema().DelayedDiagnostics.getCurrentPool()};
  auto diagState = impl.getClangSema().DelayedDiagnostics.push(diagPool);
  // Note: calling `CreateOverloadedBinOp` emits an error if the looked up
  // function is unavailable for the current target.
  auto underlyingCallResult = clangSema.CreateOverloadedBinOp(
      clang::SourceLocation(), operatorKind, operators, lhsParamRefExpr,
      rhsParamRefExpr);
  impl.getClangSema().DelayedDiagnostics.popWithoutEmitting(diagState);

  if (!diagPool.empty())
    return false;
  if (!underlyingCallResult.isUsable())
    return false;
  auto underlyingCall = underlyingCallResult.get();

  auto equalEqualBody = clang::ReturnStmt::Create(
      clangCtx, clang::SourceLocation(), underlyingCall, nullptr);
  equalEqualDecl->setBody(equalEqualBody);

  impl.synthesizedAndAlwaysVisibleDecls.insert(equalEqualDecl);
  auto lookupTable1 = impl.findLookupTable(classDecl);
  addEntryToLookupTable(*lookupTable1, equalEqualDecl, impl.getNameImporter());
  auto owningModule = importer::getClangOwningModule(classDecl, clangCtx);
  auto lookupTable2 = impl.findLookupTable(owningModule);
  if (lookupTable1 != lookupTable2)
    addEntryToLookupTable(*lookupTable2, equalEqualDecl,
                          impl.getNameImporter());
  return true;
}

bool swift::hasIteratorCategory(const clang::CXXRecordDecl *clangDecl) {
  clang::IdentifierInfo *name =
      &clangDecl->getASTContext().Idents.get("iterator_category");
  auto members = clangDecl->lookup(name);
  if (members.empty())
    return false;
  // NOTE: If this is a templated typedef, Clang might have instantiated
  // several equivalent typedef decls, so members.isSingleResult() may
  // return false here. But if they aren't equivalent, Clang should have
  // already complained about this. Let's assume that they are equivalent.
  // (see filterNonConflictingPreviousTypedefDecls in clang/Sema/SemaDecl.cpp)
  return isa<clang::TypeDecl>(members.front());
}

ValueDecl *
swift::importer::getImportedMemberOperator(const DeclBaseName &name,
                                           NominalTypeDecl *selfType,
                                           std::optional<Type> parameterType) {
  assert(name.isOperator());
  // Handle ==, -, and += operators, that are required operators for C++
  // iterator types to conform to the corresponding Cxx iterator protocols.
  // These operators can be instantiated and synthesized by clang importer below,
  // and thus require additional lookup logic when they're being deserialized.
  if (name.getIdentifier() == selfType->getASTContext().Id_EqualsOperator) {
    return getEqualEqualOperator(selfType);
  }
  if (name.getIdentifier() == selfType->getASTContext().getIdentifier("-")) {
    return getMinusOperator(selfType);
  }
  if (name.getIdentifier() == selfType->getASTContext().getIdentifier("+=") &&
      parameterType) {
    return getPlusEqualOperator(selfType, *parameterType);
  }
  return nullptr;
}

static void
conformToCxxIteratorIfNeeded(ClangImporter::Implementation &impl,
                             NominalTypeDecl *decl,
                             const clang::CXXRecordDecl *clangDecl) {
  PrettyStackTraceDecl trace("trying to conform to UnsafeCxxInputIterator", decl);
  ASTContext &ctx = decl->getASTContext();
  clang::ASTContext &clangCtx = clangDecl->getASTContext();
  clang::Sema &clangSema = impl.getClangSema();

  if (!ctx.getProtocol(KnownProtocolKind::UnsafeCxxInputIterator))
    return;

  // We consider a type to be an input iterator if it defines an
  // `iterator_category` that inherits from `std::input_iterator_tag`, e.g.
  // `using iterator_category = std::input_iterator_tag`.
  //
  // FIXME: The second hasIteratorCategory() is more conservative than it should
  // be  because it doesn't consider things like inheritance, but checking this
  // here maintains existing behavior and ensures consistency across
  // ClangImporter, where clang::Sema isn't always readily available.
  const auto *iteratorCategory =
      lookupCxxTypeMember(clangSema, clangDecl, "iterator_category");
  if (!iteratorCategory || !hasIteratorCategory(clangDecl))
    return;

  auto unwrapUnderlyingTypeDecl =
      [](const clang::TypeDecl *typeDecl) -> const clang::CXXRecordDecl * {
    const clang::CXXRecordDecl *underlyingDecl = nullptr;
    if (auto typedefDecl = dyn_cast<clang::TypedefNameDecl>(typeDecl)) {
      auto type = typedefDecl->getUnderlyingType();
      underlyingDecl = type->getAsCXXRecordDecl();
    } else {
      underlyingDecl = dyn_cast<clang::CXXRecordDecl>(typeDecl);
    }
    if (underlyingDecl) {
      underlyingDecl = underlyingDecl->getDefinition();
    }
    return underlyingDecl;
  };

  // If `iterator_category` is a typedef or a using-decl, retrieve the
  // underlying struct decl.
  auto underlyingCategoryDecl = unwrapUnderlyingTypeDecl(iteratorCategory);
  if (!underlyingCategoryDecl)
    return;

  auto isIteratorTagDecl = [&](const clang::CXXRecordDecl *base,
                               StringRef tag) {
    return base->isInStdNamespace() && base->getIdentifier() &&
           base->getName() == tag;
  };
  auto isInputIteratorDecl = [&](const clang::CXXRecordDecl *base) {
    return isIteratorTagDecl(base, "input_iterator_tag");
  };
  auto isRandomAccessIteratorDecl = [&](const clang::CXXRecordDecl *base) {
    return isIteratorTagDecl(base, "random_access_iterator_tag");
  };
  auto isContiguousIteratorDecl = [&](const clang::CXXRecordDecl *base) {
    return isIteratorTagDecl(base, "contiguous_iterator_tag"); // C++20
  };

  // Traverse all transitive bases of `underlyingDecl` to check if
  // it inherits from `std::input_iterator_tag`.
  bool isInputIterator = isInputIteratorDecl(underlyingCategoryDecl);
  bool isRandomAccessIterator =
      isRandomAccessIteratorDecl(underlyingCategoryDecl);
  underlyingCategoryDecl->forallBases([&](const clang::CXXRecordDecl *base) {
    if (isInputIteratorDecl(base)) {
      isInputIterator = true;
    }
    if (isRandomAccessIteratorDecl(base)) {
      isRandomAccessIterator = true;
      isInputIterator = true;
      return false;
    }
    return true;
  });

  if (!isInputIterator)
    return;

  bool isContiguousIterator = false;
  // In C++20, `std::contiguous_iterator_tag` is specified as a type called
  // `iterator_concept`. It is not possible to detect a contiguous iterator
  // based on its `iterator_category`. The type might not have an
  // `iterator_concept` defined.
  if (const auto *iteratorConcept =
          lookupCxxTypeMember(clangSema, clangDecl, "iterator_concept")) {
    if (auto underlyingConceptDecl =
            unwrapUnderlyingTypeDecl(iteratorConcept)) {
      isContiguousIterator = isContiguousIteratorDecl(underlyingConceptDecl);
      if (!isContiguousIterator)
        underlyingConceptDecl->forallBases(
            [&](const clang::CXXRecordDecl *base) {
              if (isContiguousIteratorDecl(base)) {
                isContiguousIterator = true;
                return false;
              }
              return true;
            });
    }
  }

  auto *pointee = impl.lookupAndImportPointee(decl);
  if (!pointee || pointee->isGetterMutating() ||
      pointee->getTypeInContext()->hasError())
    return;

  // Check if `var pointee: Pointee` is settable. This is required for the
  // conformance to UnsafeCxxMutableInputIterator but is not necessary for
  // UnsafeCxxInputIterator.
  bool pointeeSettable = pointee->isSettable(nullptr);
  Type pointeeTy = pointee->getTypeInContext();

  auto *successor = impl.lookupAndImportSuccessor(decl);
  if (!successor || successor->isMutating())
    return;
  auto successorTy = successor->getResultInterfaceType();
  if (!successorTy || successorTy->getAnyNominal() != decl)
    return;

  // Check if present: `func ==`
  auto equalEqual = getEqualEqualOperator(decl);
  if (!equalEqual) {
    // If this class is inherited, `operator==` might be defined for a base
    // class. If this is a templated class, `operator==` might be templated as
    // well. Try to instantiate it.
    clang::FunctionDecl *instantiated = instantiateTemplatedOperator(
        impl, clangDecl, clang::BinaryOperatorKind::BO_EQ);
    if (instantiated && !impl.isUnavailableInSwift(instantiated)) {
      // If `operator==` was instantiated successfully, try to find `func ==`
      // again.
      equalEqual = getEqualEqualOperator(decl);
      if (!equalEqual) {
        // If `func ==` still can't be found, it might be defined for a base
        // class of the current class.
        auto paramTy = clangCtx.getRecordType(clangDecl);
        synthesizeCXXOperator(impl, clangDecl, clang::BinaryOperatorKind::BO_EQ,
                              paramTy, paramTy, clangCtx.BoolTy);
        equalEqual = getEqualEqualOperator(decl);
      }
    }
  }
  if (!equalEqual)
    return;

  // Look for __operatorStar(), which must be non-mutating and return a
  // reference. This makes sure we use the const operator* overload.
  auto operatorStar = getNonMutatingDereferenceOperator(decl);
  Type dereferenceResultTy = pointeeTy;
  if (operatorStar) {
    assert(!operatorStar->isMutating() &&
           "this __operatorStar can't be mutating");
    auto operatorStarReturnTy = operatorStar->getResultInterfaceType();
    assert(operatorStarReturnTy &&
           "__operatorStar doesn't have a return type?");

    if (operatorStarReturnTy &&
        operatorStarReturnTy->getAnyPointerElementType() &&
        (operatorStarReturnTy->getAnyPointerElementType()->getCanonicalType() ==
         pointeeTy->getCanonicalType()))
      dereferenceResultTy = operatorStar->getResultInterfaceType();
  }
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("Pointee"), pointeeTy);
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("DereferenceResult"),
                               dereferenceResultTy);

  if (pointeeSettable)
    impl.addSynthesizedProtocolAttrs(
        decl, {KnownProtocolKind::UnsafeCxxMutableInputIterator});
  else
    impl.addSynthesizedProtocolAttrs(
        decl, {KnownProtocolKind::UnsafeCxxInputIterator});

  if (!isRandomAccessIterator ||
      !ctx.getProtocol(KnownProtocolKind::UnsafeCxxRandomAccessIterator))
    return;

  // Try to conform to UnsafeCxxRandomAccessIterator if possible.

  // Check if present: `func -`
  auto minus = getMinusOperator(decl);
  if (!minus) {
    clang::FunctionDecl *instantiated = instantiateTemplatedOperator(
        impl, clangDecl, clang::BinaryOperatorKind::BO_Sub);
    if (instantiated && !impl.isUnavailableInSwift(instantiated)) {
      minus = getMinusOperator(decl);
      if (!minus) {
        clang::QualType returnTy = instantiated->getReturnType();
        auto paramTy = clangCtx.getRecordType(clangDecl);
        synthesizeCXXOperator(impl, clangDecl,
                              clang::BinaryOperatorKind::BO_Sub, paramTy,
                              paramTy, returnTy);
        minus = getMinusOperator(decl);
      }
    }
  }
  if (!minus)
    return;
  auto distanceTy = minus->getResultInterfaceType();
  // distanceTy conforms to BinaryInteger, this is ensured by getMinusOperator.

  auto plusEqual = getPlusEqualOperator(decl, distanceTy);
  if (!plusEqual) {
    clang::FunctionDecl *instantiated = instantiateTemplatedOperator(
        impl, clangDecl, clang::BinaryOperatorKind::BO_AddAssign);
    if (instantiated && !impl.isUnavailableInSwift(instantiated)) {
      plusEqual = getPlusEqualOperator(decl, distanceTy);
      if (!plusEqual) {
        clang::QualType returnTy = instantiated->getReturnType();
        auto clangMinus = cast<clang::FunctionDecl>(minus->getClangDecl());
        auto lhsTy = clangCtx.getRecordType(clangDecl);
        auto rhsTy = clangMinus->getReturnType();
        synthesizeCXXOperator(impl, clangDecl,
                              clang::BinaryOperatorKind::BO_AddAssign, lhsTy,
                              rhsTy, returnTy);
        plusEqual = getPlusEqualOperator(decl, distanceTy);
      }
    }
  }
  if (!plusEqual)
    return;

  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("Distance"), distanceTy);
  if (pointeeSettable)
    impl.addSynthesizedProtocolAttrs(
        decl, {KnownProtocolKind::UnsafeCxxMutableRandomAccessIterator});
  else
    impl.addSynthesizedProtocolAttrs(
        decl, {KnownProtocolKind::UnsafeCxxRandomAccessIterator});

  if (isContiguousIterator) {
    if (pointeeSettable)
      impl.addSynthesizedProtocolAttrs(
          decl, {KnownProtocolKind::UnsafeCxxMutableContiguousIterator});
    else
      impl.addSynthesizedProtocolAttrs(
          decl, {KnownProtocolKind::UnsafeCxxContiguousIterator});
  }
}

static void
conformToCxxConvertibleToBoolIfNeeded(ClangImporter::Implementation &impl,
                                      swift::NominalTypeDecl *decl,
                                      const clang::CXXRecordDecl *clangDecl) {
  PrettyStackTraceDecl trace("trying to conform to CxxConvertibleToBool", decl);
  ASTContext &ctx = decl->getASTContext();

  auto conversionId = ctx.getIdentifier("__convertToBool");
  auto conversions = lookupDirectWithoutExtensions(decl, conversionId);

  // Find a non-mutating overload of `__convertToBool`.
  FuncDecl *conversion = nullptr;
  for (auto c : conversions) {
    auto candidate = dyn_cast<FuncDecl>(c);
    if (!candidate || candidate->isMutating())
      continue;
    if (conversion)
      // Overload ambiguity?
      return;
    conversion = candidate;
  }
  if (!conversion)
    return;
  auto conversionTy = conversion->getResultInterfaceType();
  if (!conversionTy->isBool())
    return;

  impl.addSynthesizedProtocolAttrs(decl,
                                   {KnownProtocolKind::CxxConvertibleToBool});
}

static void conformToCxxOptional(ClangImporter::Implementation &impl,
                                 NominalTypeDecl *decl,
                                 const clang::CXXRecordDecl *clangDecl) {
  PrettyStackTraceDecl trace("conforming to CxxOptional", decl);
  ASTContext &ctx = decl->getASTContext();
  clang::ASTContext &clangCtx = impl.getClangASTContext();
  clang::Sema &clangSema = impl.getClangSema();

  auto *value_type = lookupCxxTypeMember(clangSema, clangDecl, "value_type",
                                         /*mustBeComplete=*/true);
  if (!value_type)
    return;

  auto *Wrapped = dyn_cast_or_null<TypeAliasDecl>(
      impl.importDecl(value_type, impl.CurrentVersion));
  if (!Wrapped)
    return;

  if (!impl.lookupAndImportPointee(decl))
    return;

  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("Wrapped"),
                               Wrapped->getUnderlyingType());
  impl.addSynthesizedProtocolAttrs(decl, {KnownProtocolKind::CxxOptional});

  // `std::optional` has a C++ constructor that takes the wrapped value as a
  // parameter. Unfortunately this constructor has templated parameter type, so
  // it isn't directly usable from Swift. Let's explicitly instantiate a
  // constructor with the wrapped value type, and then import it into Swift.

  auto valueType = clangCtx.getTypeDeclType(value_type);

  auto constRefValueType =
      clangCtx.getLValueReferenceType(valueType.withConst());
  // Create a fake variable with type of the wrapped value.
  auto fakeValueVarDecl = clang::VarDecl::Create(
      clangCtx, /*DC*/ clangCtx.getTranslationUnitDecl(),
      clang::SourceLocation(), clang::SourceLocation(), /*Id*/ nullptr,
      constRefValueType, clangCtx.getTrivialTypeSourceInfo(constRefValueType),
      clang::StorageClass::SC_None);
  auto fakeValueRefExpr = new (clangCtx) clang::DeclRefExpr(
      clangCtx, fakeValueVarDecl, false,
      constRefValueType.getNonReferenceType(), clang::ExprValueKind::VK_LValue,
      clang::SourceLocation());

  auto clangDeclTyInfo = clangCtx.getTrivialTypeSourceInfo(
      clang::QualType(clangDecl->getTypeForDecl(), 0));
  SmallVector<clang::Expr *, 1> constructExprArgs = {fakeValueRefExpr};

  // Instantiate the templated constructor that would accept this fake variable.
  clang::Sema::SFINAETrap trap(clangSema);
  auto constructExprResult = clangSema.BuildCXXTypeConstructExpr(
      clangDeclTyInfo, clangDecl->getLocation(), constructExprArgs,
      clangDecl->getLocation(), /*ListInitialization*/ false);
  if (!constructExprResult.isUsable() || trap.hasErrorOccurred())
    return;

  auto castExpr = dyn_cast_or_null<clang::CastExpr>(constructExprResult.get());
  if (!castExpr)
    return;

  // The temporary bind expression will only be present for some non-trivial C++
  // types.
  auto bindTempExpr =
      dyn_cast_or_null<clang::CXXBindTemporaryExpr>(castExpr->getSubExpr());

  auto constructExpr = dyn_cast_or_null<clang::CXXConstructExpr>(
      bindTempExpr ? bindTempExpr->getSubExpr() : castExpr->getSubExpr());
  if (!constructExpr)
    return;

  auto constructorDecl = constructExpr->getConstructor();

  auto importedConstructor =
      impl.importDecl(constructorDecl, impl.CurrentVersion);
  if (!importedConstructor)
    return;
  decl->addMember(importedConstructor);
}

static void conformToCxxBorrowingSequenceIfNedded(
    ClangImporter::Implementation &impl, NominalTypeDecl *decl,
    const clang::CXXRecordDecl *clangDecl,
    const ProtocolConformance *rawIteratorConformance) {
  PrettyStackTraceDecl trace("trying to conform to CxxBorrowingSequence", decl);
  ASTContext &ctx = decl->getASTContext();

  ProtocolDecl *cxxIteratorProto =
      ctx.getProtocol(KnownProtocolKind::UnsafeCxxInputIterator);
  ProtocolDecl *cxxBorrowingSequenceProto =
      ctx.getProtocol(KnownProtocolKind::CxxBorrowingSequence);
  if (!cxxIteratorProto || !cxxBorrowingSequenceProto)
    return;

  // Take the default definition of `BorrowingIterator` from
  // CxxBorrowingSequence protocol. This type is currently
  // `CxxBorrowingIterator<Self>`.
  auto borrowingIteratorDecl = cxxBorrowingSequenceProto->getAssociatedType(
      ctx.getIdentifier("_BorrowingIterator"));
  if (!borrowingIteratorDecl)
    return;
  auto borrowingIteratorNominal =
      borrowingIteratorDecl->getDefaultDefinitionType()->getAnyNominal();

  // Substitute generic `Self` parameter.
  auto declSelfTy = decl->getDeclaredInterfaceType();
  auto borrowingIteratorTy =
      BoundGenericType::get(borrowingIteratorNominal, Type(), {declSelfTy});

  auto dereferenceResultDecl = cxxIteratorProto->getAssociatedType(
      ctx.getIdentifier("DereferenceResult"));
  if (!dereferenceResultDecl)
    return;

  auto dereferenceResultTy =
      rawIteratorConformance->getTypeWitness(dereferenceResultDecl);

  if (dereferenceResultTy && dereferenceResultTy->getAnyPointerElementType()) {
    // Only conform to CxxBorrowingSequence if `__operatorStar` returns
    // `UnsafePointer<Pointee>`. Otherwise, we can't create a span for pointee.
    impl.addSynthesizedTypealias(decl, ctx.getIdentifier("_BorrowingIterator"),
                                 borrowingIteratorTy);
    impl.addSynthesizedProtocolAttrs(decl,
                                     {KnownProtocolKind::CxxBorrowingSequence});
  }
}

static void
conformToCxxSequenceIfNeeded(ClangImporter::Implementation &impl,
                             NominalTypeDecl *decl,
                             const clang::CXXRecordDecl *clangDecl) {
  PrettyStackTraceDecl trace("trying to conform to CxxSequence", decl);
  ASTContext &ctx = decl->getASTContext();

  ProtocolDecl *cxxIteratorProto =
      ctx.getProtocol(KnownProtocolKind::UnsafeCxxInputIterator);
  ProtocolDecl *cxxSequenceProto =
      ctx.getProtocol(KnownProtocolKind::CxxSequence);
  // If the Cxx module is missing, or does not include one of the necessary
  // protocols, bail.
  if (!cxxIteratorProto || !cxxSequenceProto)
    return;

  // Check if present: `func __beginUnsafe() -> RawIterator`
  auto beginId = ctx.getIdentifier("__beginUnsafe");
  auto begin = lookupDirectSingleWithoutExtensions<FuncDecl>(decl, beginId);
  if (!begin)
    return;
  auto rawIteratorTy = begin->getResultInterfaceType();

  // Check if present: `func __endUnsafe() -> RawIterator`
  auto endId = ctx.getIdentifier("__endUnsafe");
  auto end = lookupDirectSingleWithoutExtensions<FuncDecl>(decl, endId);
  if (!end)
    return;

  // Check if `begin()` and `end()` are non-mutating.
  if (begin->isMutating() || end->isMutating())
    return;

  // Check if `__beginUnsafe` and `__endUnsafe` have the same return type.
  auto endTy = end->getResultInterfaceType();
  if (!endTy || endTy->getCanonicalType() != rawIteratorTy->getCanonicalType())
    return;

  // Check if RawIterator conforms to UnsafeCxxInputIterator.
  auto rawIteratorConformanceRef =
      checkConformance(rawIteratorTy, cxxIteratorProto);
  if (!rawIteratorConformanceRef)
    return;
  auto rawIteratorConformance = rawIteratorConformanceRef.getConcrete();
  auto pointeeDecl =
      cxxIteratorProto->getAssociatedType(ctx.getIdentifier("Pointee"));
  assert(pointeeDecl &&
         "UnsafeCxxInputIterator must have a Pointee associated type");
  auto pointeeTy = rawIteratorConformance->getTypeWitness(pointeeDecl);
  assert(pointeeTy && "valid conformance must have a Pointee witness");

  impl.addSynthesizedTypealias(decl, ctx.Id_Element, pointeeTy);
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("_Element"), pointeeTy);
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("RawIterator"),
                               rawIteratorTy);

  conformToCxxBorrowingSequenceIfNedded(impl, decl, clangDecl,
                                        rawIteratorConformance);

  // CxxSequence conformance.
  // Take the default definition of `Iterator` from CxxSequence protocol. This
  // type is currently `CxxIterator<Self>`.
  auto iteratorDecl = cxxSequenceProto->getAssociatedType(ctx.Id_Iterator);
  auto iteratorTy = iteratorDecl->getDefaultDefinitionType();
  // Substitute generic `Self` parameter.
  auto declSelfTy = decl->getDeclaredInterfaceType();
  auto cxxSequenceSelfTy = cxxSequenceProto->getSelfInterfaceType();
  iteratorTy = iteratorTy.subst(
      [&](SubstitutableType *dependentType) {
        if (dependentType->isEqual(cxxSequenceSelfTy))
          return declSelfTy;
        return Type(dependentType);
      },
      LookUpConformanceInModule());
  impl.addSynthesizedTypealias(decl, ctx.Id_Iterator, iteratorTy);

  // Not conforming the type to CxxSequence protocol here:
  // The current implementation of CxxSequence triggers extra copies of the C++
  // collection when creating a CxxIterator instance. It needs a more efficient
  // implementation, which is not possible with the existing Swift features.
  // impl.addSynthesizedProtocolAttrs(decl, {KnownProtocolKind::CxxSequence});

  // Try to conform to CxxRandomAccessCollection if possible.

  auto tryToConformToRandomAccessCollection = [&]() -> bool {
    auto cxxRAIteratorProto =
        ctx.getProtocol(KnownProtocolKind::UnsafeCxxRandomAccessIterator);
    if (!cxxRAIteratorProto ||
        !ctx.getProtocol(KnownProtocolKind::CxxRandomAccessCollection))
      return false;

    // Check if RawIterator conforms to UnsafeCxxRandomAccessIterator.
    if (!checkConformance(rawIteratorTy, cxxRAIteratorProto))
      return false;

    // CxxRandomAccessCollection always uses Int as an Index.
    auto indexTy = ctx.getIntType();

    auto sliceTy = ctx.getSliceType();
    sliceTy = sliceTy.subst(
        [&](SubstitutableType *dependentType) {
          if (dependentType->isEqual(cxxSequenceSelfTy))
            return declSelfTy;
          return Type(dependentType);
        },
        LookUpConformanceInModule());

    auto indicesTy = ctx.getRangeType();
    indicesTy = indicesTy.subst(
        [&](SubstitutableType *dependentType) {
          if (dependentType->isEqual(cxxSequenceSelfTy))
            return indexTy;
          return Type(dependentType);
        },
        LookUpConformanceInModule());

    impl.addSynthesizedTypealias(decl, ctx.getIdentifier("Index"), indexTy);
    impl.addSynthesizedTypealias(decl, ctx.getIdentifier("Indices"), indicesTy);
    impl.addSynthesizedTypealias(decl, ctx.getIdentifier("SubSequence"),
                                 sliceTy);

    auto tryToConformToMutatingRACollection = [&]() -> bool {
      auto rawMutableIteratorProto = ctx.getProtocol(
          KnownProtocolKind::UnsafeCxxMutableRandomAccessIterator);
      if (!rawMutableIteratorProto)
        return false;

      // Check if present: `func __beginMutatingUnsafe() -> RawMutableIterator`
      auto beginMutatingId = ctx.getIdentifier("__beginMutatingUnsafe");
      auto beginMutating =
          lookupDirectSingleWithoutExtensions<FuncDecl>(decl, beginMutatingId);
      if (!beginMutating)
        return false;
      auto rawMutableIteratorTy = beginMutating->getResultInterfaceType();

      // Check if present: `func __endMutatingUnsafe() -> RawMutableIterator`
      auto endMutatingId = ctx.getIdentifier("__endMutatingUnsafe");
      auto endMutating =
          lookupDirectSingleWithoutExtensions<FuncDecl>(decl, endMutatingId);
      if (!endMutating)
        return false;

      if (!checkConformance(rawMutableIteratorTy, rawMutableIteratorProto))
        return false;

      impl.addSynthesizedTypealias(
          decl, ctx.getIdentifier("RawMutableIterator"), rawMutableIteratorTy);
      impl.addSynthesizedProtocolAttrs(
          decl, {KnownProtocolKind::CxxMutableRandomAccessCollection});
      return true;
    };

    bool conformedToMutableRAC = tryToConformToMutatingRACollection();

    if (!conformedToMutableRAC)
      impl.addSynthesizedProtocolAttrs(
          decl, {KnownProtocolKind::CxxRandomAccessCollection});

    return true;
  };

  bool conformedToRAC = tryToConformToRandomAccessCollection();

  // If the collection does not support random access, let's still allow the
  // developer to explicitly convert a C++ sequence to a Swift Array (making a
  // copy of the sequence's elements) by conforming the type to
  // CxxCollectionConvertible. This enables an overload of Array.init declared
  // in the Cxx module.
  ProtocolDecl *cxxConvertibleProto =
      ctx.getProtocol(KnownProtocolKind::CxxConvertibleToCollection);
  if (!conformedToRAC && cxxConvertibleProto) {
    impl.addSynthesizedProtocolAttrs(
        decl, {KnownProtocolKind::CxxConvertibleToCollection});
  }
}

bool swift::isUnsafeStdMethod(const clang::CXXMethodDecl *methodDecl) {
  auto *parentDecl =
      dyn_cast<clang::CXXRecordDecl>(methodDecl->getDeclContext());
  if (!parentDecl || !parentDecl->isInStdNamespace() ||
      !parentDecl->getIdentifier())
    return false;

  if (methodDecl->getIdentifier() && methodDecl->getName() == "insert") {
    // Types for which the insert method is considered unsafe,
    // due to potential iterator invalidation.
    return llvm::StringSwitch<bool>(parentDecl->getName())
        .Cases({"set", "unordered_set", "multiset"}, true)
        .Cases({"map", "unordered_map", "multimap"}, true)
        .Default(false);
  }
  return false;
}

static void conformToCxxSet(ClangImporter::Implementation &impl,
                            NominalTypeDecl *decl,
                            const clang::CXXRecordDecl *clangDecl,
                            bool isUniqueSet) {
  PrettyStackTraceDecl trace("conforming to CxxSet", decl);
  ASTContext &ctx = decl->getASTContext();
  auto &clangCtx = impl.getClangASTContext();
  auto &clangSema = impl.getClangSema();

  // Look up the type members we need from Clang
  //
  // N.B. we don't actually need const_iterator for multiset, but it should be
  // there. If it's not there for any reason, we should probably bail out.

  auto *size_type = lookupCxxTypeMember(clangSema, clangDecl, "size_type",
                                        /*mustBeComplete=*/true);
  auto *value_type = lookupCxxTypeMember(clangSema, clangDecl, "value_type",
                                         /*mustBeComplete=*/true);
  auto *iterator = lookupCxxTypeMember(clangSema, clangDecl, "iterator",
                                       /*mustBeComplete=*/true);
  auto *const_iterator =
      lookupCxxTypeMember(clangSema, clangDecl, "const_iterator",
                          /*mustBeComplete=*/true);
  if (!size_type || !value_type || !iterator || !const_iterator)
    return;

  const clang::CXXMethodDecl *insert = nullptr;
  {
    // CxxSet requires the InsertionResult associated type, which is the return
    // type of std::set (and co.)'s insert function. But there is no equivalent
    // typedef in C++ we can use directly, so we need get it by converting the
    // return type of the insert function.
    //
    // A wrinkle here is that std::set actually has multiple insert overloads.
    // There are two overloads that could work for us:
    //
    //    insert_return_type insert(const value_type &value);
    //    insert_return_type insert(value_type &&value);
    //
    // where insert_return_type is std::pair<iterator, bool> for std::set and
    // std::unordered_set and just iterator for std::multiset.
    //
    // Look for the version with the single const-lref value_type parameter,
    // since that's the one that maps to Swift's semantics most closely.
    //
    // NOTE: this code is a bit lengthy, and could be abstracted into a helper
    // function, but at this time of writing, the only two times we need to look
    // for a member is for std::set and std::map's insert methods. We keep this
    // lookup routine inlined for now until the interface for a reasonably
    // encapsulated helper function emerges.

    auto R = clang::LookupResult(
        clangSema, &clangSema.PP.getIdentifierTable().get("insert"),
        clang::SourceLocation(), clang::Sema::LookupMemberName);
    R.suppressDiagnostics();
    clangSema.LookupQualifiedName(
        R, const_cast<clang::CXXRecordDecl *>(clangDecl));
    switch (R.getResultKind()) {
    case clang::LookupResultKind::Found:
    case clang::LookupResultKind::FoundOverloaded:
      break;
    default:
      return;
    }

    for (auto *nd : R) {
      if (auto *insertOverload = dyn_cast<clang::CXXMethodDecl>(nd)) {
        if (insertOverload->param_size() != 1)
          continue;
        auto *paramTy = (*insertOverload->param_begin())
                            ->getType()
                            ->getAs<clang::ReferenceType>();
        if (!paramTy)
          continue;
        if (paramTy->getPointeeType()->getCanonicalTypeUnqualified() !=
            clangCtx.getTypeDeclType(value_type)->getCanonicalTypeUnqualified())
          continue;
        if (!paramTy->getPointeeType().isConstQualified())
          continue;
        insert = insertOverload; // Found the insert() we're looking for
        break;
      }
    }
  }
  if (!insert)
    return;

  // We've looked up everything we need from Clang for the conformance.
  // Now, use ClangImporter to convert import those types to Swift.
  //
  // NOTE: we're actually importing the typedefs and function members here,
  // but not *adding* them as members to the Swift StructDecl---that is done
  // elsewhere (and could be lazy too, though not at this time of writing).
  // We are just using these imported Swift members for their type fields,
  // because importDecl() needs fewer arguments than importTypeIgnoreIUO().

  auto *Size = dyn_cast_or_null<TypeAliasDecl>(
      impl.importDecl(size_type, impl.CurrentVersion));
  auto *Value = dyn_cast_or_null<TypeAliasDecl>(
      impl.importDecl(value_type, impl.CurrentVersion));
  auto *RawMutableIterator = dyn_cast_or_null<TypeAliasDecl>(
      impl.importDecl(iterator, impl.CurrentVersion));
  auto *RawIterator = dyn_cast_or_null<TypeAliasDecl>(
      impl.importDecl(const_iterator, impl.CurrentVersion));
  auto *Insert =
      dyn_cast_or_null<FuncDecl>(impl.importDecl(insert, impl.CurrentVersion));
  if (!Size || !Value || !RawMutableIterator || !RawIterator || !Insert)
    return;

  // We have our Swift types, synthesize type aliases and conformances

  impl.addSynthesizedTypealias(decl, ctx.Id_Element,
                               Value->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.Id_ArrayLiteralElement,
                               Value->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("Size"),
                               Size->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("RawIterator"),
                               RawIterator->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("RawMutableIterator"),
                               RawMutableIterator->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("InsertionResult"),
                               Insert->getResultInterfaceType());

  impl.addSynthesizedProtocolAttrs(decl, {KnownProtocolKind::CxxSet});
  if (isUniqueSet)
    impl.addSynthesizedProtocolAttrs(decl, {KnownProtocolKind::CxxUniqueSet});
}

static void conformToCxxPair(ClangImporter::Implementation &impl,
                             NominalTypeDecl *decl,
                             const clang::CXXRecordDecl *clangDecl) {
  PrettyStackTraceDecl trace("conforming to CxxPair", decl);
  ASTContext &ctx = decl->getASTContext();
  clang::Sema &clangSema = impl.getClangSema();

  auto *first_type = lookupCxxTypeMember(clangSema, clangDecl, "first_type",
                                         /*mustBeComplete=*/true);
  auto *second_type = lookupCxxTypeMember(clangSema, clangDecl, "second_type",
                                          /*mustBeComplete=*/true);
  if (!first_type || !second_type)
    return;

  auto *First = dyn_cast_or_null<TypeAliasDecl>(
      impl.importDecl(first_type, impl.CurrentVersion));
  auto *Second = dyn_cast_or_null<TypeAliasDecl>(
      impl.importDecl(second_type, impl.CurrentVersion));
  if (!First || !Second)
    return;

  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("First"),
                               First->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("Second"),
                               Second->getUnderlyingType());
  impl.addSynthesizedProtocolAttrs(decl, {KnownProtocolKind::CxxPair});
}

static void conformToCxxDictionary(ClangImporter::Implementation &impl,
                                   NominalTypeDecl *decl,
                                   const clang::CXXRecordDecl *clangDecl) {
  PrettyStackTraceDecl trace("conforming to CxxDictionary", decl);
  ASTContext &ctx = decl->getASTContext();
  clang::ASTContext &clangCtx = impl.getClangASTContext();
  clang::Sema &clangSema = impl.getClangSema();

  auto *key_type = lookupCxxTypeMember(clangSema, clangDecl, "key_type", true);
  auto *mapped_type =
      lookupCxxTypeMember(clangSema, clangDecl, "mapped_type", true);
  auto *value_type =
      lookupCxxTypeMember(clangSema, clangDecl, "value_type", true);
  auto *size_type =
      lookupCxxTypeMember(clangSema, clangDecl, "size_type", true);
  auto *iterator = lookupCxxTypeMember(clangSema, clangDecl, "iterator", true);
  auto *const_iterator =
      lookupCxxTypeMember(clangSema, clangDecl, "const_iterator", true);
  if (!key_type || !mapped_type || !value_type || !size_type || !iterator ||
      !const_iterator)
    return;

  const clang::CXXMethodDecl *insert = nullptr;
  {
    // CxxDictionary requires the InsertionResult associated type, which is the
    // return type of std::map (and co.)'s insert function. But there is no
    // equivalent typedef in C++ we can use directly, so we need get it by
    // converting the return type of this overload of the insert function:
    //
    //    insert_return_type insert(const value_type &value);
    //
    // See also: extended monologuing in conformToCxxSet().
    auto R = clang::LookupResult(
        clangSema, &clangSema.PP.getIdentifierTable().get("insert"),
        clang::SourceLocation(), clang::Sema::LookupMemberName);
    R.suppressDiagnostics();
    clangSema.LookupQualifiedName(
        R, const_cast<clang::CXXRecordDecl *>(clangDecl));
    switch (R.getResultKind()) {
    case clang::LookupResultKind::Found:
    case clang::LookupResultKind::FoundOverloaded:
      break;
    default:
      return;
    }

    for (auto *nd : R) {
      if (auto *insertOverload = dyn_cast<clang::CXXMethodDecl>(nd)) {
        if (insertOverload->param_size() != 1)
          continue;
        auto *paramTy = (*insertOverload->param_begin())
                            ->getType()
                            ->getAs<clang::ReferenceType>();
        if (!paramTy)
          continue;
        if (paramTy->getPointeeType()->getCanonicalTypeUnqualified() !=
            clangCtx.getTypeDeclType(value_type)->getCanonicalTypeUnqualified())
          continue;
        if (!paramTy->getPointeeType().isConstQualified())
          continue;
        insert = insertOverload; // Found the insert() we're looking for
        break;
      }
    }
  }
  if (!insert)
    return;

  auto *Size = dyn_cast_or_null<TypeAliasDecl>(
      impl.importDecl(size_type, impl.CurrentVersion));
  auto *Key = dyn_cast_or_null<TypeAliasDecl>(
      impl.importDecl(key_type, impl.CurrentVersion));
  auto *Value = dyn_cast_or_null<TypeAliasDecl>(
      impl.importDecl(mapped_type, impl.CurrentVersion));
  auto *Element = dyn_cast_or_null<TypeAliasDecl>(
      impl.importDecl(value_type, impl.CurrentVersion));
  auto *RawIterator = dyn_cast_or_null<TypeAliasDecl>(
      impl.importDecl(const_iterator, impl.CurrentVersion));
  auto *RawMutableIterator = dyn_cast_or_null<TypeAliasDecl>(
      impl.importDecl(iterator, impl.CurrentVersion));
  if (!Size || !Key || !Value || !Element || !RawIterator ||
      !RawMutableIterator)
    return;

  auto *Insert =
      dyn_cast_or_null<FuncDecl>(impl.importDecl(insert, impl.CurrentVersion));
  if (!Insert)
    return;

  impl.addSynthesizedTypealias(decl, ctx.Id_Key, Key->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.Id_Value, Value->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.Id_Element,
                               Element->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("RawIterator"),
                               RawIterator->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("RawMutableIterator"),
                               RawMutableIterator->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("Size"),
                               Size->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("InsertionResult"),
                               Insert->getResultInterfaceType());
  impl.addSynthesizedProtocolAttrs(decl, {KnownProtocolKind::CxxDictionary});

  // Make the original subscript that returns a non-optional value unavailable.
  // CxxDictionary adds another subscript that returns an optional value,
  // similarly to Swift.Dictionary.
  //
  // NOTE: this relies on the SubscriptDecl member being imported eagerly.
  for (auto member : decl->getCurrentMembersWithoutLoading()) {
    if (auto subscript = dyn_cast<SubscriptDecl>(member)) {
      impl.markUnavailable(subscript,
                           "use subscript with optional return value");
    }
  }
}

static void conformToCxxVector(ClangImporter::Implementation &impl,
                               NominalTypeDecl *decl,
                               const clang::CXXRecordDecl *clangDecl) {
  PrettyStackTraceDecl trace("conforming to CxxVector", decl);
  ASTContext &ctx = decl->getASTContext();
  clang::Sema &clangSema = impl.getClangSema();

  auto *value_type = lookupCxxTypeMember(clangSema, clangDecl, "value_type",
                                         /*mustBeComplete=*/true);
  auto *size_type = lookupCxxTypeMember(clangSema, clangDecl, "size_type",
                                        /*mustBeComplete=*/true);
  auto *const_iterator =
      lookupCxxTypeMember(clangSema, clangDecl, "const_iterator",
                          /*mustBeComplete=*/true);

  auto *Element = dyn_cast_or_null<TypeAliasDecl>(
      impl.importDecl(value_type, impl.CurrentVersion));
  auto *Size = dyn_cast_or_null<TypeAliasDecl>(
      impl.importDecl(size_type, impl.CurrentVersion));
  auto *RawIterator = dyn_cast_or_null<TypeAliasDecl>(
      impl.importDecl(const_iterator, impl.CurrentVersion));
  if (!Element || !Size || !RawIterator)
    return;

  impl.addSynthesizedTypealias(decl, ctx.Id_Element,
                               Element->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.Id_ArrayLiteralElement,
                               Element->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("Size"),
                               Size->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("RawIterator"),
                               RawIterator->getUnderlyingType());
  impl.addSynthesizedProtocolAttrs(decl, {KnownProtocolKind::CxxVector});
}

static void conformToCxxSpan(ClangImporter::Implementation &impl,
                             NominalTypeDecl *decl,
                             const clang::CXXRecordDecl *clangDecl) {
  PrettyStackTraceDecl trace("conforming to CxxSpan", decl);
  ASTContext &ctx = decl->getASTContext();
  clang::ASTContext &clangCtx = impl.getClangASTContext();
  clang::Sema &clangSema = impl.getClangSema();

  auto *element_type = lookupCxxTypeMember(clangSema, clangDecl, "element_type",
                                           /*mustBeComplete=*/true);
  auto *size_type = lookupCxxTypeMember(clangSema, clangDecl, "size_type",
                                        /*mustBeComplete=*/true);
  auto *pointer = lookupCxxTypeMember(clangSema, clangDecl, "pointer",
                                      /*mustBeComplete=*/true);
  if (!element_type || !size_type || !pointer)
    return;

  auto pointerType = clangCtx.getTypeDeclType(pointer);
  auto sizeType = clangCtx.getTypeDeclType(size_type);

  auto *Element = dyn_cast_or_null<TypeAliasDecl>(
      impl.importDecl(element_type, impl.CurrentVersion));
  auto *Size = dyn_cast_or_null<TypeAliasDecl>(
      impl.importDecl(size_type, impl.CurrentVersion));
  if (!Element || !Size)
    return;

  impl.addSynthesizedTypealias(decl, ctx.Id_Element,
                               Element->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("Size"),
                               Size->getUnderlyingType());

  if (pointerType->getPointeeType().isConstQualified())
    impl.addSynthesizedProtocolAttrs(decl, {KnownProtocolKind::CxxSpan});
  else
    impl.addSynthesizedProtocolAttrs(decl, {KnownProtocolKind::CxxMutableSpan});

  // create fake variable for pointer (constructor arg 1)
  auto fakePointerVarDecl = clang::VarDecl::Create(
      clangCtx, /*DC*/ clangCtx.getTranslationUnitDecl(),
      clang::SourceLocation(), clang::SourceLocation(), /*Id*/ nullptr,
      pointerType, clangCtx.getTrivialTypeSourceInfo(pointerType),
      clang::StorageClass::SC_None);

  auto fakePointer = new (clangCtx) clang::DeclRefExpr(
      clangCtx, fakePointerVarDecl, false, pointerType,
      clang::ExprValueKind::VK_LValue, clang::SourceLocation());

  // create fake variable for count (constructor arg 2)
  auto fakeCountVarDecl = clang::VarDecl::Create(
      clangCtx, /*DC*/ clangCtx.getTranslationUnitDecl(),
      clang::SourceLocation(), clang::SourceLocation(), /*Id*/ nullptr,
      sizeType, clangCtx.getTrivialTypeSourceInfo(sizeType),
      clang::StorageClass::SC_None);

  auto fakeCount = new (clangCtx) clang::DeclRefExpr(
      clangCtx, fakeCountVarDecl, false, sizeType,
      clang::ExprValueKind::VK_LValue, clang::SourceLocation());

  // Use clangSema.BuildCxxTypeConstructExpr to create a CXXTypeConstructExpr,
  // passing constPointer and count
  SmallVector<clang::Expr *, 2> constructExprArgs = {fakePointer, fakeCount};

  auto clangDeclTyInfo = clangCtx.getTrivialTypeSourceInfo(
      clang::QualType(clangDecl->getTypeForDecl(), 0));

  // Instantiate the templated constructor that would accept this fake variable.
  auto constructExprResult = clangSema.BuildCXXTypeConstructExpr(
      clangDeclTyInfo, clangDecl->getLocation(), constructExprArgs,
      clangDecl->getLocation(), /*ListInitialization*/ false);
  if (!constructExprResult.isUsable())
    return;

  auto constructExpr =
      dyn_cast_or_null<clang::CXXConstructExpr>(constructExprResult.get());
  if (!constructExpr)
    return;

  auto constructorDecl = constructExpr->getConstructor();
  auto importedConstructor =
      impl.importDecl(constructorDecl, impl.CurrentVersion);
  if (!importedConstructor)
    return;

  auto attr = AvailableAttr::createUniversallyDeprecated(
      importedConstructor->getASTContext(), "use 'init(_:)' instead.", "");
  importedConstructor->addAttribute(attr);

  decl->addMember(importedConstructor);
}

void swift::deriveAutomaticCxxConformances(
    ClangImporter::Implementation &Impl, NominalTypeDecl *result,
    const clang::CXXRecordDecl *clangDecl) {

  ASSERT(result && clangDecl && "this should not be called with nullptrs");

  // Skip synthesizing conformances if the associated Clang node is from
  // a module that doesn't require cplusplus, to prevent us from accidentally
  // pulling in Cxx/CxxStdlib modules when a client is importing a C library.
  //
  // We will still attempt to synthesize to account for scenarios where the
  // module specification is missing altogether.
  if (auto *clangModule = importer::getClangOwningModule(
          result->getClangNode(), Impl.getClangASTContext());
      clangModule && !requiresCPlusPlus(clangModule))
    return;

  // Automatic conformances: these may be applied to any type that fits the
  // requirements.
  conformToCxxIteratorIfNeeded(Impl, result, clangDecl);
  conformToCxxSequenceIfNeeded(Impl, result, clangDecl);
  conformToCxxConvertibleToBoolIfNeeded(Impl, result, clangDecl);

  // CxxStdlib conformances: these should only apply to known C++ stdlib types,
  // which we determine by name and membership in the std namespace.
  if (!clangDecl->getIdentifier() || !clangDecl->isInStdNamespace())
    return;

  auto ty = identifyCxxStdTypeByName(clangDecl->getName());
  switch (ty) {
  case CxxStdType::uncategorized:
    return;
  case CxxStdType::optional:
    conformToCxxOptional(Impl, result, clangDecl);
    return;
  case CxxStdType::set:
  case CxxStdType::unordered_set:
    conformToCxxSet(Impl, result, clangDecl, /*isUniqueSet=*/true);
    return;
  case CxxStdType::multiset:
    conformToCxxSet(Impl, result, clangDecl, /*isUniqueSet=*/false);
    return;
  case CxxStdType::pair:
    conformToCxxPair(Impl, result, clangDecl);
    return;
  case CxxStdType::map:
  case CxxStdType::unordered_map:
  case CxxStdType::multimap:
    conformToCxxDictionary(Impl, result, clangDecl);
    return;
  case CxxStdType::vector:
    conformToCxxVector(Impl, result, clangDecl);
    return;
  case CxxStdType::span:
    conformToCxxSpan(Impl, result, clangDecl);
    return;
  }
}
