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
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Basic/Assertions.h"
#include "swift/ClangImporter/ClangImporterRequests.h"
#include "clang/Sema/DelayedDiagnostic.h"
#include "clang/Sema/Overload.h"

using namespace swift;
using namespace swift::importer;

/// Alternative to `NominalTypeDecl::lookupDirect`.
/// This function does not attempt to load extensions of the nominal decl.
static TinyPtrVector<ValueDecl *>
lookupDirectWithoutExtensions(NominalTypeDecl *decl, Identifier id) {
  ASTContext &ctx = decl->getASTContext();
  auto *importer = static_cast<ClangImporter *>(ctx.getClangModuleLoader());

  TinyPtrVector<ValueDecl *> result;

  if (id.isOperator()) {
    auto underlyingId =
        ctx.getIdentifier(getPrivateOperatorName(std::string(id)));
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

static FuncDecl *getInsertFunc(NominalTypeDecl *decl,
                               TypeAliasDecl *valueType) {
  ASTContext &ctx = decl->getASTContext();

  auto insertId = ctx.getIdentifier("__insertUnsafe");
  auto inserts = lookupDirectWithoutExtensions(decl, insertId);
  FuncDecl *insert = nullptr;
  for (auto candidate : inserts) {
    if (auto candidateMethod = dyn_cast<FuncDecl>(candidate)) {
      if (!candidateMethod->hasParameterList())
        continue;
      auto params = candidateMethod->getParameters();
      if (params->size() != 1)
        continue;
      auto param = params->front();
      if (param->getTypeInContext()->getCanonicalType() !=
          valueType->getUnderlyingType()->getCanonicalType())
        continue;
      insert = candidateMethod;
      break;
    }
  }
  return insert;
}

static bool isStdDecl(const clang::CXXRecordDecl *clangDecl,
                      llvm::ArrayRef<StringRef> names) {
  if (!clangDecl->isInStdNamespace())
    return false;
  if (!clangDecl->getIdentifier())
    return false;
  StringRef name = clangDecl->getName();
  return llvm::is_contained(names, name);
}

static clang::TypeDecl *
lookupNestedClangTypeDecl(const clang::CXXRecordDecl *clangDecl,
                          StringRef name) {
  clang::IdentifierInfo *nestedDeclName =
      &clangDecl->getASTContext().Idents.get(name);
  auto nestedDecls = clangDecl->lookup(nestedDeclName);
  // If this is a templated typedef, Clang might have instantiated several
  // equivalent typedef decls. If they aren't equivalent, Clang has already
  // complained about this. Let's assume that they are equivalent. (see
  // filterNonConflictingPreviousTypedefDecls in clang/Sema/SemaDecl.cpp)
  if (nestedDecls.empty())
    return nullptr;
  auto nestedDecl = nestedDecls.front();
  return dyn_cast_or_null<clang::TypeDecl>(nestedDecl);
}

static clang::TypeDecl *
getIteratorCategoryDecl(const clang::CXXRecordDecl *clangDecl) {
  return lookupNestedClangTypeDecl(clangDecl, "iterator_category");
}

static clang::TypeDecl *
getIteratorConceptDecl(const clang::CXXRecordDecl *clangDecl) {
  return lookupNestedClangTypeDecl(clangDecl, "iterator_concept");
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
  auto module = decl->getModuleContext();
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
    if (!equalEqual || !equalEqual->hasParameterList())
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
    if (!minus || !minus->hasParameterList())
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
    if (!plusEqual || !plusEqual->hasParameterList())
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
  clangSema.LookupOverloadedBinOp(candidateSet, opKind, ops, {arg, arg}, true);

  clang::OverloadCandidateSet::iterator best;
  switch (candidateSet.BestViableFunction(clangSema, clang::SourceLocation(),
                                          best)) {
  case clang::OR_Success: {
    if (auto clangCallee = best->Function) {
      auto lookupTable = impl.findLookupTable(classDecl);
      addEntryToLookupTable(*lookupTable, clangCallee, impl.getNameImporter());
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
  auto lookupTable = impl.findLookupTable(classDecl);
  addEntryToLookupTable(*lookupTable, equalEqualDecl, impl.getNameImporter());
  return true;
}

bool swift::isIterator(const clang::CXXRecordDecl *clangDecl) {
  return getIteratorCategoryDecl(clangDecl);
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

void swift::conformToCxxIteratorIfNeeded(
    ClangImporter::Implementation &impl, NominalTypeDecl *decl,
    const clang::CXXRecordDecl *clangDecl) {
  PrettyStackTraceDecl trace("conforming to UnsafeCxxInputIterator", decl);

  assert(decl);
  assert(clangDecl);
  ASTContext &ctx = decl->getASTContext();
  clang::ASTContext &clangCtx = clangDecl->getASTContext();

  if (!ctx.getProtocol(KnownProtocolKind::UnsafeCxxInputIterator))
    return;

  // We consider a type to be an input iterator if it defines an
  // `iterator_category` that inherits from `std::input_iterator_tag`, e.g.
  // `using iterator_category = std::input_iterator_tag`.
  auto iteratorCategory = getIteratorCategoryDecl(clangDecl);
  if (!iteratorCategory)
    return;

  auto unwrapUnderlyingTypeDecl =
      [](clang::TypeDecl *typeDecl) -> clang::CXXRecordDecl * {
    clang::CXXRecordDecl *underlyingDecl = nullptr;
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
  if (auto iteratorConcept = getIteratorConceptDecl(clangDecl)) {
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

  // Check if present: `var pointee: Pointee { get }`
  auto pointeeId = ctx.getIdentifier("pointee");
  auto pointee = lookupDirectSingleWithoutExtensions<VarDecl>(decl, pointeeId);
  if (!pointee || pointee->isGetterMutating() || pointee->getTypeInContext()->hasError())
    return;

  // Check if `var pointee: Pointee` is settable. This is required for the
  // conformance to UnsafeCxxMutableInputIterator but is not necessary for
  // UnsafeCxxInputIterator.
  bool pointeeSettable = pointee->isSettable(nullptr);

  // Check if present: `func successor() -> Self`
  auto successorId = ctx.getIdentifier("successor");
  auto successor =
      lookupDirectSingleWithoutExtensions<FuncDecl>(decl, successorId);
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

  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("Pointee"),
                               pointee->getTypeInContext());
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

void swift::conformToCxxConvertibleToBoolIfNeeded(
    ClangImporter::Implementation &impl, swift::NominalTypeDecl *decl,
    const clang::CXXRecordDecl *clangDecl) {
  PrettyStackTraceDecl trace("conforming to CxxConvertibleToBool", decl);

  assert(decl);
  assert(clangDecl);
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

void swift::conformToCxxOptionalIfNeeded(
    ClangImporter::Implementation &impl, NominalTypeDecl *decl,
    const clang::CXXRecordDecl *clangDecl) {
  PrettyStackTraceDecl trace("conforming to CxxOptional", decl);

  assert(decl);
  assert(clangDecl);
  ASTContext &ctx = decl->getASTContext();

  if (!isStdDecl(clangDecl, {"optional"}))
    return;

  ProtocolDecl *cxxOptionalProto =
      ctx.getProtocol(KnownProtocolKind::CxxOptional);
  // If the Cxx module is missing, or does not include one of the necessary
  // protocol, bail.
  if (!cxxOptionalProto)
    return;

  auto pointeeId = ctx.getIdentifier("pointee");
  auto pointees = lookupDirectWithoutExtensions(decl, pointeeId);
  if (pointees.size() != 1)
    return;
  auto pointee = dyn_cast<VarDecl>(pointees.front());
  if (!pointee)
    return;
  auto pointeeTy = pointee->getInterfaceType();

  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("Wrapped"), pointeeTy);
  impl.addSynthesizedProtocolAttrs(decl, {KnownProtocolKind::CxxOptional});
}

void swift::conformToCxxSequenceIfNeeded(
    ClangImporter::Implementation &impl, NominalTypeDecl *decl,
    const clang::CXXRecordDecl *clangDecl) {
  PrettyStackTraceDecl trace("conforming to CxxSequence", decl);

  assert(decl);
  assert(clangDecl);
  ASTContext &ctx = decl->getASTContext();

  ProtocolDecl *cxxIteratorProto =
      ctx.getProtocol(KnownProtocolKind::UnsafeCxxInputIterator);
  ProtocolDecl *cxxSequenceProto =
      ctx.getProtocol(KnownProtocolKind::CxxSequence);
  ProtocolDecl *cxxConvertibleProto =
      ctx.getProtocol(KnownProtocolKind::CxxConvertibleToCollection);
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

  // Take the default definition of `Iterator` from CxxSequence protocol. This
  // type is currently `CxxIterator<Self>`.
  auto iteratorDecl = cxxSequenceProto->getAssociatedType(ctx.Id_Iterator);
  auto iteratorTy = iteratorDecl->getDefaultDefinitionType();
  // Substitute generic `Self` parameter.
  auto cxxSequenceSelfTy = cxxSequenceProto->getSelfInterfaceType();
  auto declSelfTy = decl->getDeclaredInterfaceType();
  iteratorTy = iteratorTy.subst(
      [&](SubstitutableType *dependentType) {
        if (dependentType->isEqual(cxxSequenceSelfTy))
          return declSelfTy;
        return Type(dependentType);
      },
      LookUpConformanceInModule());

  impl.addSynthesizedTypealias(decl, ctx.Id_Element, pointeeTy);
  impl.addSynthesizedTypealias(decl, ctx.Id_Iterator, iteratorTy);
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("RawIterator"),
                               rawIteratorTy);
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

    impl.addSynthesizedTypealias(decl, ctx.getIdentifier("Element"), pointeeTy);
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
  if (!conformedToRAC && cxxConvertibleProto) {
    impl.addSynthesizedTypealias(decl, ctx.getIdentifier("Element"), pointeeTy);
    impl.addSynthesizedProtocolAttrs(
        decl, {KnownProtocolKind::CxxConvertibleToCollection});
  }
}

static bool isStdSetType(const clang::CXXRecordDecl *clangDecl) {
  return isStdDecl(clangDecl, {"set", "unordered_set", "multiset"});
}

static bool isStdMapType(const clang::CXXRecordDecl *clangDecl) {
  return isStdDecl(clangDecl, {"map", "unordered_map", "multimap"});
}

bool swift::isUnsafeStdMethod(const clang::CXXMethodDecl *methodDecl) {
  auto parentDecl =
      dyn_cast<clang::CXXRecordDecl>(methodDecl->getDeclContext());
  if (!parentDecl)
    return false;
  if (!isStdSetType(parentDecl) && !isStdMapType(parentDecl))
    return false;
  if (methodDecl->getDeclName().isIdentifier() &&
      methodDecl->getName() == "insert")
    return true;
  return false;
}

void swift::conformToCxxSetIfNeeded(ClangImporter::Implementation &impl,
                                    NominalTypeDecl *decl,
                                    const clang::CXXRecordDecl *clangDecl) {
  PrettyStackTraceDecl trace("conforming to CxxSet", decl);

  assert(decl);
  assert(clangDecl);
  ASTContext &ctx = decl->getASTContext();

  // Only auto-conform types from the C++ standard library. Custom user types
  // might have a similar interface but different semantics.
  if (!isStdSetType(clangDecl))
    return;

  auto valueType = lookupDirectSingleWithoutExtensions<TypeAliasDecl>(
      decl, ctx.getIdentifier("value_type"));
  auto sizeType = lookupDirectSingleWithoutExtensions<TypeAliasDecl>(
      decl, ctx.getIdentifier("size_type"));
  if (!valueType || !sizeType)
    return;

  auto insert = getInsertFunc(decl, valueType);
  if (!insert)
    return;

  impl.addSynthesizedTypealias(decl, ctx.Id_Element,
                               valueType->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.Id_ArrayLiteralElement,
                               valueType->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("Size"),
                               sizeType->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("InsertionResult"),
                               insert->getResultInterfaceType());
  impl.addSynthesizedProtocolAttrs(decl, {KnownProtocolKind::CxxSet});

  // If this isn't a std::multiset, try to also synthesize the conformance to
  // CxxUniqueSet.
  if (!isStdDecl(clangDecl, {"set", "unordered_set"}))
    return;

  ProtocolDecl *cxxInputIteratorProto =
      ctx.getProtocol(KnownProtocolKind::UnsafeCxxInputIterator);
  if (!cxxInputIteratorProto)
    return;

  auto rawIteratorType = lookupDirectSingleWithoutExtensions<TypeAliasDecl>(
      decl, ctx.getIdentifier("const_iterator"));
  auto rawMutableIteratorType =
      lookupDirectSingleWithoutExtensions<TypeAliasDecl>(
          decl, ctx.getIdentifier("iterator"));
  if (!rawIteratorType || !rawMutableIteratorType)
    return;

  auto rawIteratorTy = rawIteratorType->getUnderlyingType();
  auto rawMutableIteratorTy = rawMutableIteratorType->getUnderlyingType();

  if (!checkConformance(rawIteratorTy, cxxInputIteratorProto) ||
      !checkConformance(rawMutableIteratorTy, cxxInputIteratorProto))
    return;

  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("RawIterator"),
                               rawIteratorTy);
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("RawMutableIterator"),
                               rawMutableIteratorTy);
  impl.addSynthesizedProtocolAttrs(decl, {KnownProtocolKind::CxxUniqueSet});
}

void swift::conformToCxxPairIfNeeded(ClangImporter::Implementation &impl,
                                     NominalTypeDecl *decl,
                                     const clang::CXXRecordDecl *clangDecl) {
  PrettyStackTraceDecl trace("conforming to CxxPair", decl);

  assert(decl);
  assert(clangDecl);
  ASTContext &ctx = decl->getASTContext();

  // Only auto-conform types from the C++ standard library. Custom user types
  // might have a similar interface but different semantics.
  if (!isStdDecl(clangDecl, {"pair"}))
    return;

  auto firstType = lookupDirectSingleWithoutExtensions<TypeAliasDecl>(
      decl, ctx.getIdentifier("first_type"));
  auto secondType = lookupDirectSingleWithoutExtensions<TypeAliasDecl>(
      decl, ctx.getIdentifier("second_type"));
  if (!firstType || !secondType)
    return;

  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("First"),
                               firstType->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("Second"),
                               secondType->getUnderlyingType());
  impl.addSynthesizedProtocolAttrs(decl, {KnownProtocolKind::CxxPair});
}

void swift::conformToCxxDictionaryIfNeeded(
    ClangImporter::Implementation &impl, NominalTypeDecl *decl,
    const clang::CXXRecordDecl *clangDecl) {
  PrettyStackTraceDecl trace("conforming to CxxDictionary", decl);

  assert(decl);
  assert(clangDecl);
  ASTContext &ctx = decl->getASTContext();

  // Only auto-conform types from the C++ standard library. Custom user types
  // might have a similar interface but different semantics.
  if (!isStdMapType(clangDecl))
    return;

  auto keyType = lookupDirectSingleWithoutExtensions<TypeAliasDecl>(
      decl, ctx.getIdentifier("key_type"));
  auto valueType = lookupDirectSingleWithoutExtensions<TypeAliasDecl>(
      decl, ctx.getIdentifier("mapped_type"));
  auto iterType = lookupDirectSingleWithoutExtensions<TypeAliasDecl>(
      decl, ctx.getIdentifier("const_iterator"));
  auto mutableIterType = lookupDirectSingleWithoutExtensions<TypeAliasDecl>(
      decl, ctx.getIdentifier("iterator"));
  auto sizeType = lookupDirectSingleWithoutExtensions<TypeAliasDecl>(
      decl, ctx.getIdentifier("size_type"));
  auto keyValuePairType = lookupDirectSingleWithoutExtensions<TypeAliasDecl>(
      decl, ctx.getIdentifier("value_type"));
  if (!keyType || !valueType || !iterType || !mutableIterType || !sizeType ||
      !keyValuePairType)
    return;

  auto insert = getInsertFunc(decl, keyValuePairType);
  if (!insert)
    return;

  ProtocolDecl *cxxInputIteratorProto =
      ctx.getProtocol(KnownProtocolKind::UnsafeCxxInputIterator);
  ProtocolDecl *cxxMutableInputIteratorProto =
      ctx.getProtocol(KnownProtocolKind::UnsafeCxxMutableInputIterator);
  if (!cxxInputIteratorProto || !cxxMutableInputIteratorProto)
    return;

  auto rawIteratorTy = iterType->getUnderlyingType();
  auto rawMutableIteratorTy = mutableIterType->getUnderlyingType();

  // Check if RawIterator conforms to UnsafeCxxInputIterator.
  if (!checkConformance(rawIteratorTy, cxxInputIteratorProto))
    return;

  // Check if RawMutableIterator conforms to UnsafeCxxMutableInputIterator.
  if (!checkConformance(rawMutableIteratorTy, cxxMutableInputIteratorProto))
    return;

  // Make the original subscript that returns a non-optional value unavailable.
  // CxxDictionary adds another subscript that returns an optional value,
  // similarly to Swift.Dictionary.
  for (auto member : decl->getCurrentMembersWithoutLoading()) {
    if (auto subscript = dyn_cast<SubscriptDecl>(member)) {
      impl.markUnavailable(subscript,
                           "use subscript with optional return value");
    }
  }

  impl.addSynthesizedTypealias(decl, ctx.Id_Key, keyType->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.Id_Value,
                               valueType->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.Id_Element,
                               keyValuePairType->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("RawIterator"),
                               rawIteratorTy);
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("RawMutableIterator"),
                               rawMutableIteratorTy);
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("Size"),
                               sizeType->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("InsertionResult"),
                               insert->getResultInterfaceType());
  impl.addSynthesizedProtocolAttrs(decl, {KnownProtocolKind::CxxDictionary});
}

void swift::conformToCxxVectorIfNeeded(ClangImporter::Implementation &impl,
                                       NominalTypeDecl *decl,
                                       const clang::CXXRecordDecl *clangDecl) {
  PrettyStackTraceDecl trace("conforming to CxxVector", decl);

  assert(decl);
  assert(clangDecl);
  ASTContext &ctx = decl->getASTContext();

  // Only auto-conform types from the C++ standard library. Custom user types
  // might have a similar interface but different semantics.
  if (!isStdDecl(clangDecl, {"vector"}))
    return;

  auto valueType = lookupDirectSingleWithoutExtensions<TypeAliasDecl>(
      decl, ctx.getIdentifier("value_type"));
  auto iterType = lookupDirectSingleWithoutExtensions<TypeAliasDecl>(
      decl, ctx.getIdentifier("const_iterator"));
  if (!valueType || !iterType)
    return;

  ProtocolDecl *cxxRandomAccessIteratorProto =
      ctx.getProtocol(KnownProtocolKind::UnsafeCxxRandomAccessIterator);
  if (!cxxRandomAccessIteratorProto)
    return;

  auto rawIteratorTy = iterType->getUnderlyingType();

  // Check if RawIterator conforms to UnsafeCxxRandomAccessIterator.
  if (!checkConformance(rawIteratorTy, cxxRandomAccessIteratorProto))
    return;

  impl.addSynthesizedTypealias(decl, ctx.Id_Element,
                               valueType->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.Id_ArrayLiteralElement,
                               valueType->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("RawIterator"),
                               rawIteratorTy);
  impl.addSynthesizedProtocolAttrs(decl, {KnownProtocolKind::CxxVector});
}

void swift::conformToCxxFunctionIfNeeded(
    ClangImporter::Implementation &impl, NominalTypeDecl *decl,
    const clang::CXXRecordDecl *clangDecl) {
  PrettyStackTraceDecl trace("conforming to CxxFunction", decl);

  assert(decl);
  assert(clangDecl);
  ASTContext &ctx = decl->getASTContext();
  clang::ASTContext &clangCtx = impl.getClangASTContext();
  clang::Sema &clangSema = impl.getClangSema();

  // Only auto-conform types from the C++ standard library. Custom user types
  // might have a similar interface but different semantics.
  if (!isStdDecl(clangDecl, {"function"}))
    return;

  // There is no typealias for the argument types on the C++ side, so to
  // retrieve the argument types we look at the overload of `operator()` that
  // got imported into Swift.

  auto callAsFunctionDecl = lookupDirectSingleWithoutExtensions<FuncDecl>(
      decl, ctx.getIdentifier("callAsFunction"));
  if (!callAsFunctionDecl)
    return;

  auto operatorCallDecl = dyn_cast_or_null<clang::CXXMethodDecl>(
      callAsFunctionDecl->getClangDecl());
  if (!operatorCallDecl)
    return;

  std::vector<clang::QualType> operatorCallParamTypes;
  llvm::transform(
      operatorCallDecl->parameters(),
      std::back_inserter(operatorCallParamTypes),
      [](const clang::ParmVarDecl *paramDecl) { return paramDecl->getType(); });

  auto funcPointerType = clangCtx.getPointerType(clangCtx.getFunctionType(
      operatorCallDecl->getReturnType(), operatorCallParamTypes,
      clang::FunctionProtoType::ExtProtoInfo())).withConst();

  // Create a fake variable with a function type that matches the type of
  // `operator()`.
  auto fakeFuncPointerVarDecl = clang::VarDecl::Create(
      clangCtx, /*DC*/ clangCtx.getTranslationUnitDecl(),
      clang::SourceLocation(), clang::SourceLocation(), /*Id*/ nullptr,
      funcPointerType, clangCtx.getTrivialTypeSourceInfo(funcPointerType),
      clang::StorageClass::SC_None);
  auto fakeFuncPointerRefExpr = new (clangCtx) clang::DeclRefExpr(
      clangCtx, fakeFuncPointerVarDecl, false, funcPointerType,
      clang::ExprValueKind::VK_LValue, clang::SourceLocation());

  auto clangDeclTyInfo = clangCtx.getTrivialTypeSourceInfo(
      clang::QualType(clangDecl->getTypeForDecl(), 0));
  SmallVector<clang::Expr *, 1> constructExprArgs = {fakeFuncPointerRefExpr};

  // Instantiate the templated constructor that would accept this fake variable.
  auto constructExprResult = clangSema.BuildCXXTypeConstructExpr(
      clangDeclTyInfo, clangDecl->getLocation(), constructExprArgs,
      clangDecl->getLocation(), /*ListInitialization*/ false);
  if (!constructExprResult.isUsable())
    return;

  auto castExpr = dyn_cast_or_null<clang::CastExpr>(constructExprResult.get());
  if (!castExpr)
    return;

  auto bindTempExpr =
      dyn_cast_or_null<clang::CXXBindTemporaryExpr>(castExpr->getSubExpr());
  if (!bindTempExpr)
    return;

  auto constructExpr =
      dyn_cast_or_null<clang::CXXConstructExpr>(bindTempExpr->getSubExpr());
  if (!constructExpr)
    return;

  auto constructorDecl = constructExpr->getConstructor();

  auto importedConstructor =
      impl.importDecl(constructorDecl, impl.CurrentVersion);
  if (!importedConstructor)
    return;
  decl->addMember(importedConstructor);

  // TODO: actually conform to some form of CxxFunction protocol

}

void swift::conformToCxxSpanIfNeeded(ClangImporter::Implementation &impl,
                                     NominalTypeDecl *decl,
                                     const clang::CXXRecordDecl *clangDecl) {
  PrettyStackTraceDecl trace("conforming to CxxSpan", decl);

  assert(decl);
  assert(clangDecl);
  ASTContext &ctx = decl->getASTContext();
  clang::ASTContext &clangCtx = impl.getClangASTContext();
  clang::Sema &clangSema = impl.getClangSema();

  // Only auto-conform types from the C++ standard library. Custom user types
  // might have a similar interface but different semantics.
  if (!isStdDecl(clangDecl, {"span"}))
    return;

  auto elementType = lookupDirectSingleWithoutExtensions<TypeAliasDecl>(
      decl, ctx.getIdentifier("element_type"));
  auto sizeType = lookupDirectSingleWithoutExtensions<TypeAliasDecl>(
      decl, ctx.getIdentifier("size_type"));

  if (!elementType || !sizeType)
    return;

  auto pointerTypeDecl = lookupNestedClangTypeDecl(clangDecl, "pointer");
  auto countTypeDecl = lookupNestedClangTypeDecl(clangDecl, "size_type");

  if (!pointerTypeDecl || !countTypeDecl)
    return;

  // create fake variable for pointer (constructor arg 1)
  clang::QualType pointerType = clangCtx.getTypeDeclType(pointerTypeDecl);
  auto fakePointerVarDecl = clang::VarDecl::Create(
      clangCtx, /*DC*/ clangCtx.getTranslationUnitDecl(),
      clang::SourceLocation(), clang::SourceLocation(), /*Id*/ nullptr,
      pointerType, clangCtx.getTrivialTypeSourceInfo(pointerType),
      clang::StorageClass::SC_None);

  auto fakePointer = new (clangCtx) clang::DeclRefExpr(
      clangCtx, fakePointerVarDecl, false, pointerType,
      clang::ExprValueKind::VK_LValue, clang::SourceLocation());

  // create fake variable for count (constructor arg 2)
  auto countType = clangCtx.getTypeDeclType(countTypeDecl);
  auto fakeCountVarDecl = clang::VarDecl::Create(
      clangCtx, /*DC*/ clangCtx.getTranslationUnitDecl(),
      clang::SourceLocation(), clang::SourceLocation(), /*Id*/ nullptr,
      countType, clangCtx.getTrivialTypeSourceInfo(countType),
      clang::StorageClass::SC_None);

  auto fakeCount = new (clangCtx) clang::DeclRefExpr(
      clangCtx, fakeCountVarDecl, false, countType,
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

  auto attr = AvailableAttr::createPlatformAgnostic(importedConstructor->getASTContext(), "use 'init(_:)' instead.", "", PlatformAgnosticAvailabilityKind::Deprecated);
  importedConstructor->getAttrs().add(attr);

  decl->addMember(importedConstructor);

  impl.addSynthesizedTypealias(decl, ctx.Id_Element,
                               elementType->getUnderlyingType());
  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("Size"),
                               sizeType->getUnderlyingType());

  if (pointerType->getPointeeType().isConstQualified()) {
    impl.addSynthesizedProtocolAttrs(decl, {KnownProtocolKind::CxxSpan});
  } else {
    impl.addSynthesizedProtocolAttrs(decl, {KnownProtocolKind::CxxMutableSpan});
  }
}
