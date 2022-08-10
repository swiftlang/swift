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
#include "swift/AST/ParameterList.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/ClangImporter/ClangImporterRequests.h"

using namespace swift;

/// Alternative to `NominalTypeDecl::lookupDirect`.
/// This function does not attempt to load extensions of the nominal decl.
static TinyPtrVector<ValueDecl *>
lookupDirectWithoutExtensions(NominalTypeDecl *decl, Identifier id) {
  // First see if there is a Clang decl with the given name.
  TinyPtrVector<ValueDecl *> result = evaluateOrDefault(
      decl->getASTContext().evaluator, ClangRecordMemberLookup({decl, id}), {});

  // Check if there are any synthesized Swift members that match the name.
  for (auto member : decl->getMembers()) {
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

static clang::TypeDecl *
getIteratorCategoryDecl(const clang::CXXRecordDecl *clangDecl) {
  clang::IdentifierInfo *iteratorCategoryDeclName =
      &clangDecl->getASTContext().Idents.get("iterator_category");
  auto iteratorCategories = clangDecl->lookup(iteratorCategoryDeclName);
  if (!iteratorCategories.isSingleResult())
    return nullptr;
  auto iteratorCategory = iteratorCategories.front();

  return dyn_cast_or_null<clang::TypeDecl>(iteratorCategory);
}

static ValueDecl *getEqualEqualOperator(NominalTypeDecl *decl) {
  auto id = decl->getASTContext().Id_EqualsOperator;

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
    auto lhsTy = lhs->getType();
    auto rhsTy = rhs->getType();
    if (!lhsTy || !rhsTy)
      return false;
    auto lhsNominal = lhsTy->getAnyNominal();
    auto rhsNominal = rhsTy->getAnyNominal();
    if (lhsNominal != rhsNominal || lhsNominal != decl)
      return false;
    return true;
  };

  // First look for `func ==` declared as a member.
  auto memberResults = lookupDirectWithoutExtensions(decl, id);
  for (const auto &member : memberResults) {
    if (isValid(member))
      return member;
  }

  // If no member `func ==` was found, look for out-of-class definitions in the
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

bool swift::isIterator(const clang::CXXRecordDecl *clangDecl) {
  return getIteratorCategoryDecl(clangDecl);
}

void swift::conformToCxxIteratorIfNeeded(
    ClangImporter::Implementation &impl, NominalTypeDecl *decl,
    const clang::CXXRecordDecl *clangDecl) {
  PrettyStackTraceDecl trace("conforming to UnsafeCxxInputIterator", decl);

  assert(decl);
  assert(clangDecl);
  ASTContext &ctx = decl->getASTContext();

  // We consider a type to be an input iterator if it defines an
  // `iterator_category` that inherits from `std::input_iterator_tag`, e.g.
  // `using iterator_category = std::input_iterator_tag`.
  auto iteratorCategory = getIteratorCategoryDecl(clangDecl);
  if (!iteratorCategory)
    return;

  // If `iterator_category` is a typedef or a using-decl, retrieve the
  // underlying struct decl.
  clang::CXXRecordDecl *underlyingCategoryDecl = nullptr;
  if (auto typedefDecl = dyn_cast<clang::TypedefNameDecl>(iteratorCategory)) {
    auto type = typedefDecl->getUnderlyingType();
    underlyingCategoryDecl = type->getAsCXXRecordDecl();
  } else {
    underlyingCategoryDecl = dyn_cast<clang::CXXRecordDecl>(iteratorCategory);
  }
  if (underlyingCategoryDecl) {
    underlyingCategoryDecl = underlyingCategoryDecl->getDefinition();
  }

  if (!underlyingCategoryDecl)
    return;

  auto isInputIteratorDecl = [&](const clang::CXXRecordDecl *base) {
    return base->isInStdNamespace() && base->getIdentifier() &&
           base->getName() == "input_iterator_tag";
  };

  // Traverse all transitive bases of `underlyingDecl` to check if
  // it inherits from `std::input_iterator_tag`.
  bool isInputIterator = isInputIteratorDecl(underlyingCategoryDecl);
  underlyingCategoryDecl->forallBases([&](const clang::CXXRecordDecl *base) {
    if (isInputIteratorDecl(base)) {
      isInputIterator = true;
      return false;
    }
    return true;
  });

  if (!isInputIterator)
    return;

  // Check if present: `var pointee: Pointee { get }`
  auto pointeeId = ctx.getIdentifier("pointee");
  auto pointees = lookupDirectWithoutExtensions(decl, pointeeId);
  if (pointees.size() != 1)
    return;
  auto pointee = dyn_cast<VarDecl>(pointees.front());
  if (!pointee || pointee->isGetterMutating())
    return;

  // Check if present: `func successor() -> Self`
  auto successorId = ctx.getIdentifier("successor");
  auto successors = lookupDirectWithoutExtensions(decl, successorId);
  if (successors.size() != 1)
    return;
  auto successor = dyn_cast<FuncDecl>(successors.front());
  if (!successor || successor->isMutating())
    return;
  auto successorTy = successor->getResultInterfaceType();
  if (!successorTy || successorTy->getAnyNominal() != decl)
    return;

  // Check if present: `func ==`
  auto equalEqual = getEqualEqualOperator(decl);
  if (!equalEqual)
    return;

  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("Pointee"),
                               pointee->getType());
  impl.addSynthesizedProtocolAttrs(decl,
                                   {KnownProtocolKind::UnsafeCxxInputIterator});
}
