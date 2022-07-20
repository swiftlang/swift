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
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"

using namespace swift;

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

bool swift::isIterator(const clang::CXXRecordDecl *clangDecl) {
  return getIteratorCategoryDecl(clangDecl);
}

void swift::conformToCxxIteratorIfNeeded(
    ClangImporter::Implementation &impl, NominalTypeDecl *decl,
    const clang::CXXRecordDecl *clangDecl) {
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
  auto pointees = decl->lookupDirect(pointeeId);
  if (pointees.size() != 1)
    return;
  auto pointee = dyn_cast<VarDecl>(pointees.front());
  if (!pointee || pointee->isGetterMutating())
    return;

  // Check if present: `func successor() -> Self`
  auto successorId = ctx.getIdentifier("successor");
  auto successors = decl->lookupDirect(successorId);
  if (successors.size() != 1)
    return;
  auto successor = dyn_cast<FuncDecl>(successors.front());
  if (!successor || successor->isMutating())
    return;
  auto successorTy = successor->getResultInterfaceType();
  if (!successorTy || successorTy->getAnyNominal() != decl)
    return;

  // Check if present: `func ==`
  // FIXME: this only detects `operator==` declared as a member.
  auto equalEquals = decl->lookupDirect(ctx.Id_EqualsOperator);
  if (equalEquals.empty())
    return;
  auto equalEqual = dyn_cast<FuncDecl>(equalEquals.front());
  if (!equalEqual || !equalEqual->hasParameterList())
    return;
  auto equalEqualParams = equalEqual->getParameters();
  if (equalEqualParams->size() != 2)
    return;
  auto equalEqualLHS = equalEqualParams->get(0);
  auto equalEqualRHS = equalEqualParams->get(1);
  if (equalEqualLHS->isInOut() || equalEqualRHS->isInOut())
    return;
  auto equalEqualLHSTy = equalEqualLHS->getType();
  auto equalEqualRHSTy = equalEqualRHS->getType();
  if (!equalEqualLHSTy || !equalEqualRHSTy ||
      equalEqualLHSTy->getAnyNominal() != equalEqualRHSTy->getAnyNominal())
    return;

  impl.addSynthesizedTypealias(decl, ctx.getIdentifier("Pointee"),
                               pointee->getType());
  impl.addSynthesizedProtocolAttrs(decl,
                                   {KnownProtocolKind::UnsafeCxxInputIterator});
}
