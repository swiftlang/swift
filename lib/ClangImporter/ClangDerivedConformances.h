//===--- ClangDerivedConformances.h -----------------------------*- C++ -*-===//
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

#ifndef SWIFT_CLANG_DERIVED_CONFORMANCES_H
#define SWIFT_CLANG_DERIVED_CONFORMANCES_H

#include "ImporterImpl.h"
#include "swift/AST/ASTContext.h"

namespace swift {

/// Whether a C++ record decl has a public type member named "iterator_category"
/// (or, from C++20 on, "iterator_concept"), a heuristic we use to determine
/// whether that record type is an iterator. The member may be declared by the
/// record itself, inherited from a base class, or provided by an explicit
/// specialization of std::iterator_traits.
bool hasIteratorCategoryOrIteratorConcept(
    clang::Sema &sema, const clang::CXXRecordDecl *clangDecl);

bool isUnsafeStdMethod(const clang::CXXMethodDecl *methodDecl);

void deriveAutomaticCxxConformances(ClangImporter::Implementation &Impl,
                                    NominalTypeDecl *result,
                                    const clang::CXXRecordDecl *clangDecl);
} // namespace swift

#endif // SWIFT_CLANG_DERIVED_CONFORMANCES_H
