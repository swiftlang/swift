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

/// If \p decl is a constructor of a non-Escapable 'std::optional', cache the
/// lifetime dependence of its result, at \p returnIdx, for \p result.
///
/// Returns true if it cached one. Returns false, leaving \p result to the
/// general lifetime inference, if \p decl is not such a constructor, or if one
/// of its arguments is neither the wrapped value nor a tag, so its dependence
/// cannot be known.
///
/// 'std::optional' stores copies of its constructor arguments, so the result
/// copies the lifetime of each argument. The 'std::nullopt_t' and
/// 'std::in_place_t' tags carry no lifetime, so an optional constructed from
/// tags alone, or from nothing, is immortal. This includes the constructor
/// synthesized for 'CxxOptional', the internal base classes standard libraries
/// build 'std::optional' on, and the constructors those classes inherit.
bool inferStdOptionalConstructorDependence(ClangImporter::Implementation &Impl,
                                           const clang::FunctionDecl *decl,
                                           AbstractFunctionDecl *result,
                                           size_t returnIdx);

void deriveAutomaticCxxConformances(ClangImporter::Implementation &Impl,
                                    NominalTypeDecl *result,
                                    const clang::CXXRecordDecl *clangDecl);
} // namespace swift

#endif // SWIFT_CLANG_DERIVED_CONFORMANCES_H
