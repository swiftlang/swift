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

/// Whether a C++ record decl contains a type member named "iterator_category",
/// a heuristic we use to determine whether that record type is an iterator.
///
/// This function returns true if there is exactly one public type member named
/// "iterator_category", but does not look for inherited members. Note that, as
/// a result of these limitations, it may return false even if that record type
/// is usable as an iterator.
bool hasIteratorCategory(const clang::CXXRecordDecl *clangDecl);

bool isUnsafeStdMethod(const clang::CXXMethodDecl *methodDecl);

void deriveAutomaticCxxConformances(ClangImporter::Implementation &Impl,
                                    NominalTypeDecl *result,
                                    const clang::CXXRecordDecl *clangDecl);
} // namespace swift

#endif // SWIFT_CLANG_DERIVED_CONFORMANCES_H
