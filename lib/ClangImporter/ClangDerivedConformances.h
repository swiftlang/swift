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

bool isIterator(const clang::CXXRecordDecl *clangDecl);

/// If the decl is a C++ input iterator, synthesize a conformance to the
/// UnsafeCxxInputIterator protocol, which is defined in the std overlay.
void conformToCxxIteratorIfNeeded(ClangImporter::Implementation &impl,
                                  NominalTypeDecl *decl,
                                  const clang::CXXRecordDecl *clangDecl);

} // namespace swift

#endif // SWIFT_CLANG_DERIVED_CONFORMANCES_H
