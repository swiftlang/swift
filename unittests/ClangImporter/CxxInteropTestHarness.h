//===--- CxxInteropTestHarness.h --------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// A lightweight in-process harness for C++ interop unit tests. It imports a
// snippet of C++ source (as an in-memory bridging header) and lets a callback
// inspect either the imported Swift Decl or the underlying clang::Decl:
//
//   import("struct S { int a; };")
//       .checkDecl("S", [](ValueDecl *d) { ... });
//
//   compile("struct S { int a; };")
//       .checkCppDecl("S", [](const clang::Decl *d, ASTContext &ctx) { ... });
//
// The Swift standard library is loaded so importing pointer-bearing C++ types
// succeeds. Reuse this harness from any unittests/ClangImporter test.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_UNITTESTS_CLANGIMPORTER_CXXINTEROPTESTHARNESS_H
#define SWIFT_UNITTESTS_CLANGIMPORTER_CXXINTEROPTESTHARNESS_H

#include "llvm/ADT/STLFunctionalExtras.h"
#include "llvm/ADT/StringRef.h"
#include <memory>

namespace clang {
class Decl;
} // end namespace clang

namespace swift {

class ASTContext;
class ValueDecl;

namespace unittest {

class CxxInteropModule;

/// Fluent handle over an imported C++ test module. 
class CxxInteropChecker {
  std::shared_ptr<CxxInteropModule> impl;

public:
  explicit CxxInteropChecker(llvm::StringRef cxxSource);

  /// Look up the imported Swift decl named \p name and invoke \p check on it.
  CxxInteropChecker &
  checkDecl(llvm::StringRef name,
            llvm::function_ref<void(ValueDecl *)> check);

  /// Look up the imported Swift decl named \p name, then invoke \p check on the
  /// underlying clang::Decl. Going through the Swift import ensures the decl (and any
  /// referenced template specialization) is fully instantiated.
  CxxInteropChecker &checkCppDecl(
      llvm::StringRef name,
      llvm::function_ref<void(const clang::Decl *, ASTContext &)> check);
};

/// Import a snippet of C++ source into a synthetic module.
CxxInteropChecker import(llvm::StringRef cxxSource);

/// Alias for `import`, reading more naturally when a test cares about the
/// clang-side decl rather than the imported Swift decl.
CxxInteropChecker compile(llvm::StringRef cxxSource);

} // end namespace unittest
} // end namespace swift

// Attribute spellings for forcing escapability/ownership in C++ test inputs.
// Use with string concatenation, e.g.:
//   compile("struct " SWIFT_TEST_NONESCAPABLE " V { int *p; };")
#define SWIFT_TEST_NONESCAPABLE R"(__attribute__((swift_attr("~Escapable"))))"
#define SWIFT_TEST_ESCAPABLE R"(__attribute__((swift_attr("Escapable"))))"
#define SWIFT_TEST_SELF_CONTAINED R"(__attribute__((swift_attr("import_owned"))))"
#define SWIFT_TEST_UNSAFE R"(__attribute__((swift_attr("unsafe"))))"

#endif // SWIFT_UNITTESTS_CLANGIMPORTER_CXXINTEROPTESTHARNESS_H
