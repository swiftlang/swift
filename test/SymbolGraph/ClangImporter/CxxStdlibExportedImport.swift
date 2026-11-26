// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -module-name CxxStdlib -cxx-interoperability-mode=default %s -emit-symbol-graph -emit-symbol-graph-dir %t
// RUN: %FileCheck %s --input-file %t/CxxStdlib.symbols.json

// A Swift overlay named like the C++ standard library module re-exports the
// Clang module of the same name. Emitting a symbol graph for it must not trip
// the "no duplicate decls" assertion in `getDisplayDeclsRecursivelyAndImports`.
// Synthesized declarations (such as `!=` for C++ types with a member
// `operator!=`) belong to the bridging header module, are not filtered by
// `ClangModuleUnit`, and used to be returned once per `std` submodule.

// REQUIRES: OS=macosx

// CHECK: "symbols"

@_exported import CxxStdlib

public func fromCxxStdlibOverlay() {}
