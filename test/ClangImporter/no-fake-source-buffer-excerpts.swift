// REQUIRES: OS=macosx
//
// This triggers a warning about ignored configuration macros; the warning then
// attempts to emit an excerpt from one of the clang importer's fake buffers
// (<swift-imported-modules>) which is full of 250kb of nulls. We want to check
// that we do not emit a gigantic block of nulls to stderr.
//
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -I %S/Inputs/custom-modules -import-objc-header %S/Inputs/no-fake-source-buffer-excerpts.h %s 2>%t/errors
// RUN: od -a < %t/errors | %FileCheck %s
// CHECK-NOT: nul nul nul nul
