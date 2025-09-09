// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/unsafe_swift_decls.swiftmodule %S/Inputs/unsafe_swift_decls.swift

// RUN: %target-typecheck-verify-swift -strict-memory-safety -enable-experimental-feature StrictConcurrency -I %t

// REQUIRES: concurrency
// REQUIRES: swift_feature_StrictConcurrency

@preconcurrency @unsafe import unsafe_swift_decls
