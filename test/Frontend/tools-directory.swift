// Check that clang is looked up in -tools-directory through the file system of
// the compiler, so the lookup is allowed by the IO sandbox.

// RUN: %empty-directory(%t)
// RUN: touch %t/clang
// RUN: %target-swift-frontend -typecheck -parse-stdlib %s -tools-directory %t
// RUN: %target-swift-frontend -emit-ir -parse-stdlib %s -tools-directory %t -o %t/main.ll

public func f() {}
