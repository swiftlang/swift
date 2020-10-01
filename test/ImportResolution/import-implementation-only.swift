// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/abcde.swiftmodule %S/Inputs/abcde.swift
// RUN: %target-swift-frontend -emit-module -o %t/Library.swiftmodule %s -I %t

// RUN: echo 'import Library; foo()' > %t/main.swift
// RUN: %target-swift-frontend -typecheck %t/main.swift -I %t

// Delete the indirect dependency; everything should still work.
// RUN: rm %t/abcde.swiftmodule
// RUN: %target-swift-frontend -typecheck %t/main.swift -I %t

@_implementationOnly import abcde

public func foo() {}
