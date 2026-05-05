// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %swift-interface-tool -action minimize %t/no-imports/input.swift > %t/no-imports/output.swift
// RUN: %diff -u %t/no-imports/expected.swift %t/no-imports/output.swift

// RUN: %swift-interface-tool -action minimize %t/empty-input/input.swift > %t/empty-input/output.swift
// RUN: %diff -u %t/empty-input/expected.swift %t/empty-input/output.swift

// RUN: %swift-interface-tool -action minimize %t/comments-only/input.swift > %t/comments-only/output.swift
// RUN: %diff -u %t/comments-only/expected.swift %t/comments-only/output.swift

// RUN: %swift-interface-tool -action minimize %t/header-only/input.swift > %t/header-only/output.swift
// RUN: %diff -u %t/header-only/expected.swift %t/header-only/output.swift

//--- no-imports/input.swift
public func publicFunc() {
  print("hello")
}

public struct Foo {
  public var x: Int
}

public class Bar {}
public protocol Baz {}
public enum Qux { case a, b }
public typealias TopAlias = Int
public var topVar: Int = 0

infix operator <+> : AdditionPrecedence

// An `#if` without canImport and with an empty body after minimization is
// removed.
#if DEBUG
public func debugFunc() {}
#endif
//--- no-imports/expected.swift
//--- empty-input/input.swift
//--- empty-input/expected.swift
//--- comments-only/input.swift
// A regular comment.
// swift-interface-format-version: 1.0
// swift-compiler-version: Apple Swift version 6.0
// swift-module-flags: -target arm64-apple-macos14.0 -module-name MyModule

/// A doc comment, attached to nothing.

/* A block comment. */

//--- comments-only/expected.swift
// swift-interface-format-version: 1.0
// swift-compiler-version: Apple Swift version 6.0
// swift-module-flags: -target arm64-apple-macos14.0 -module-name MyModule
//--- header-only/input.swift
// swift-interface-format-version: 1.0
// swift-compiler-version: Apple Swift version 6.0
// swift-module-flags: -target arm64-apple-macos14.0 -module-name MyModule
public func publicFunc() {}

public struct Foo {}
//--- header-only/expected.swift
// swift-interface-format-version: 1.0
// swift-compiler-version: Apple Swift version 6.0
// swift-module-flags: -target arm64-apple-macos14.0 -module-name MyModule
