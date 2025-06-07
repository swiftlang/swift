// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %t/Library.swift -module-name Library
// RUN: rm -f %t/Library.swiftmodule
// RUN: %target-swift-frontend -I %t -typecheck -verify %t/test.swift


//--- Library.swift

public struct Hello<T: ~Copyable> {
  public init() {}
}

//--- test.swift
import Library

struct NC: ~Copyable {}

let x: Hello<NC> = .init()
