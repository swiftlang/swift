// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -o %t %t/lib.swift
// RUN: %target-swift-frontend -typecheck -primary-file %t/main.swift %t/other.swift -I %t -verify -enable-upcoming-feature MemberImportVisibility

// REQUIRES: swift_feature_MemberImportVisibility

//--- main.swift

import Swift
// expected-note 6 {{add import of module 'lib'}}

for _ in makeSequence() { }
// expected-error@-1 {{instance method 'makeIterator()' is not available due to missing import of defining module 'lib'}}
// expected-error@-2 {{instance method 'next()' is not available due to missing import of defining module 'lib'}}

takesMessage("\(1)")
// expected-error@-1 {{initializer 'init(stringInterpolation:)' is not available due to missing import of defining module 'lib'}}
// expected-error@-2 {{instance method 'appendInterpolation' is not available due to missing import of defining module 'lib'}}
// expected-error@-3 2 {{instance method 'appendLiteral' is not available due to missing import of defining module 'lib'}}

//--- other.swift

import lib

func makeSequence() -> EmptySequence {
  return MySequence()
}

func takesMessage(_ x: Message) { }

//--- lib.swift

public struct EmptySequence: Sequence {
  public struct Iterator: IteratorProtocol {
    public mutating func next() -> Int? { nil }
  }

  public func makeIterator() -> Iterator { Iterator() }
  public init() { }
}

public struct MessageInterpolation: StringInterpolationProtocol {
  public init(literalCapacity: Int, interpolationCount: Int) { }
  public mutating func appendInterpolation(_ value: @autoclosure () -> Int) { }
  public mutating func appendLiteral(_ literal: String) { }
}

public struct Message: ExpressibleByStringInterpolation {
  public init(stringInterpolation: MessageInterpolation) { }
  public init(stringLiteral: String) { }
}
