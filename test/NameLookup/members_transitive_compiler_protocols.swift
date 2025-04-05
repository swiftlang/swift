// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -o %t %t/lib.swift
// RUN: %target-swift-frontend -typecheck -primary-file %t/main.swift %t/other.swift -I %t -verify -enable-upcoming-feature MemberImportVisibility

// REQUIRES: swift_feature_MemberImportVisibility

//--- main.swift

import Swift
// expected-note 2 {{add import of module 'lib'}}

for _ in makeSequence() { }
// expected-error@-1 {{instance method 'makeIterator()' is not available due to missing import of defining module 'lib'}}
// expected-error@-2 {{instance method 'next()' is not available due to missing import of defining module 'lib'}}

//--- other.swift

import lib

func makeSequence() -> EmptySequence {
  return MySequence()
}

//--- lib.swift

public struct EmptySequence: Sequence {
  public struct Iterator: IteratorProtocol {
    public mutating func next() -> Int? { nil }
  }

  public func makeIterator() -> Iterator { Iterator() }
  public init() { }
}
