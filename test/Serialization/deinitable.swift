// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -enable-experimental-feature NondeinitableTypes -module-name DeinitableLib -o %t/DeinitableLib.swiftmodule %t/DeinitableLib.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature NondeinitableTypes -I %t %t/Client.swift -verify-ignore-unrelated

// REQUIRES: swift_feature_NondeinitableTypes

//--- DeinitableLib.swift

@frozen
public struct ND: ~Copyable, ~Deinitable {
  public init() {}

  public consuming func finish() {
    discard self
  }
}

public func borrowNondeinitable<T: ~Copyable & ~Deinitable>(_ t: borrowing T) {}

public func borrowNoncopyable<T: ~Copyable>(_ t: borrowing T) {}

public struct Box<T: ~Copyable & ~Deinitable>: ~Copyable, ~Deinitable {
  public let value: T
}

public protocol HasNondeinitable {
  associatedtype A: ~Copyable, ~Deinitable
}

public protocol HasNoncopyable {
  associatedtype A: ~Copyable
}

//--- Client.swift

import DeinitableLib

// `~Deinitable` survives serialization on a nominal type, a generic parameter,
// and an associated type.
func test(_ nd: borrowing ND) {
  borrowNondeinitable(nd) // Ok
  borrowNoncopyable(nd) // expected-error {{global function 'borrowNoncopyable' requires that 'ND' conform to 'Deinitable'}}
}

func box(_: borrowing Box<ND>) {} // Ok

struct ConformsWithNondeinitable: HasNondeinitable { // Ok
  typealias A = ND
}

func associatedTypes<T: HasNondeinitable, U: HasNoncopyable>(
  _: T.Type, _ t: borrowing T.A, _: U.Type, _ u: borrowing U.A
) {
  borrowNondeinitable(t) // Ok
  borrowNoncopyable(t) // expected-error {{global function 'borrowNoncopyable' requires that 'T.A' conform to 'Deinitable'}}
  borrowNoncopyable(u) // Ok
}
