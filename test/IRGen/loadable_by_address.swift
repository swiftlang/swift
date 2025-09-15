// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -I %t -c %s

import resilient_struct

public struct LargeValue {
  var field : (Int64, Int64, Int64, Int64, Int64,
               Int64, Int64, Int64, Int64, Int64)? = nil
  public init() {}
}

// This test use to crash with a compiler assert.
public class Test {
  public typealias Closure = ((LargeValue, _ last: Bool) -> Void)

  public init() {}

  public var t : Test?

  public func forceClosure(_ cl: () -> ()) {
    cl()
  }

  public func execute(input: (arg1: Size, closure: Closure)) {
    if let _t = t {
      _t.forceClosure {
        var i = input
      }
    }
  }
}

// So did this test case.
enum E {
  case a
}

protocol P {
  associatedtype A
  typealias S = C<A>.S

  var v: E { get set }

  var v2: (t: Int, i: S)? { get set }
}

class C<A> {
  struct S { }

  init(_: ClosedRange<Double>) { }
}

class C2<T>: P {
  typealias A = T
  var v: E = .a

  var v2: (t: Int, i: S)?
}
