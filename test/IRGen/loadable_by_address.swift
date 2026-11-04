// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -target %target-swift-6.2-abi-triple -enable-experimental-feature BuiltinModule -I %t -c %s
// RUN: %target-swift-frontend -target %target-swift-6.2-abi-triple -enable-experimental-feature BuiltinModule -I %t -c %s -O

// REQUIRES: swift_feature_BuiltinModule

import Builtin
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

// rdar://180755649
// This used to crash at -O with "Unimplemented user" on the struct instruction
// in UUID.uuid.modify.
public typealias uuid_t = (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8,
                           UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)

public struct UUID {
  var _storage: InlineArray<2, UInt64>

  public var uuid: uuid_t {
    get { Builtin.reinterpretCast(_storage) }
    set { _storage = Builtin.reinterpretCast(newValue) }
  }
}
