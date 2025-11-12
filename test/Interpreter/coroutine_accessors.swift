// RUN: %empty-directory(%t)

// RUN: split-file %s %t

// RUN: %target-swift-frontend \
// RUN:     %t/Library.swift   \
// RUN:     -enable-callee-allocated-coro-abi \
// RUN:     -emit-module       \
// RUN:     -module-name Library \
// RUN:     -parse-as-library  \
// RUN:     -enable-library-evolution \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -emit-module-path %t/Library.swiftmodule

// RUN: %target-build-swift-dylib(%t/%target-library-name(Library)) \
// RUN:     %t/Library.swift \
// RUN:     -Xfrontend -enable-callee-allocated-coro-abi \
// RUN:     -emit-module \
// RUN:     -enable-library-evolution \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -emit-module-path %t/Library.swiftmodule \
// RUN:     -module-name Library

// RUN: %target-swift-frontend \
// RUN:     %t/Executable.swift \
// RUN:     -enable-callee-allocated-coro-abi \
// RUN:     -c \
// RUN:     -parse-as-library \
// RUN:     -module-name Executable \
// RUN:     -I %t \
// RUN:     -o %t/Executable.o

// RUN: %target-build-swift \
// RUN:     %t/Executable.o \
// RUN:     -lLibrary \
// RUN:     -L %t \
// RUN:     %target-rpath(%t) \
// RUN:     -o %t/main

// RUN: %target-codesign %t/main %t/%target-library-name(Library)
// RUN: %target-run %t/main %t/%target-library-name(Library) | %FileCheck %s

// REQUIRES: swift_feature_CoroutineAccessors
// REQUIRES: executable_test

//--- Library.swift

@frozen
public struct Uniquint {
  public init(value: Int) { self.value = value }
  public var value: Int
}

var _i: Uniquint = Uniquint(value: 17)
open class Base {
  public init() {}
  open var i: Uniquint {
    read {
      yield _i
    }
    modify {
      yield &_i
    }
  }
}

public func unpack(_ b: Base) -> Int {
  return b.i.value
}

//--- Executable.swift

import Library

var j: Uniquint = Uniquint(value: 42)
class Derived : Base {
  override init() { super.init() }
  override var i: Uniquint {
    get {
      return Uniquint(value: 42)
    }
    set {
      j = newValue
    }
  }
}

@inline(never)
func myPrint<T>(_ t: T) {
  print(t)
}

@main struct M {
  static func main() {
    let b = Derived()
    let i = unpack(b)
    // CHECK: 42
    myPrint(i)
  }
}
