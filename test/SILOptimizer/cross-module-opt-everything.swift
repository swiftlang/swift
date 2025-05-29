// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module -o %t/Module.swiftmodule %t/Module.swift -experimental-performance-annotations -enable-cmo-everything -wmo
// RUN: %target-swift-frontend -emit-ir %t/Main.swift -I%t -experimental-performance-annotations -enable-cmo-everything -wmo


// REQUIRES: swift_in_compiler
// REQUIRES: OS=macosx || OS=linux-gnu

// BEGIN Module.swift

@_semantics("optimize.no.crossmodule")
private func bar<R>(count: Int, _ body: (UnsafeMutableBufferPointer<Int>) -> R) -> R {
  let pointer = UnsafeMutablePointer<Int>(bitPattern: 42)!
  let inoutBufferPointer = UnsafeMutableBufferPointer(start: pointer, count: count)
  return body(inoutBufferPointer)
}

public func foo<R>(_ body: () -> R) -> R {
  bar(count: 10) { p in
    body()
  }
}

public func module_func() {
  foo {
    return 0
  }
}

public protocol P {
  func f()
}

public struct S1: P {
  public init() {}
  public func f() {}
}

public struct S2: P {
  public init() {}
  public func f() {}
}

public func open_existential(_ p: P) {
  p.f()
}

// BEGIN Main.swift

import Module

@_noLocks
public func test() {
  module_func()
}

@_noLocks
public func client_func() {
  foo {
    return 0
  }
}

public func client_func_2() {
  open_existential(S1())
  open_existential(S2())
}
