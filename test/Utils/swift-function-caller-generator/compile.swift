// REQUIRES: swift_feature_LifetimeDependence
// REQUIRES: swift_feature_Lifetimes

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift -emit-module %t/in.swift -enable-experimental-feature Lifetimes -enable-experimental-feature LifetimeDependence -o %t/%target-library-name(Test) -module-name Test -emit-library

// RUN: %target-swift-emit-module-interface(%t/Test.swiftinterface) %t/in.swift -enable-experimental-feature Lifetimes -enable-experimental-feature LifetimeDependence
// RUN: %swift-function-caller-generator Test %t/Test.swiftinterface > %t/out.swift
// RUN: %diff %t/out.swift %t/out.expected

// RUN: %target-swift-frontend-verify -typecheck -strict-memory-safety %t/out.swift -I %t -enable-experimental-feature Lifetimes -enable-experimental-feature LifetimeDependence

//--- in.swift
public func foo(x: Int) -> Int {
  return x
}

public func bar(_ y: UnsafePointer<CInt>) {}

@_lifetime(borrow z)
public func baz(_ z: Span<CInt>) -> Span<CInt> {
  return z
}

@_lifetime(`func`: copy `func`)
public func qux(_ func: inout MutableSpan<CInt>) {}

public struct S {
  mutating func m(_ x: Int) -> Int {
    return x
  }
  public func pub(_ x: Int) -> Int {
    return x
  }
  private func priv(_ x: Int) -> Int {
    return x
  }
}

public class C {
  public func pub(_ x: Int) -> Int {
    return x
  }
  private func priv(_ x: Int) -> Int {
    return x
  }
  open func ope(_ x: Int) -> Int {
    return x
  }
  public final class func clas(x: Int) -> Int {
    return x
  }
  open class func clas2(x: Int) -> Int {
    return x
  }
}

@available(*, unavailable)
public func unavailable(x: Int) -> Int { return x }

@available(*, unavailable)
public struct Sunavailable {
  public func munavailable(x: Int) -> Int { return x }
}

public class Base {
  public func foo() {}
  public class func foo() {}
  public func bar() {}
}
public class Derived: Base {
  override public func foo() {}
  override public class func foo() {}
  public override func bar() {}
}

//--- out.expected
import Test


public func call_foo(x: Swift::Int) -> Swift::Int {
  return foo(x: x)
}

public func call_bar(_ y: Swift::UnsafePointer<Swift::CInt>) {
  return unsafe bar(y)
}

#if compiler(>=5.3) && $LifetimeDependence
#if compiler(>=5.3) && $Lifetimes
@_lifetime(borrow z)
public func call_baz(_ z: Swift::Span<Swift::CInt>) -> Swift::Span<Swift::CInt> {
  return baz(z)
}

#else
@lifetime(borrow z)
public func call_baz(_ z: Swift::Span<Swift::CInt>) -> Swift::Span<Swift::CInt> {
  return baz(z)
}

#endif
#endif
#if compiler(>=5.3) && $LifetimeDependence
#if compiler(>=5.3) && $Lifetimes
@_lifetime(`func`: copy `func`)
public func call_qux(_ func: inout Swift::MutableSpan<Swift::CInt>) {
  return qux(&`func`)
}

#else
@lifetime(`func`: copy `func`)
public func call_qux(_ func: inout Swift::MutableSpan<Swift::CInt>) {
  return qux(&`func`)
}

#endif
#endif
public extension S {
  func call_pub_S(_ x: Swift::Int) -> Swift::Int {
    return pub(x)
  }
}

public extension C {
  final func call_pub_C(_ x: Swift::Int) -> Swift::Int {
    return pub(x)
  }
  final func call_ope_C(_ x: Swift::Int) -> Swift::Int {
    return ope(x)
  }
  final func call_clas_C_classmethod(x: Swift::Int) -> Swift::Int {
    return C.clas(x: x)
  }
  final func call_clas2_C_classmethod(x: Swift::Int) -> Swift::Int {
    return C.clas2(x: x)
  }
}

public extension Base {
  final func call_foo_Base() {
    return foo()
  }
  final func call_foo_Base_classmethod() {
    return Base.foo()
  }
  final func call_bar_Base() {
    return bar()
  }
}

public extension Derived {
  final func call_foo_Derived() {
    return foo()
  }
  final func call_foo_Derived_classmethod() {
    return Derived.foo()
  }
  final func call_bar_Derived() {
    return bar()
  }
}
