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
  public func call_pub(_ self: S, _ x: Swift::Int) -> Swift::Int {
  return self.pub(x)
}

  public func call_pub(_ self: C, _ x: Swift::Int) -> Swift::Int {
  return self.pub(x)
}
func call_ope(_ self: C, _ x: Swift::Int) -> Swift::Int {
  return self.ope(x)
}
