// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/ParameterizedProtocols.swiftinterface) %s -module-name ParameterizedProtocols -target %target-swift-5.1-abi-triple
// RUN: %target-swift-typecheck-module-from-interface(%t/ParameterizedProtocols.swiftinterface) -module-name ParameterizedProtocols -target %target-swift-5.1-abi-triple
// RUN: %FileCheck %s < %t/ParameterizedProtocols.swiftinterface

public protocol P<T> {
  associatedtype T
}

public protocol Q<T>: P {}

struct S<T>: Q {}


// CHECK-LABEL: public func returnsP() -> some ParameterizedProtocols.P
public func returnsP() -> some P { return S<Int>() }

// CHECK-LABEL: public func returnsPInt() -> some ParameterizedProtocols.P<Swift.Int>
public func returnsPInt() -> some P<Int> { return S<Int>() }

// CHECK-LABEL: public func returnsPT<T>(_: T) -> some ParameterizedProtocols.P<T>
public func returnsPT<T>(_: T) -> some P<T> { return S<T>() }

// CHECK-LABEL: public func returnsPArrayT<T>(_: T) -> some ParameterizedProtocols.P<Swift.Array<T>>
public func returnsPArrayT<T>(_: T) -> some P<Array<T>> { return S<Array<T>>() }


// CHECK-LABEL: public func returnsQ() -> some ParameterizedProtocols.Q
public func returnsQ() -> some Q { return S<Int>() }

// CHECK-LABEL: public func returnsQInt() -> some ParameterizedProtocols.Q<Swift.Int>
public func returnsQInt() -> some Q<Int> { return S<Int>() }

// CHECK-LABEL: public func returnsQT<T>(_: T) -> some ParameterizedProtocols.Q<T>
public func returnsQT<T>(_: T) -> some Q<T> { return S<T>() }

// CHECK-LABEL: public func returnsQArrayT<T>(_: T) -> some ParameterizedProtocols.Q<Swift.Array<T>>
public func returnsQArrayT<T>(_: T) -> some Q<Array<T>> { return S<Array<T>>() }
