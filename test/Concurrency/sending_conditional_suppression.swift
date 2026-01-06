// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-upcoming-feature SendingArgsAndResults -swift-version 5 -enable-library-evolution -module-name test -emit-module -o %t/test.swiftmodule -emit-module-interface-path - -target %target-swift-5.1-abi-triple %s | %FileCheck %s

// REQUIRES: swift_feature_SendingArgsAndResults
// REQUIRES: asserts

public class NonSendableKlass {}

// CHECK: public func transferArgTest(_ x: sending test.NonSendableKlass)
public func transferArgTest(_ x: sending NonSendableKlass) {}

// CHECK-NEXT: public func transferResultTest() -> sending test.NonSendableKlass
public func transferResultTest() -> sending NonSendableKlass { fatalError() }

// CHECK-NEXT: public func transferArgAndResultTest(_ x: test.NonSendableKlass, _ y: sending test.NonSendableKlass, _ z: test.NonSendableKlass) -> sending test.NonSendableKlass
public func transferArgAndResultTest(_ x: NonSendableKlass, _ y: sending NonSendableKlass, _ z: NonSendableKlass) -> sending NonSendableKlass { fatalError() }

// CHECK-NEXT: public func argEmbeddedInType(_ fn: (sending test.NonSendableKlass) -> ())
public func argEmbeddedInType(_ fn: (sending NonSendableKlass) -> ()) {}

// CHECK-NEXT: public func resultEmbeddedInType(_ fn: () -> sending test.NonSendableKlass)
public func resultEmbeddedInType(_ fn: () -> sending NonSendableKlass) {}

// CHECK-NEXT: public func argAndResultEmbeddedInType(_ fn: (test.NonSendableKlass, sending test.NonSendableKlass, test.NonSendableKlass) -> sending test.NonSendableKlass)
public func argAndResultEmbeddedInType(_ fn: (NonSendableKlass, sending NonSendableKlass, NonSendableKlass) -> sending NonSendableKlass) {}

// CHECK-LABEL: public class TestInKlass
public class TestInKlass {
  // CHECK-NEXT: public func testKlassArg(_ x: sending test.NonSendableKlass)
  public func testKlassArg(_ x: sending NonSendableKlass) { fatalError() }

  // CHECK-NEXT: public func testKlassResult() -> sending test.NonSendableKlass
  public func testKlassResult() -> sending NonSendableKlass { fatalError() }

  // CHECK-NEXT: public func testKlassArgAndResult(_ x: test.NonSendableKlass, _ y: sending test.NonSendableKlass, z: test.NonSendableKlass) -> sending test.NonSendableKlass
  public func testKlassArgAndResult(_ x: NonSendableKlass, _ y: sending NonSendableKlass, z: NonSendableKlass) -> sending NonSendableKlass { fatalError() }
}

// CHECK-LABEL: public struct TestInStruct
public struct TestInStruct {
  // CHECK-NEXT: public func testKlassArg(_ x: sending test.NonSendableKlass)
  public func testKlassArg(_ x: sending NonSendableKlass) { fatalError() }

  // CHECK-NEXT: public func testKlassResult() -> sending test.NonSendableKlass
  public func testKlassResult() -> sending NonSendableKlass { fatalError() }

  // CHECK-NEXT: public func testKlassArgAndResult(_ x: test.NonSendableKlass, _ y: sending test.NonSendableKlass, z: test.NonSendableKlass) -> sending test.NonSendableKlass
  public func testKlassArgAndResult(_ x: NonSendableKlass, _ y: sending NonSendableKlass, z: NonSendableKlass) -> sending NonSendableKlass { fatalError() }

  // CHECK-NEXT: public func testFunctionArg(_ x: () -> sending test.NonSendableKlass)
  public func testFunctionArg(_ x: () -> sending NonSendableKlass) { fatalError() }

  // CHECK-NEXT: public func testFunctionResult() -> () -> sending test.NonSendableKlass
  public func testFunctionResult() -> (() -> sending NonSendableKlass) { fatalError() }

  // CHECK-NEXT: @usableFromInline
  // CHECK-NEXT: internal func testUsableFromInlineKlassArg(_ x: sending test.NonSendableKlass)
  @usableFromInline func testUsableFromInlineKlassArg(_ x: sending NonSendableKlass) { fatalError() }

  // CHECK-NEXT: @usableFromInline
  // CHECK-NEXT: internal func testUsableFromInlineKlassResult() -> sending test.NonSendableKlass
  @usableFromInline
  func testUsableFromInlineKlassResult() -> sending NonSendableKlass { fatalError() }

  // CHECK-NEXT: @usableFromInline
  // CHECK-NEXT: internal func testUsableFromInlineKlassArgAndResult(_ x: test.NonSendableKlass, _ y: sending test.NonSendableKlass, z: test.NonSendableKlass) -> sending test.NonSendableKlass
  @usableFromInline
  func testUsableFromInlineKlassArgAndResult(_ x: NonSendableKlass, _ y: sending NonSendableKlass, z: NonSendableKlass) -> sending NonSendableKlass { fatalError() }

  // CHECK-NEXT: @usableFromInline
  // CHECK-NEXT: internal func testUsableFromInlineFunctionArg(_ x: () -> sending test.NonSendableKlass)
  @usableFromInline
  func testUsableFromInlineFunctionArg(_ x: () -> sending NonSendableKlass) { fatalError() }

  // CHECK-NEXT: @usableFromInline
  // CHECK-NEXT: internal func testUsableFromInlineFunctionResult() -> () -> sending test.NonSendableKlass
  @usableFromInline
  func testUsableFromInlineFunctionResult() -> (() -> sending NonSendableKlass) { fatalError() }

  // CHECK-NEXT: public var publicVarFieldFunctionArg: (sending test.NonSendableKlass) -> ()
  public var publicVarFieldFunctionArg: (sending NonSendableKlass) -> ()

  // CHECK-NEXT: @usableFromInline
  // CHECK-NEXT: internal var internalVarFieldFunctionArg: (sending test.NonSendableKlass) -> ()
  @usableFromInline
  var internalVarFieldFunctionArg: (sending NonSendableKlass) -> ()

  // CHECK-NEXT: public let publicLetFieldFunctionArg: (sending test.NonSendableKlass) -> ()
  public let publicLetFieldFunctionArg: (sending NonSendableKlass) -> ()

  // CHECK-NEXT: @usableFromInline
  // CHECK-NEXT: internal let internalLetFieldFunctionArg: (sending test.NonSendableKlass) -> ()
  @usableFromInline
  let internalLetFieldFunctionArg: (sending NonSendableKlass) -> ()

  // CHECK-NEXT: @usableFromInline
  // CHECK-NEXT: internal init(_ x: Swift.Int, transformWithResult: @escaping () async throws -> sending test.NonSendableKlass)
  @usableFromInline
  internal init(_ x: Int, transformWithResult: @escaping () async throws -> sending NonSendableKlass) { fatalError() }
}

// CHECK-LABEL: @inlinable public func withCheckedContinuation<T>(isolation: isolated (any _Concurrency.Actor)? = #isolation, function: Swift.String = #function, _ body: (_Concurrency.CheckedContinuation<T, Swift.Never>) -> Swift.Void) async -> sending T {
// CHECK-NEXT:  fatalError()
// CHECK-NEXT: }
@inlinable public func withCheckedContinuation<T>(
  isolation: isolated (any _Concurrency.Actor)? = #isolation,
  function: String = #function,
  _ body: (_Concurrency.CheckedContinuation<T, Swift.Never>) -> Swift.Void
) async -> sending T {
  fatalError()
}

// CHECK-LABEL: public var publicGlobal: (sending test.NonSendableKlass) -> ()
public var publicGlobal: (sending NonSendableKlass) -> () = { x in fatalError() }

// CHECK: @usableFromInline
// CHECK-NEXT: internal var usableFromInlineGlobal: (sending test.NonSendableKlass) -> ()
@usableFromInline
internal var usableFromInlineGlobal: (sending NonSendableKlass) -> () = { x in fatalError() }
