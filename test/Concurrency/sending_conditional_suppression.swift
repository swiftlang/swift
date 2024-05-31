// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -enable-upcoming-feature SendingArgsAndResults -swift-version 5 -enable-library-evolution -module-name test -emit-module -o %t/test.swiftmodule -emit-module-interface-path - %s | %FileCheck %s

public class NonSendableKlass {}

// CHECK-LABEL: #if compiler(>=5.3) && $SendingArgsAndResults
// CHECK-NEXT: public func transferArgTest(_ x: sending test.NonSendableKlass)
// CHECK-NEXT: #else
// CHECK-NEXT: public func transferArgTest(_ x: test.NonSendableKlass)
// CHECK-NEXT: #endif
public func transferArgTest(_ x: sending NonSendableKlass) {}

// CHECK-LABEL: #if compiler(>=5.3) && $SendingArgsAndResults
// CHECK-NEXT: public func transferResultTest() -> sending test.NonSendableKlass
// CHECK-NEXT: #else
// CHECK-NEXT: public func transferResultTest() -> test.NonSendableKlass
// CHECK-NEXT: #endif
public func transferResultTest() -> sending NonSendableKlass { fatalError() }

// CHECK-LABEL: #if compiler(>=5.3) && $SendingArgsAndResults
// CHECK-NEXT: public func transferArgAndResultTest(_ x: test.NonSendableKlass, _ y: sending test.NonSendableKlass, _ z: test.NonSendableKlass) -> sending test.NonSendableKlass
// CHECK-NEXT: #else
// CHECK-NEXT: public func transferArgAndResultTest(_ x: test.NonSendableKlass, _ y: test.NonSendableKlass, _ z: test.NonSendableKlass) -> test.NonSendableKlass
// CHECK-NEXT: #endif
public func transferArgAndResultTest(_ x: NonSendableKlass, _ y: sending NonSendableKlass, _ z: NonSendableKlass) -> sending NonSendableKlass { fatalError() }

// CHECK-LABEL: #if compiler(>=5.3) && $SendingArgsAndResults
// CHECK-NEXT: public func argEmbeddedInType(_ fn: (sending test.NonSendableKlass) -> ())
// CHECK-NEXT: #else
// CHECK-NEXT: public func argEmbeddedInType(_ fn: (test.NonSendableKlass) -> ())
// CHECK-NEXT: #endif
public func argEmbeddedInType(_ fn: (sending NonSendableKlass) -> ()) {}

// CHECK-LABEL: #if compiler(>=5.3) && $SendingArgsAndResults
// CHECK-NEXT: public func resultEmbeddedInType(_ fn: () -> sending test.NonSendableKlass)
// CHECK-NEXT: #else
// CHECK-NEXT: public func resultEmbeddedInType(_ fn: () -> test.NonSendableKlass)
// CHECK-NEXT: #endif
public func resultEmbeddedInType(_ fn: () -> sending NonSendableKlass) {}

// CHECK-LABEL: #if compiler(>=5.3) && $SendingArgsAndResults
// CHECK-NEXT: public func argAndResultEmbeddedInType(_ fn: (test.NonSendableKlass, sending test.NonSendableKlass, test.NonSendableKlass) -> sending test.NonSendableKlass)
// CHECK-NEXT: #else
// CHECK-NEXT: public func argAndResultEmbeddedInType(_ fn: (test.NonSendableKlass, test.NonSendableKlass, test.NonSendableKlass) -> test.NonSendableKlass)
// CHECK-NEXT: #endif
public func argAndResultEmbeddedInType(_ fn: (NonSendableKlass, sending NonSendableKlass, NonSendableKlass) -> sending NonSendableKlass) {}

public class TestInKlass {
  // CHECK-LABEL: #if compiler(>=5.3) && $SendingArgsAndResults
  // CHECK-NEXT: public func testKlassArg(_ x: sending test.NonSendableKlass)
  // CHECK-NEXT: #else
  // CHECK-NEXT: public func testKlassArg(_ x: test.NonSendableKlass)
  // CHECK-NEXT: #endif
  public func testKlassArg(_ x: sending NonSendableKlass) { fatalError() }

  // CHECK-LABEL: #if compiler(>=5.3) && $SendingArgsAndResults
  // CHECK-NEXT: public func testKlassResult() -> sending test.NonSendableKlass
  // CHECK-NEXT: #else
  // CHECK-NEXT: public func testKlassResult() -> test.NonSendableKlass
  // CHECK-NEXT: #endif
  public func testKlassResult() -> sending NonSendableKlass { fatalError() }

  // CHECK-LABEL: #if compiler(>=5.3) && $SendingArgsAndResults
  // CHECK-NEXT: public func testKlassArgAndResult(_ x: test.NonSendableKlass, _ y: sending test.NonSendableKlass, z: test.NonSendableKlass) -> sending test.NonSendableKlass
  // CHECK-NEXT: #else
  // CHECK-NEXT: public func testKlassArgAndResult(_ x: test.NonSendableKlass, _ y: test.NonSendableKlass, z: test.NonSendableKlass) -> test.NonSendableKlass
  // CHECK-NEXT: #endif
  public func testKlassArgAndResult(_ x: NonSendableKlass, _ y: sending NonSendableKlass, z: NonSendableKlass) -> sending NonSendableKlass { fatalError() }
}

public struct TestInStruct {
  // CHECK-LABEL: #if compiler(>=5.3) && $SendingArgsAndResults
  // CHECK-NEXT: public func testKlassArg(_ x: sending test.NonSendableKlass)
  // CHECK-NEXT: #else
  // CHECK-NEXT: public func testKlassArg(_ x: test.NonSendableKlass)
  // CHECK-NEXT: #endif
  public func testKlassArg(_ x: sending NonSendableKlass) { fatalError() }

  // CHECK-LABEL: #if compiler(>=5.3) && $SendingArgsAndResults
  // CHECK-NEXT: public func testKlassResult() -> sending test.NonSendableKlass
  // CHECK-NEXT: #else
  // CHECK-NEXT: public func testKlassResult() -> test.NonSendableKlass
  // CHECK-NEXT: #endif
  public func testKlassResult() -> sending NonSendableKlass { fatalError() }

  // CHECK-LABEL: #if compiler(>=5.3) && $SendingArgsAndResults
  // CHECK-NEXT: public func testKlassArgAndResult(_ x: test.NonSendableKlass, _ y: sending test.NonSendableKlass, z: test.NonSendableKlass) -> sending test.NonSendableKlass
  // CHECK-NEXT: #else
  // CHECK-NEXT: public func testKlassArgAndResult(_ x: test.NonSendableKlass, _ y: test.NonSendableKlass, z: test.NonSendableKlass) -> test.NonSendableKlass
  // CHECK-NEXT: #endif
  public func testKlassArgAndResult(_ x: NonSendableKlass, _ y: sending NonSendableKlass, z: NonSendableKlass) -> sending NonSendableKlass { fatalError() }

  // CHECK-LABEL: #if compiler(>=5.3) && $SendingArgsAndResults
  // CHECK-NEXT: public func testFunctionArg(_ x: () -> sending test.NonSendableKlass)
  // CHECK-NEXT: #else
  // CHECK-NEXT: public func testFunctionArg(_ x: () -> test.NonSendableKlass)
  // CHECK-NEXT: #endif  
  public func testFunctionArg(_ x: () -> sending NonSendableKlass) { fatalError() }

  // CHECK-LABEL: #if compiler(>=5.3) && $SendingArgsAndResults
  // CHECK-NEXT: public func testFunctionResult() -> (() -> sending test.NonSendableKlass)
  // CHECK-NEXT: #else
  // CHECK-NEXT: public func testFunctionResult() -> (() -> test.NonSendableKlass)
  // CHECK-NEXT: #endif  
  public func testFunctionResult() -> (() -> sending NonSendableKlass) { fatalError() }

  // CHECK-LABEL: #if compiler(>=5.3) && $SendingArgsAndResults
  // CHECK-NEXT: @usableFromInline
  // CHECK-NEXT: internal func testUsableFromInlineKlassArg(_ x: sending test.NonSendableKlass)
  // CHECK-NEXT: #else
  // CHECK-NEXT: @usableFromInline
  // CHECK-NEXT: internal func testUsableFromInlineKlassArg(_ x: test.NonSendableKlass)
  // CHECK-NEXT: #endif
  @usableFromInline func testUsableFromInlineKlassArg(_ x: sending NonSendableKlass) { fatalError() }

  // CHECK-LABEL: #if compiler(>=5.3) && $SendingArgsAndResults
  // CHECK-NEXT: @usableFromInline
  // CHECK-NEXT: internal func testUsableFromInlineKlassResult() -> sending test.NonSendableKlass
  // CHECK-NEXT: #else
  // CHECK-NEXT: @usableFromInline
  // CHECK-NEXT: internal func testUsableFromInlineKlassResult() -> test.NonSendableKlass
  // CHECK-NEXT: #endif
  @usableFromInline
  func testUsableFromInlineKlassResult() -> sending NonSendableKlass { fatalError() }

  // CHECK-LABEL: #if compiler(>=5.3) && $SendingArgsAndResults
  // CHECK-NEXT: @usableFromInline
  // CHECK-NEXT: internal func testUsableFromInlineKlassArgAndResult(_ x: test.NonSendableKlass, _ y: sending test.NonSendableKlass, z: test.NonSendableKlass) -> sending test.NonSendableKlass
  // CHECK-NEXT: #else
  // CHECK-NEXT: @usableFromInline
  // CHECK-NEXT: internal func testUsableFromInlineKlassArgAndResult(_ x: test.NonSendableKlass, _ y: test.NonSendableKlass, z: test.NonSendableKlass) -> test.NonSendableKlass
  // CHECK-NEXT: #endif
  @usableFromInline
  func testUsableFromInlineKlassArgAndResult(_ x: NonSendableKlass, _ y: sending NonSendableKlass, z: NonSendableKlass) -> sending NonSendableKlass { fatalError() }

  // CHECK-LABEL: #if compiler(>=5.3) && $SendingArgsAndResults
  // CHECK-NEXT: @usableFromInline
  // CHECK-NEXT: internal func testUsableFromInlineFunctionArg(_ x: () -> sending test.NonSendableKlass)
  // CHECK-NEXT: #else
  // CHECK-NEXT: @usableFromInline
  // CHECK-NEXT: internal func testUsableFromInlineFunctionArg(_ x: () -> test.NonSendableKlass)
  // CHECK-NEXT: #endif
  @usableFromInline
  func testUsableFromInlineFunctionArg(_ x: () -> sending NonSendableKlass) { fatalError() }

  // CHECK-LABEL: #if compiler(>=5.3) && $SendingArgsAndResults
  // CHECK-NEXT: @usableFromInline
  // CHECK-NEXT: internal func testUsableFromInlineFunctionResult() -> (() -> sending test.NonSendableKlass)
  // CHECK-NEXT: #else
  // CHECK-NEXT: @usableFromInline
  // CHECK-NEXT: internal func testUsableFromInlineFunctionResult() -> (() -> test.NonSendableKlass)
  // CHECK-NEXT: #endif
  @usableFromInline
  func testUsableFromInlineFunctionResult() -> (() -> sending NonSendableKlass) { fatalError() }
}

// Make sure that we emit compiler(>= 5.3) when emitting the suppressing check
// to make sure we do not fail if we fail to parse sending in the if block.

// CHECK: #if compiler(>=5.3) && $OptionalIsolatedParameters && $ExpressionMacroDefaultArguments
// CHECK-NEXT: #if compiler(>=5.3) && $SendingArgsAndResults
// CHECK-NEXT: @available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
// CHECK-NEXT: @backDeployed(before: macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, visionOS 9999)
// CHECK-NEXT: @inlinable public func withCheckedContinuation<T>(isolation:
@available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
@backDeployed(before: macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, visionOS 9999)
@inlinable public func withCheckedContinuation<T>(
  isolation: isolated (any _Concurrency.Actor)? = #isolation,
  function: String = #function,
  _ body: (_Concurrency.CheckedContinuation<T, Swift.Never>) -> Swift.Void
) async -> sending T {
  return await withUnsafeContinuation {
    body(CheckedContinuation(continuation: $0, function: function))
  }
}
