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
}
