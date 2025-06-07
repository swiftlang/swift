// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -disable-access-control \
// RUN:   -enable-experimental-feature LifetimeDependence

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_LifetimeDependence

// Test dependencies on the standard library Span APIs.

// =============================================================================
// Span-providing properties
// =============================================================================

extension UnsafeRawBufferPointer {
  @available(SwiftStdlib 6.2, *)
  public var storage: RawSpan {
    @lifetime(borrow self)
    get {
      let span = RawSpan(_unsafeBytes: self)
      return _overrideLifetime(span, borrowing: self)
    }
  }
}

@available(SwiftStdlib 6.2, *)
func read(_ span: RawSpan) {}

@available(SwiftStdlib 6.2, *)
func testUBPStorage(ubp: UnsafeRawBufferPointer) {
  // 'span' is valid within the lexical scope of variable 'ubp', which is the entire function.
  let span = ubp.storage
  read(span)
}

@available(SwiftStdlib 6.2, *)
@lifetime(borrow ubp)
func testUBPStorageReturn(ubp: UnsafeRawBufferPointer) -> RawSpan {
  // 'storage' can be returned since the function's return value also has a dependence on 'ubp'.
  return ubp.storage
}

@available(SwiftStdlib 6.2, *)
@lifetime(borrow ubp)
func testUBPStorageCopy(ubp: UnsafeRawBufferPointer) -> RawSpan {
  let localBuffer = ubp
  return localBuffer.storage // expected-error {{lifetime-dependent value escapes its scope}}
                             // expected-note  @-2{{it depends on the lifetime of variable 'localBuffer'}}
                             // expected-note  @-2{{this use causes the lifetime-dependent value to escape}}
}

@available(SwiftStdlib 6.2, *)
func testUBPStorageEscape(array: [Int64]) {
  var span = RawSpan()
  array.withUnsafeBytes {
    span = $0.storage // expected-error {{lifetime-dependent value escapes its scope}}
                      // expected-note  @-2{{it depends on the lifetime of argument '$0'}}
                      // expected-note  @-2{{this use causes the lifetime-dependent value to escape}}
  }
  read(span)
}
