// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -target %target-swift-6.2-abi-triple \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature Lifetimes
// RUN: %target-swift-frontend -enable-lifetime-resolution %s -emit-sil \
// RUN:   -target %target-swift-6.2-abi-triple \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature Lifetimes

// REQUIRES: swift_feature_Lifetimes

// Test dependencies on the standard library Span APIs.

// =============================================================================
// Span-providing properties
// =============================================================================

extension UnsafeRawBufferPointer {
  public var storage: RawSpan {
    @_lifetime(borrow self)
    get {
      let span = RawSpan(_unsafeBytes: self)
      return _overrideLifetime(span, borrowing: self)
    }
  }
}

func read(_ span: RawSpan) {}

func testUBPStorage(ubp: UnsafeRawBufferPointer) {
  // 'span' is valid within the lexical scope of variable 'ubp', which is the entire function.
  let span = ubp.storage
  read(span)
}

@_lifetime(borrow ubp)
func testUBPStorageReturn(ubp: UnsafeRawBufferPointer) -> RawSpan {
  // 'storage' can be returned since the function's return value also has a dependence on 'ubp'.
  return ubp.storage
}

@_lifetime(borrow ubp)
func testUBPStorageCopy(ubp: UnsafeRawBufferPointer) -> RawSpan {
  let localBuffer = ubp
  return localBuffer.storage // expected-error {{lifetime-dependent value escapes its scope}}
                             // expected-note  @-2{{it depends on the lifetime of variable 'localBuffer'}}
                             // expected-note  @-2{{this use causes the lifetime-dependent value to escape}}
}

func testUBPStorageEscape(array: [Int64]) {
  var span = RawSpan()  // expected-error{{lifetime-dependent variable 'span' escapes its scope}}
  array.withUnsafeBytes {  // expected-note{{it depends on the lifetime of argument '$0'}}
    span = $0.storage
  } // expected-note{{this use causes the lifetime-dependent value to escape}}
  read(span)
}

// =============================================================================
// Optional.span and Optional.mutableSpan
// =============================================================================

@_lifetime(borrow o)
func escapingOptionalSpan(_ o: Int?) -> Span<Int> { // expected-note {{it depends on the lifetime of argument 'o'}}
  return o.span // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1 {{this use causes the lifetime-dependent value to escape}}
}

func readInts(_ span: Span<Int>) {}

func conflictingMutation() {
  var o: Int? = 1
  let span = o.span // expected-note {{conflicting access is here}}
  o = nil // expected-error {{overlapping accesses to 'o'}}
  readInts(span)
}

func writeInts(_ span: inout MutableSpan<Int>) {}

func conflictingBorrow() {
  var o: Int? = 1
  var span = o.mutableSpan // expected-error {{overlapping accesses to 'o'}}
  _ = o.span // expected-note {{conflicting access is here}}
  writeInts(&span)
}
