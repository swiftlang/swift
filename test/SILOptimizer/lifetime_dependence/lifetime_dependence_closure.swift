// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -target %target-swift-6.2-abi-triple \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature Lifetimes

// RUN: %if swift_feature_OnceFunctions %{ %target-swift-frontend %s -emit-sil -enable-experimental-feature OnceFunctions \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -target %target-swift-6.2-abi-triple \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -verify-additional-prefix once- \
// RUN:   -D ONCE_FUNCTIONS %}

// REQUIRES: swift_feature_Lifetimes

struct NCInt: ~Copyable {
  var value: Int

  init(_ value: Int) { self.value = value }
}

struct NEInt : ~Escapable {
  let value: Int

  @_lifetime(borrow borrowed)
  init(borrowed: borrowing NCInt) {
    self.value = borrowed.value
  }
}

extension NCInt {
  var neInt: NEInt {
    borrowing get {
      NEInt(borrowed: self)
    }
  }
}

func ncint_get_neint_mutable_local() {
  var ncInt = NCInt(731)
  do {
    // Begin read access ncInt
    var neInt = ncInt.neInt

    // Capture neInt in a nonescapable closure.
    assert(ncInt.value == neInt.value)
    // End read access ncInt

    // Begin read access ncInt
    // Fully reassign neInt
    neInt = ncInt.neInt
    // Destroy neInt
    // End read access ncInt
  }
  ncInt.value = 1
}

struct NE: ~Escapable {
  let p: UnsafeRawPointer?

  public func condition() -> Bool {
    return true
  }
}

func getLocalPointer() -> UnsafeRawPointer? {
  return nil
}

func takePicker(picker: @_lifetime(copy ne0, copy ne1) (_ ne0: NE, _ ne1: NE) -> NE) {
    let x = NE(p: getLocalPointer())
    let y = NE(p: getLocalPointer())
    _ = picker(x, y)
}

func takeOnePicker(picker: @_lifetime(copy ne0) (_ ne0: NE, NE) -> NE) {
    let x = NE(p: getLocalPointer())
    let y = NE(p: getLocalPointer())
    _ = picker(x, y)
}

func takeCapturePicker(picker: /* DEFAULT: @_lifetime(captures, copy ne0, copy ne1) */ (_ ne0: NE, _ ne1: NE) -> NE) {
    let x = NE(p: getLocalPointer())
    let y = NE(p: getLocalPointer())
    _ = picker(x, y)
}

func takeMutator(mutator: @_lifetime(ne: copy ne) (_ ne: inout NE) -> ()) {
  var ne = NE(p: getLocalPointer())
  mutator(&ne)
  let _ = ne
}

func takeCaptureMutator(mutator: @_lifetime(ne: captures, copy ne) (_ ne: inout NE) -> ()) {
  var ne = NE(p: getLocalPointer())
  mutator(&ne)
  let _ = ne
}

func testClosureLifetimes(cond: Bool) {
  takePicker { ne0, ne1 in ne0 } // OK
  takePicker { ne0, ne1 in ne1 } // OK

  takePicker { ne0, ne1 in if cond { return ne0 } else { return ne1 } } // OK

  // Several of the following error cases should start passing with no source
  // changes once closure context lifetimes are implemented. Some may require
  // explicit lifetime annotations; for those, add an extra test case with the
  // annotation.
  
  // OK, ne2 is captured but its lifetime is not related
  let ne2 = NE(p: getLocalPointer())
  takePicker { ne0, ne1 in
    if ne2.condition() { return ne0 } else { return ne1 }
  }

  let ne3 = NE(p: getLocalPointer())
  // expected-error@-1{{lifetime-dependent variable 'ne3' escapes its scope}}
  // expected-note@-2{{it depends on a closure capture; this is not yet supported}}
  takePicker { ne0, ne1 in ne3 }
  // expected-note@-1{{this use causes the lifetime-dependent value to escape}}

  var ne4 = NE(p: getLocalPointer()) // expected-error{{lifetime-dependent variable 'ne4' escapes its scope}}
  takePicker { ne0, ne1 in
    ne4 = NE(p: getLocalPointer())   // expected-note{{it depends on the lifetime of this parent value}}
    return ne0   // expected-note{{this use causes the lifetime-dependent value to escape}}
  }
  let _ = ne4

  let ne5 = NE(p: getLocalPointer()) // expected-note{{it depends on a closure capture; this is not yet supported}}
  takePicker { ne0, ne1 in
    let ne = ne5 // expected-error{{lifetime-dependent variable 'ne' escapes its scope}}
    return ne    // expected-note{{this use causes the lifetime-dependent value to escape}}
  }

  var ne6 = NE(p: getLocalPointer())           // expected-error{{lifetime-dependent variable 'ne6' escapes its scope}}
  takePicker { ne0, ne1 in // expected-note{{it depends on the lifetime of argument 'ne1'}}
    ne6 = ne1
    return ne0             // expected-note{{this use causes the lifetime-dependent value to escape}}
  }
  let _ = ne6

  takeMutator { ne0 in // OK
    let neLocal = ne0
    ne0 = neLocal
  }

  let ne8 = NE(p: getLocalPointer())       // expected-note{{it depends on a closure capture; this is not yet supported}}
  takeMutator { ne0 in // expected-error{{lifetime-dependent variable 'ne0' escapes its scope}}
    ne0 = ne8
  }                    // expected-note{{this use causes the lifetime-dependent value to escape}}


  takeOnePicker { ne0, ne1 in ne0 } // OK
  takeOnePicker { ne0, ne1 in ne1 }
  // expected-error@-1{{lifetime-dependent variable 'ne1' escapes its scope}}
  // expected-note@-2{{it depends on the lifetime of argument 'ne1'}}
  // expected-note@-3{{this use causes the lifetime-dependent value to escape}}

  takeOnePicker { ne0, ne1 in
                 // expected-error@-1{{lifetime-dependent variable 'ne1' escapes its scope}}
                 // expected-note@-2{{it depends on the lifetime of argument 'ne1'}}
    if cond {
      return ne0
    } else {
      return ne1 // expected-note{{this use causes the lifetime-dependent value to escape}}
    }
  }

  // Callbacks with captures dependencies.

  takeCapturePicker { ne0, ne1 in ne0 } // OK
  takeCapturePicker { ne0, ne1 in ne1 } // OK
  let ne9 = NE(p: getLocalPointer())
  takeCapturePicker { ne0, ne1 in ne9 } // OK


  takeCaptureMutator { ne0 in // OK
    let neLocal = ne0
    ne0 = neLocal
  }

  let ne10 = NE(p: getLocalPointer())
  takeCaptureMutator { ne0 in // OK
    ne0 = ne10
  }
}

// MARK: Methods with lifetime dependencies
func transfer(ne: NE) -> NE { ne }
struct S {
  static func staticTransfer(ne: NE) -> NE { ne }
  @_lifetime(borrow ne)
  static func staticTransferBorrowingNE(ne: NE) -> NE { ne }
  func transfer(ne: NE) -> NE { ne }
  @_lifetime(borrow self, copy ne)
  func transferDependingOnSelf(ne: NE) -> NE { ne }
}

do {
  typealias Transfer = @_lifetime(copy ne) (_ ne: NE) -> NE
  let _ = transfer // OK
  let _: (NE) -> NE = transfer // OK
  let _: Transfer = transfer // OK
  let _: Transfer = S.staticTransfer // OK, method is static

  // Non-static methods are not supported yet
  let s = S()
  let _: Transfer = s.transfer
  // expected-error@-1{{lifetime-dependent value escapes its scope}}
  // expected-note@-2{{it depends on the lifetime of argument 'ne'}}
  // expected-note@-3{{this use causes the lifetime-dependent value to escape}}
  let _: Transfer = s.transferDependingOnSelf
  // expected-error@-1{{lifetime-dependent value escapes its scope}}
  // expected-note@-2{{it depends on the lifetime of argument 'ne'}}
  // expected-note@-3{{this use causes the lifetime-dependent value to escape}}
  // expected-error@-4{{lifetime-dependent value escapes its scope}}
  // expected-note@-5{{it depends on a closure capture; this is not yet supported}}
  // expected-note@-6{{this use causes the lifetime-dependent value to escape}}
}

// ~Escapable function types
@_lifetime(borrow body)
func borrowClosureNE(body: /* @_lifetime(captures) */ () -> NE) -> NE {
  body()
}

func callBorrowClosureNE(ne: NE) -> NE {
  // expected-error@-1{{lifetime-dependent variable 'ne' escapes its scope}}
  // expected-note@-2{{it depends on the lifetime of argument 'ne'}}
  let neo = borrowClosureNE { ne }
  // expected-error@-1{{lifetime-dependent variable 'neo' escapes its scope}}
  // expected-note@-2{{this use causes the lifetime-dependent value to escape}}
  // expected-note@-3{{it depends on the lifetime of this parent value}}

  return neo // expected-note{{this use causes the lifetime-dependent value to escape}}
}

@_lifetime(copy body)
func copyClosureNE(body: () -> NE) -> NE {
  body()
}

func callCopyClosureNE(ne: NE) -> NE {
  let neo = copyClosureNE { ne }
  return neo
}

// =============================================================================
// easily recognizable patterns involving the capture of an array's span

func closure_array_int(_: [Int], _ f: ()->Int) -> Int { f() }

func testCaptureSpanReturnElement(array: consuming [Int]) -> Int {
  let span = array.span
  return closure_array_int(array) { span[0] }
}

func closure_consuming_array_int(_: consuming [Int], _ f: ()->Int) -> Int { f() }

func testConsumeAndCaptureSpanReturnElement(array: consuming [Int]) -> Int {
  // expected-error@-1{{'array' used after consume}}
  let span = array.span // expected-note{{conflicting access is here}}
  return closure_consuming_array_int(array) { span[0] }
  // expected-error@-1{{overlapping accesses to 'array', but deinitialization requires exclusive access}}
  // expected-note@-2{{consumed here}}
}

func closure_span(_ f: ()->Span<Int>) -> Span<Int> { f() }

func testArgSpanCaptureReturnSpan(array: [Int]) -> Span<Int> {
  let span = array.span
  return closure_span { span }
}

@_lifetime(immortal)
func testConsumingArgSpanCaptureReturnSpan(array: consuming [Int]) -> Span<Int> {
  let span = array.span // expected-note{{it depends on this scoped access to variable 'array'}}
  return closure_span { span }
  // expected-error@-1{{lifetime-dependent value escapes its scope}}
  // expected-note@-2{{this use causes the lifetime-dependent value to escape}}
}

@_lifetime(immortal)
func testLocalSpanCaptureReturnSpan() -> Span<Int> {
  let array = [1,2,3] // expected-note{{it depends on the lifetime of variable 'array'}}
  let span = array.span
  return closure_span { span }
  // expected-error@-1{{lifetime-dependent value escapes its scope}}
  // expected-note@-2{{this use causes the lifetime-dependent value to escape}}
}

// rdar://189095063 (non-escaping closure capture of a value that borrows a getter temporary)
//
// An address-only ~Escapable value that depends on a getter's temporary result is captured by a non-escaping closure.
final class AnyOwner {
  var value: Any = 1
}

struct AnyDependent : ~Escapable {
  let value: Any

  @_lifetime(borrow owner)
  init(owner: borrowing AnyOwner) { value = owner.value }
}

extension AnyOwner {
  var dependent: AnyDependent {
    @_lifetime(borrow self)
    get { AnyDependent(owner: self) }
  }
}

func runClosure(_ body: () -> Void) { body() }

struct AnyOwnerClient {
  let storedOwner: AnyOwner
  var computedOwner: AnyOwner { storedOwner }

  func capturedThroughComputed() {
    let d = computedOwner.dependent
    runClosure { _ = d.value }
  }
}

// rdar://191117232 (https://github.com/swiftlang/swift/issues/93665): storing a dependent value into a captured
// variable is an escape even if the closure only exits by throwing, because the caller may read the variable after
// catching the error.
struct ThrowStop: Error {}

func lendSpan<E: Error>(_ array: [Int], _ body: (Span<Int>) throws(E) -> Void) throws(E) {
  try body(array.span)
}

func lendSpanUntyped(_ array: [Int], _ body: (Span<Int>) throws -> Void) rethrows {
  try body(array.span)
}

func storeThenThrowTyped(array: [Int]) -> Int {
  var kept: Span<Int>? = nil // expected-error{{lifetime-dependent variable 'kept' escapes its scope}}
  do {
    try lendSpan(array) { (span) throws(ThrowStop) in // expected-note{{it depends on the lifetime of argument 'span'}}
      kept = span
      throw ThrowStop() // expected-note{{this use causes the lifetime-dependent value to escape}}
    }
  } catch {}
  return kept![0]
}

func storeThenThrowUntyped(array: [Int]) -> Int {
  var kept: Span<Int>? = nil // expected-error{{lifetime-dependent variable 'kept' escapes its scope}}
  do {
    try lendSpanUntyped(array) { span in // expected-note{{it depends on the lifetime of argument 'span'}}
      kept = span
      throw ThrowStop() // expected-note{{this use causes the lifetime-dependent value to escape}}
    }
  } catch {}
  return kept![0]
}

// A throwing closure that does not store the dependent value is fine.
func throwWithoutStore(array: [Int]) -> Int {
  var count = 0
  do {
    try lendSpan(array) { (span) throws(ThrowStop) in
      count = span.count
      throw ThrowStop()
    }
  } catch {}
  return count
}

// rdar://190239363 - The autoclosure on the RHS of || or && captures a
// lifetime-dependent value. ClosureLifetimeFixup must not emit a non-dead-end
// destroy of the capture on the unreachable error path, after the dependence
// scope has ended.
@available(SwiftStdlib 6.2, *)
func spanInLogicalOr(_ s: String) -> Bool {
  let span = Array(s.utf8).span
  return span[0] == UInt8(ascii: "_") || span[0] == UInt8(ascii: "-")
}

@available(SwiftStdlib 6.2, *)
func spanInLogicalAnd(_ s: String) -> Bool {
  let span = Array(s.utf8).span
  return span[0] == UInt8(ascii: "_") && span[1] == UInt8(ascii: "-")
}

#if ONCE_FUNCTIONS
// An on-stack at-most-once or exactly-once closure consumes its captures, which is not an escape.
func runOnce(_ body: @once () -> Void) { body() }
func runCallableOnce(_ body: @callable(once) () -> Void) { body() }

func captureSpanInOnceClosure(span: Span<Int>) -> Int {
  var count = 0
  runOnce { count = span.count }
  runCallableOnce { count += span.count }
  return count
}

func captureLocalSpanInOnceClosure(array: [Int]) -> Int {
  let span = array.span
  var count = 0
  runOnce { count = span.count }
  runCallableOnce { count += span.count }
  return count
}

func captureNEInOnceClosure(nc: borrowing NCInt) -> Int {
  let ne = nc.neInt
  var value = 0
  runOnce { value = ne.value }
  runCallableOnce { value += ne.value }
  return value
}

struct NCNEInt: ~Copyable, ~Escapable {
  let value: Int

  @_lifetime(borrow borrowed)
  init(borrowed: borrowing NCInt) {
    self.value = borrowed.value
  }
}

func consumeNCNE(_ ne: consuming NCNEInt) -> Int { ne.value }

func captureNoncopyableNEInOnceClosure(nc: borrowing NCInt) -> Int {
  let ne = NCNEInt(borrowed: nc)
  var value = 0
  runOnce { value = ne.value }
  return value
}

func consumeNoncopyableNEInOnceClosure(nc: borrowing NCInt) -> Int {
  let ne = NCNEInt(borrowed: nc)
  var value = 0
  runCallableOnce { value = consumeNCNE(ne) }
  return value
}

func captureGenericNEInOnceClosure<T: ~Escapable>(_ t: T, _ use: (borrowing T) -> Void) {
  runOnce { use(t) }
  runCallableOnce { use(t) }
}

// The closure still carries the dependence of the values it consumes.
func runOnceSpan(_ body: @once () -> Span<Int>) -> Span<Int> { body() }

@_lifetime(immortal)
func testLocalSpanCaptureInOnceClosureReturnSpan() -> Span<Int> {
  let array = [1,2,3] // expected-once-note{{it depends on the lifetime of variable 'array'}}
  let span = array.span
  return runOnceSpan { span }
  // expected-once-error@-1{{lifetime-dependent value escapes its scope}}
  // expected-once-note@-2{{this use causes the lifetime-dependent value to escape}}
}
#endif
