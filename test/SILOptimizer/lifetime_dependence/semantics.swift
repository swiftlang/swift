// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature LifetimeDependence

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_LifetimeDependence

// Lifetime dependence semantics by example.
struct Span<T>: ~Escapable {
  private var base: UnsafePointer<T>
  private var count: Int

  @lifetime(borrow base)
  init(base: UnsafePointer<T>, count: Int) {
    self.base = base
    self.count = count
  }

  @lifetime(borrow generic)
  init<S>(base: UnsafePointer<T>, count: Int, generic: borrowing S) {
    self.base = base
    self.count = count
  }
}

extension Span {
  consuming func dropFirst() -> Span<T> {
    let nextPointer = self.base + 1
    let local = Span(base: nextPointer, count: self.count - 1)
    return unsafeLifetime(dependent: local, dependsOn: self)
  }
}

// TODO: Remove @_unsafeNonescapableResult. Instead, the unsafe dependence should be expressed by a builtin that is
// hidden within the function body.
@_unsafeNonescapableResult
@lifetime(source)
func unsafeLifetime<T: ~Copyable & ~Escapable, U: ~Copyable & ~Escapable>(
  dependent: consuming T, dependsOn source: borrowing U)
  -> T {
  dependent
}

// TODO: Remove @_unsafeNonescapableResult. Instead, the unsafe dependence should be expressed by a builtin that is
// hidden within the function body.
@_unsafeNonescapableResult
@lifetime(borrow source)
func unsafeLifetime<T: ~Copyable & ~Escapable, U: ~Copyable & ~Escapable>(
  dependent: consuming T, scope source: borrowing U)
  -> T {
  dependent
}

extension Span {
  mutating func droppingPrefix(length: Int) -> /* */ Span<T> {
    let oldBase = base
    let result = Span(base: oldBase, count: length)
    self.base += length
    self.count -= length
    return unsafeLifetime(dependent: result, dependsOn: self)
  }
}

extension Array {
  @lifetime(borrow self)
  borrowing func span() -> Span<Element> {
    /* not the real implementation */
    let p = self.withUnsafeBufferPointer { $0.baseAddress! }
    let span = Span(base: p, count: 1)
    return unsafeLifetime(dependent: span, scope: self)
  }
}

struct InnerTrivial {
  var p: UnsafePointer<Int>

  @lifetime(borrow self)
  borrowing func span() -> Span<Int> {
    Span(base: p, count: 1)
  }
}

struct InnerObject {
  let object: AnyObject
  var p: UnsafePointer<Int>

  @lifetime(borrow self)
  borrowing func span() -> Span<Int> {
    Span(base: p, count: 1)
  }
}

struct Outer {
  var _innerTrivial: InnerTrivial
  var _innerObject: InnerObject
  let trivialPointer: UnsafePointer<InnerTrivial>
  let objectPointer: UnsafePointer<InnerObject>

  var innerTrivialAddress: InnerTrivial {
    unsafeAddress {
      trivialPointer
    }
  }

  var innerObjectAddress: InnerObject {
    unsafeAddress {
      objectPointer
    }
  }

  var innerTrivialTemp: InnerTrivial {
    get { _innerTrivial }
  }

  var innerObjectTemp: InnerObject {
    get { _innerObject }
  }

  /* TODO: rdar://137608270 Add Builtin.addressof() support for @addressable arguments
  @addressableSelf
  var innerAddress: Inner {
    unsafeAddress {
      Builtin.addressof(inner)
    }
  }
  */
}

func parse(_ span: Span<Int>) {}

// =============================================================================
// Scoped dependence on values
// =============================================================================

// The duration of a scoped dependence is the lexical scope of the variable.
func testScopedLet(_ arg: [Int] ) {
  let a: Array<Int> = arg
  let span: Span<Int> // expected-error {{lifetime-dependent variable 'span' escapes its scope}}
  do {
    let a2 = a        // expected-note {{it depends on the lifetime of variable 'a2'}}
    span = a2.span()
  }
  parse(span) // expected-note {{this use of the lifetime-dependent value is out of scope}}
}

func testScopedCopy(_ arg: [Int] ) {
  let a: Array<Int> = arg
  let span: Span<Int> // expected-error {{lifetime-dependent variable 'span' escapes its scope}}
  do {
    let a2 = a        // expected-note {{it depends on the lifetime of variable 'a2'}}
    span = a2.span()
  }
  parse(span) // expected-note {{this use of the lifetime-dependent value is out of scope}}
}

// =============================================================================
// Inherited dependence on values
// =============================================================================

func copySpan<T>(_ arg: Span<T>) -> /* */ Span<T> { arg }

func testInheritedCopy(_ arg: [Int] ) {
  let a: Array<Int> = arg
  let result: Span<Int>
  do {
    let span = a.span()
    let temp = span
    result = copySpan(temp)
  }
  parse(result) // ✅ Safe: within lifetime of 'a'
}

func testInheritedCopyVar(_ arg: [Int] ) {
  let a1: Array<Int> = arg
  let a2: Array<Int> = arg
  var span = a1.span()
  var result: Span<Int>
  do {
    var temp = span
    result = copySpan(temp)
    span = a2.span()
    temp = a2.span()
    // 'result' still depends on 'a1', not 'a2'
  }
  parse(result) // ✅ Safe: within lifetime of 'a'
}

// =============================================================================
// Scoped dependence on inherited dependence
// =============================================================================

@lifetime(borrow arg)
func reborrowSpan<T>(_ arg: Span<T>) -> Span<T> { arg }

func testScopedOfInheritedWithCall(_ arg: [Int] ) {
  let a: Array<Int> = arg
  let span = a.span()
  // TODO: should be // ✅ Safe: 'copySpan' result should be borrowed over `parse`
  // rdar://128821299 ([nonescaping] extend borrowed arguments that are the source of a scoped dependence)
  parse(reborrowSpan(copySpan(span))) // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1{{this use of the lifetime-dependent value is out of scope}}
  // expected-note @-2{{it depends on the lifetime of this parent value}}
}

func testScopedOfInheritedWithLet(_ arg: [Int] ) {
  let a: Array<Int> = arg
  let span = a.span()
  // TODO: should be // ✅ Safe: 'copySpan' result should be borrowed over `result`
  // rdar://128821299 ([nonescaping] extend borrowed arguments that are the source of a scoped dependence)
  let result = reborrowSpan(copySpan(span)) // expected-error {{lifetime-dependent variable 'result' escapes its scope}}
  // expected-note @-1{{it depends on the lifetime of this parent value}}  
  _ = result
} // expected-note {{this use of the lifetime-dependent value is out of scope}}

// =============================================================================
// Scoped dependence on trivial values
// =============================================================================

@lifetime(borrow a)
func testTrivialScope<T>(a: Array<T>) -> Span<T> {
  let p = a.withUnsafeBufferPointer { $0.baseAddress! }
  return Span(base: p, count: 1)
  // expected-error @-1{{lifetime-dependent value escapes its scope}}
  // expected-note  @-3{{it depends on the lifetime of variable 'p'}}
  // expected-note  @-3{{this use causes the lifetime-dependent value to escape}}
}

extension Span {
  public func withThrowingClosure<E: Error>(_ body: () throws(E) -> ()) throws(E) -> () {
    try body()
  }
}

// Test dependence on an local variable that needs to be extended into the dead-end block of a never-throwing apply.
public func test(p: UnsafePointer<Int>) {
  let pointer = p
  let span = Span(base: pointer, count: 1)
  span.withThrowingClosure {}
}

// =============================================================================
// Scoped dependence on property access
// =============================================================================

@lifetime(borrow outer)
func testBorrowStoredTrivial(outer: Outer) -> Span<Int> {
  outer._innerTrivial.span()
}

@lifetime(borrow outer)
func testBorrowStoredObject(outer: Outer) -> Span<Int> {
  outer._innerObject.span()
}

@lifetime(borrow outer)
func testBorrowTrivialAddressProjection(outer: Outer) -> Span<Int> {
  outer.innerTrivialAddress.span()
}

@lifetime(borrow outer)
func testBorrowObjectAddressProjection(outer: Outer) -> Span<Int> {
  outer.innerObjectAddress.span()
}

func testExprExtendTrivialTemp(outer: Outer) {
  parse(outer.innerTrivialTemp.span())
  // expected-error @-1{{lifetime-dependent value escapes its scope}}
  // expected-note  @-2{{it depends on the lifetime of this parent value}}
  // expected-note  @-3{{this use of the lifetime-dependent value is out of scope}}
}

func testExprExtendObjectTemp(outer: Outer) {
  parse(outer.innerObjectTemp.span())
  // expected-error @-1{{lifetime-dependent value escapes its scope}}
  // expected-note  @-2{{it depends on the lifetime of this parent value}}
  // expected-note  @-3{{this use of the lifetime-dependent value is out of scope}}
}

func testLocalExtendTrivialTemp(outer: Outer) {
  let span = outer.innerTrivialTemp.span()
  // expected-error @-1{{lifetime-dependent variable 'span' escapes its scope}}
  // expected-note  @-2{{it depends on the lifetime of this parent value}}
  parse(span) // expected-note {{this use of the lifetime-dependent value is out of scope}}
}

func testLocalExtendObjectTemp(outer: Outer) {
  let span = outer.innerObjectTemp.span()
  // expected-error @-1{{lifetime-dependent variable 'span' escapes its scope}}
  // expected-note  @-2{{it depends on the lifetime of this parent value}}
  parse(span) // expected-note {{this use of the lifetime-dependent value is out of scope}}
}

@lifetime(borrow outer)
func testReturnTrivialTemp(outer: Outer) -> Span<Int> {
  outer.innerTrivialTemp.span()
  // expected-error @-1{{lifetime-dependent value escapes its scope}}
  // expected-note  @-2{{it depends on the lifetime of this parent value}}
} // expected-note {{this use causes the lifetime-dependent value to escape}}

@lifetime(borrow outer)
func testReturnObjectTemp(outer: Outer) -> Span<Int> {
  outer.innerObjectTemp.span()
  // expected-error @-1{{lifetime-dependent value escapes its scope}}
  // expected-note  @-2{{it depends on the lifetime of this parent value}}
} // expected-note  {{this use causes the lifetime-dependent value to escape}}
