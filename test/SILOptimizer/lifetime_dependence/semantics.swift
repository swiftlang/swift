// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -enable-builtin-module \
// RUN:   -enable-experimental-feature LifetimeDependence \
// RUN:   -enable-experimental-feature AddressableParameters \
// RUN:   -enable-experimental-feature AddressableTypes

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_LifetimeDependence
// REQUIRES: swift_feature_AddressableParameters
// REQUIRES: swift_feature_AddressableTypes

import Builtin

struct NotEscapable: ~Escapable {}

// Lifetime dependence semantics by example.
public struct Span<T>: ~Escapable {
  private var base: UnsafePointer<T>?
  private var count: Int

  @lifetime(borrow base)
  init(base: UnsafePointer<T>?, count: Int) {
    self.base = base
    self.count = count
  }

  @lifetime(borrow generic)
  init<S>(base: UnsafePointer<T>?, count: Int, generic: borrowing S) {
    self.base = base
    self.count = count
  }

  public subscript(_ position: Int) -> T {
    unsafeAddress {
      return base!.advanced(by: position)
    }
  }
}

extension Span {
  @lifetime(copy self)
  consuming func dropFirst() -> Span<T> {
    let nextPointer = self.base.flatMap { $0 + 1 }
    let local = Span(base: nextPointer, count: self.count - 1)
    return _overrideLifetime(local, copying: self)
  }
}

extension Span {
  @lifetime(copy self)
  mutating func droppingPrefix(length: Int) -> /* */ Span<T> {
    let oldBase = base
    let result = Span(base: oldBase, count: length)
    if let base = self.base {
      self.base = base + length
      self.count -= length
    }
    return _overrideLifetime(result, copying: self)
  }
}

struct MutableSpan<T>: ~Escapable, ~Copyable {
  let base: UnsafeMutablePointer<T>
  let count: Int

  @lifetime(&base)
  init(base: UnsafeMutablePointer<T>, count: Int) {
    self.base = base
    self.count = count
  }
}

extension Array {
  @lifetime(borrow self)
  borrowing func span() -> Span<Element> {
    /* not the real implementation */
    let p = self.withUnsafeBufferPointer { $0.baseAddress! }
    let span = Span(base: p, count: 1)
    return _overrideLifetime(span, borrowing: self)
  }
}

extension Array {
  @lifetime(&self)
  mutating func mutableSpan() -> MutableSpan<Element> {
    /* not the real implementation */
    let p = self.withUnsafeMutableBufferPointer { $0.baseAddress! }
    let span = MutableSpan<Element>(base: p, count: 1)
    return _overrideLifetime(span, mutating: &self)
  }
}

struct InnerTrivial {
  var p: UnsafePointer<Int>

  @lifetime(borrow self)
  borrowing func span() -> Span<Int> {
    Span(base: p, count: 1)
  }
}

struct TrivialHolder {
  var p: UnsafePointer<Int>
  var pa: UnsafePointer<AddressableInt>

  var addressableInt: AddressableInt { unsafeAddress { pa } }

  @lifetime(borrow self)
  borrowing func span() -> Span<Int> {
    Span(base: p, count: 1)
  }
}

struct Holder {
  let object: AnyObject
  var p: UnsafePointer<Int>
  var pa: UnsafePointer<AddressableInt>

  var addressableInt: AddressableInt { unsafeAddress { pa } }

  @lifetime(borrow self)
  borrowing func span() -> Span<Int> {
    Span(base: p, count: 1)
  }
}

@_addressableForDependencies
struct AddressableInt {
  let value: Int

  @lifetime(borrow self)
  borrowing func span() -> Span<Int> {
    // TODO: we actually want the address of self.value
    let p = UnsafePointer<Int>(Builtin.unprotectedAddressOfBorrow(self))
    let span = Span(base: p, count: 1)
    return _overrideLifetime(span, borrowing: self)
  }
}

@_addressableForDependencies
struct AddressableObject {
  let object: AnyObject

  @lifetime(borrow self)
  borrowing func span() -> Span<AnyObject> {
    // TODO: we actually want the address of self.object
    let p = UnsafePointer<AnyObject>(Builtin.unprotectedAddressOfBorrow(self))
    let span = Span(base: p, count: 1)
    return _overrideLifetime(span, borrowing: self)
  }
}

struct Outer {
  var _innerTrivial: InnerTrivial
  var _innerObject: Holder
  let trivialPointer: UnsafePointer<InnerTrivial>
  let objectPointer: UnsafePointer<Holder>

  var innerTrivialAddress: InnerTrivial {
    unsafeAddress {
      trivialPointer
    }
  }

  var innerObjectAddress: Holder {
    unsafeAddress {
      objectPointer
    }
  }

  var innerTrivialTemp: InnerTrivial {
    get { _innerTrivial }
  }

  var innerObjectTemp: Holder {
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

@lifetime(copy arg)
func copySpan<T>(_ arg: Span<T>) -> /* */ Span<T> { arg }

@lifetime(borrow arg)
func reborrowSpan<T>(_ arg: Span<T>) -> Span<T> { arg }

@lifetime(&arg)
func reborrowGenericInout<T: ~Escapable>(_ arg: inout T) -> T { arg }

@lifetime(copy arg)
func inheritSpan<T>(_ arg: Span<T>) -> Span<T> { arg }

@lifetime(copy arg)
func inheritGeneric<T: ~Escapable>(_ arg: consuming T) -> T { arg }

public struct NE: ~Escapable {}

@lifetime(&ne)
func borrowNE<T: ~Escapable>(ne: inout T) -> T {
  ne
}

// =============================================================================
// Initialization
// =============================================================================

struct View: ~Escapable {
  let owner: AnyObject

  @lifetime(borrow owner)
  init(owner: borrowing AnyObject) {
    self.owner = copy owner // OK
  }
}

struct MutableView: ~Escapable, ~Copyable {
  let owner: AnyObject

  // A copy of a borrow is indistinguishable with the borrow scope.
  @lifetime(borrow owner)
  init(owner: borrowing AnyObject) {
    self.owner = copy owner // OK
  }

  @lifetime(&mutableOwner)
  init(mutableOwner: inout AnyObject) {
    // Initialization of a ~Escapable value is unenforced. So we can initialize
    // the `owner` property without locally borrowing `mutableOwner`.
    self.owner = mutableOwner // OK
  }
}

struct Container<T> {
  var owner: AnyObject
  let pointer: UnsafeMutablePointer<T>
  let count: Int
}

// Dependence on an empty initialized value should be scoped to variable decl.
@lifetime(copy x)
func f(x: NotEscapable) -> NotEscapable {
  let local = NotEscapable() // expected-error {{lifetime-dependent variable 'local' escapes its scope}}
  // expected-note @-1{{it depends on the lifetime of this parent value}}
  return local // expected-note {{this use causes the lifetime-dependent value to escape}}
}

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

@lifetime(copy span)
public func testBorrowInheritedArg<T>(_ span: Span<T>) -> Span<T> {
  reborrowSpan(span) // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-2{{it depends on the lifetime of argument 'span'}}
} // expected-note {{this use causes the lifetime-dependent value to escape}}

@lifetime(copy t)
public func testBorrowInheritedInoutArg<T: ~Escapable>(_ t: inout T) -> T {
  // expected-error @-1{{lifetime-dependent variable 't' escapes its scope}}
  // expected-note @-2{{it depends on the lifetime of argument 't'}}
  reborrowGenericInout(&t)
  // expected-note @-1{{this use causes the lifetime-dependent value to escape}}
}

@lifetime(copy span)
public func testCopyInheritedArg<T>(_ span: Span<T>) -> Span<T> {
  inheritSpan(span)
}

@lifetime(copy t)
public func testCopyInheritedGenericArg<T: ~Escapable>(_ t: consuming T) -> T {
  inheritGeneric(t)
}

// =============================================================================
// Scoped dependence on inherited dependence
// =============================================================================

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

// Test a scoped dependence on an inherited inout argument.
//
// If testScopedOfInheritedWithLet is not an error, then its result can outlive its borrowed value:
//  let ne1 = NE()
//  var ne2 = ne1
//  let dep = foo(ne: &ne2)
//  _ = consume ne2
//  _ = dep
//
@lifetime(copy ne)
public func testScopedOfInheritedInout<T: ~Escapable>(ne: inout T) -> T {
  // expected-error @-1{{lifetime-dependent variable 'ne' escapes its scope}}
  // expected-note  @-2{{it depends on the lifetime of argument 'ne'}}
  borrowNE(ne: &ne)
  // expected-note  @-1{{this use causes the lifetime-dependent value to escape}}
}

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
public func testTrivialLocalDeadEnd(p: UnsafePointer<Int>) {
  let pointer = p
  let span = Span(base: pointer, count: 1)
  span.withThrowingClosure {}
}

// Test dependence on a borrow of a trivial inout. The access scope can be ignored since we don't care about the
// in-memory value.
@lifetime(borrow p)
public func testTrivialInoutBorrow(p: inout UnsafePointer<Int>) -> Span<Int> {
  return Span(base: p, count: 1)
}

// =============================================================================
// Scoped dependence on global values
// =============================================================================

private let immortalInt = 0

private let immortalStrings: [String] = []

@lifetime(immortal)
func testImmortalInt() -> Span<Int> {
  let nilBasedBuffer = UnsafeBufferPointer<Int>(start: nil, count: 0)
  let span = Span(base: nilBasedBuffer.baseAddress, count: nilBasedBuffer.count)
  return _overrideLifetime(span, borrowing: immortalInt)
}

@lifetime(immortal)
func testImmortalStrings() -> Span<[String]> {
  let nilBasedBuffer = UnsafeBufferPointer<[String]>(start: nil, count: 0)
  let span = Span(base: nilBasedBuffer.baseAddress, count: nilBasedBuffer.count)
  return _overrideLifetime(span, borrowing: immortalStrings)
}

let ptr = UnsafePointer<Int>(bitPattern: 1)!
let globalTrivial = InnerTrivial(p: ptr)

// An immortal span can depend on a caller's local borrow scope even though the callee sees no such dependency.
@lifetime(borrow local)
func testGlobal(local: InnerTrivial) -> Span<Int> {
  globalTrivial.span()
}

// =============================================================================
// Scoped dependence on mutable values
// =============================================================================

@lifetime(&a)
func testInoutBorrow(a: inout [Int]) -> Span<Int> {
  a.span() // OK
}

@lifetime(&a)
func testInoutMutableBorrow(a: inout [Int]) -> MutableSpan<Int> {
  a.mutableSpan()
}

// =============================================================================
// Scoped dependence on property access
// =============================================================================

extension Container {
  @lifetime(borrow self)
  func span() -> Span<T> {
    // borrowing the 'pointer' member is allowed.
    Span(base: self.pointer, count: self.count) // OK
  }

  @lifetime(borrow self)
  func view() -> View {
    // borrowing the 'view' member is allowed.
    View(owner: self.owner) // OK
  }

  @lifetime(&self)
  mutating func mutableView() -> MutableView {
    // Reading 'self.owner' creates a local borrow scope. The caller's exclusive access on 'self' is sufficient for the
    // result.
    MutableView(owner: self.owner) // OK
  }

  @lifetime(&self)
  mutating func mutableViewModifiesOwner() -> MutableView {
    // Passing '&self.owner' inout creates a nested exclusive access that must extend to all uses of the new
    // MutableView. This is allowed because the nested access is logically extended to the end of the function (without
    // violating exclusivity).
    MutableView(mutableOwner: &self.owner)
  }

  @lifetime(&self)
  mutating func mutableSpan() -> MutableSpan<T> {
    // Reading 'self.pointer' creates a local borrow scope. The local borrow
    // scope is ignored because 'pointer' is a trivial value. Instead, the new
    // MutableSpan depends directly on 'inout self'.
    MutableSpan(base: self.pointer, count: self.count) // OK
  }
}

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
}

func testExprExtendObjectTemp(outer: Outer) {
  parse(outer.innerObjectTemp.span())
}

func testLocalExtendTrivialTemp(outer: Outer) {
  let span = outer.innerTrivialTemp.span()
  parse(span)
}

func testLocalExtendObjectTemp(outer: Outer) {
  let span = outer.innerObjectTemp.span()
  parse(span)
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

// =============================================================================
// Scoped dependence on addressable parameters
// =============================================================================

// @_addressableForDependencies supports returning a Span.
@lifetime(borrow arg)
func testAddressableInt(arg: AddressableInt) -> Span<Int> {
  arg.span()
}

// @_addressableForDependencies supports returning a Span.
@lifetime(borrow arg)
func testAddressableObject(arg: AddressableObject) -> Span<AnyObject> {
  arg.span()
}

// Helper: create a dependence on the argument's address.
@lifetime(borrow arg)
func dependsOnTrivialAddressHelper(arg: @_addressable TrivialHolder) -> Span<Int> {
  arg.span()
}

// Helper: create a dependence on the argument's address.
@lifetime(borrow arg)
func dependsOnAddressHelper(arg: @_addressable Holder) -> Span<Int> {
  arg.span()
}

/* TODO: requires -enable-address-dependencies

// Non-addressable error returning a Span.
@lifetime(borrow arg)
func testTrivialNonAddressable(arg: TrivialHolder) -> Span<Int> {
  dependsOnTrivialAddressHelper(arg: arg)
  // todo-error @-1{{lifetime-dependent value escapes its scope}
  // todo-note  @-3{{it depends on the lifetime of variable 'arg'}}
} // todo-note  {{this use causes the lifetime-dependent value to escape}}

// Non-addressable error returning a Span.
@lifetime(borrow arg)
func testNonAddressable(arg: Holder) -> Span<Int> {
  dependsOnAddressHelper(arg: arg)
  // todo-error @-1{{lifetime-dependent value escapes its scope}
  // todo-note  @-3{{it depends on the lifetime of variable 'arg'}}
} // todo-note  {{this use causes the lifetime-dependent value to escape}}
*/

/* TODO: rdar://145872854 (SILGen: @addressable inout arguments are copied)
@lifetime(borrow arg)
func test(arg: inout AddressableInt) -> Span<Int> {
  arg.span()
}

// unsafeAddress generates an addressable value with a local scope.
@lifetime(borrow arg)
func testBorrowedAddressableInt(arg: Holder) -> Int {
  let span = arg.addressableInt.span()
  return span[0]
}

// unsafeAddress generates an addressable value.
// Error returning a dependence on its local scope.
@lifetime(borrow arg)
func testBorrowedAddressableIntReturn(arg: Holder) -> Span<Int> {
  arg.addressableInt.span()
  // todo-error @-1{{lifetime-dependent value escapes its scope}
  // todo-note  @-2{{it depends on the lifetime of this parent value}}
} // todo-note  {{this use causes the lifetime-dependent value to escape}}

*/
