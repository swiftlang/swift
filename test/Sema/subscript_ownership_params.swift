// RUN: %target-typecheck-verify-swift

// Ownership specifiers on subscript parameters.

struct NC: ~Copyable {
  var value: Int
}

// `borrowing` is allowed, including for a noncopyable index.
struct Borrowing {
  var slots: [Int] = [0]
  subscript(i: borrowing Int) -> Int {
    get { slots[i] }
    set { slots[i] = newValue }
  }
  subscript(nc: borrowing NC) -> Int { return slots[nc.value] }
  subscript(coro nc: borrowing NC) -> Int {
    _read { yield slots[nc.value] }
    _modify { yield &slots[nc.value] }
  }
  subscript<T: ~Copyable>(generic t: borrowing T) -> Int { return 0 }
  subscript(defaulted i: borrowing Int = 0) -> Int { return i }
}

// A noncopyable index still needs an explicit specifier, and the diagnostic
// offers the ownership fix-its rather than saying subscripts cannot do this.
struct MissingOwnership {
  subscript(nc: NC) -> Int { return nc.value }
  // expected-error@-1 {{parameter of noncopyable type 'NC' must specify ownership}}
  // expected-note@-2 {{add 'borrowing' for an immutable reference}}
  // expected-note@-3 {{add 'inout' for a mutable reference}}
  // expected-note@-4 {{add 'consuming' to take the value from the caller}}
}

// `consuming` is allowed exactly when a single accessor performs a whole
// access, so that the index is consumed once. A subscript whose read and write
// go through separate accessors would consume it twice.
struct Consuming {
  var slots: [Int] = [0]

  // A single `_read`, so a read consumes the index once.
  subscript(readOnly i: consuming Int) -> Int { _read { yield slots[i] } }
  subscript(readOnlyNC nc: consuming NC) -> Int { _read { yield slots[nc.value] } }

  // `_read` + `_modify`: every access runs exactly one of them.
  subscript(coro nc: consuming NC) -> Int {
    _read { yield slots[nc.value] }
    _modify { yield &slots[nc.value] }
  }

  // A getter runs once per read, and a read-modify-write runs only the
  // coroutine, so mixing them is fine too.
  subscript(getModify nc: consuming NC) -> Int {
    get { slots[nc.value] }
    _modify { yield &slots[nc.value] }
  }

  // A plain getter, with no mutation at all.
  subscript(getOnly nc: consuming NC) -> Int { get { slots[nc.value] } }
}

// A `get`/`set` pair is rejected: a read-modify-write runs both.
struct ConsumingGetSet {
  var slots: [Int] = [0]
  subscript(i: consuming Int) -> Int {
    // expected-error@-1 {{'consuming' may not be used on the parameter of a subscript whose read and write go through separate accessors; a read-modify-write access would consume the argument more than once}}
    get { slots[i] }
    set { slots[i] = newValue } // expected-note {{use a '_modify' accessor instead, so that a single accessor performs the whole access}}
  }
}

// So is `_read` with a plain setter, for the same reason.
struct ConsumingReadSet {
  var slots: [Int] = [0]
  subscript(nc: consuming NC) -> Int {
    // expected-error@-1 {{'consuming' may not be used on the parameter of a subscript whose read and write go through separate accessors; a read-modify-write access would consume the argument more than once}}
    _read { yield slots[nc.value] }
    set { slots[nc.value] = newValue } // expected-note {{use a '_modify' accessor instead, so that a single accessor performs the whole access}}
  }
}

// A protocol requirement is spelled with `get`/`set`, but the accessors that
// implement it are the witness's, so the requirement itself is fine.
protocol HasConsumingSubscript {
  subscript(nc: consuming NC) -> Int { get set }
}
struct GoodConsumingWitness: HasConsumingSubscript {
  var slots: [Int] = [0]
  subscript(nc: consuming NC) -> Int {
    get { slots[nc.value] }
    _modify { yield &slots[nc.value] }
  }
}
struct BadConsumingWitness: HasConsumingSubscript {
  var slots: [Int] = [0]
  subscript(nc: consuming NC) -> Int {
    // expected-error@-1 {{'consuming' may not be used on the parameter of a subscript whose read and write go through separate accessors; a read-modify-write access would consume the argument more than once}}
    get { slots[nc.value] }
    set { slots[nc.value] = newValue } // expected-note {{use a '_modify' accessor instead, so that a single accessor performs the whole access}}
  }
}

func useConsuming(c: inout Consuming, g: inout GoodConsumingWitness) {
  _ = c[readOnly: 0]
  _ = c[readOnlyNC: NC(value: 0)]
  _ = c[coro: NC(value: 0)]
  c[coro: NC(value: 0)] = 1
  c[coro: NC(value: 0)] += 1
  _ = c[getModify: NC(value: 0)]
  c[getModify: NC(value: 0)] += 1
  _ = c[getOnly: NC(value: 0)]
  g[NC(value: 0)] += 1
}

func useConsumingGeneric<T: HasConsumingSubscript>(t: inout T) {
  _ = t[NC(value: 0)]
  t[NC(value: 0)] += 1
}

// `inout` is supported: the index takes the exclusive access itself.
struct InOut {
  var slots: [Int] = [0, 0]
  subscript(i: inout Int) -> Int {
    get { slots[i] }
    set { slots[i] = newValue; i += 1 }
  }
  subscript(coro i: inout Int) -> Int {
    _read { yield slots[i] }
    _modify { yield &slots[i] }
  }
  subscript(nc: inout NC) -> Int {
    get { nc.value }
    set { nc.value = newValue }
  }
}

func useInOut(o: inout InOut, i: inout Int, nc: inout NC) {
  _ = o[&i]
  o[&i] = 1
  o[&i] += 1
  _ = o[coro: &i]
  o[coro: &i] += 1
  _ = o[&nc]
  o[&nc] = 2
}

// An `inout` index has to be passed with `&`, like any other inout argument.
func inOutNeedsAmpersand(o: inout InOut, i: inout Int) {
  _ = o[i]
  // expected-error@-1 {{passing value of type 'Int' to an inout parameter requires explicit '&'}}
}

// The implicit inout-to-pointer conversion does not apply to subscript
// arguments, so `&x` cannot be used to form a pointer there.
struct PointerIndex {
  subscript(p: UnsafeMutablePointer<Int>) -> Int { return 0 }
  subscript(raw p: UnsafeRawPointer) -> Int { return 0 }
  subscript(arr p: UnsafePointer<Int>) -> Int { return 0 }
}
func noInOutToPointer(p: PointerIndex, i: inout Int, a: inout [Int]) {
  _ = p[&i]
  // expected-error@-1 {{cannot pass an inout argument to a subscript; use 'withUnsafeMutablePointer' to explicitly convert argument to a pointer}}
  _ = p[raw: &i]
  // expected-error@-1 {{cannot pass an inout argument to a subscript; use 'withUnsafeMutablePointer' to explicitly convert argument to a pointer}}
  _ = p[arr: &a]
  // expected-error@-1 {{cannot pass an inout argument to a subscript; use 'withUnsafeMutablePointer' to explicitly convert argument to a pointer}}
}

// Variadic parameters still get the variadic-specific diagnostic.
struct Variadic {
  subscript(i: borrowing Int...) -> Int { return 0 }
  // expected-error@-1 {{'borrowing' must not be used on variadic parameters}}
}

// A key path to a subscript with a noncopyable index stays rejected: key paths
// store their indices, which would copy.
struct KP {
  subscript(nc: borrowing NC) -> Int { return nc.value }
}
func keyPathToNoncopyableIndex() {
  _ = \KP.[NC(value: 1)]
  // expected-error@-1 {{subscript index of type 'NC' in a key path must be Hashable}}
}

// Reads and writes through a borrowing index work, and the index survives.
func useBorrowing(b: inout Borrowing, nc: borrowing NC) {
  _ = b[0]
  b[0] = 1
  b[0] += 1
  _ = b[nc]
  _ = b[coro: nc]
  b[coro: nc] += 1
  _ = nc.value
}
