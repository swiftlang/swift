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

// `consuming` is rejected: a read-modify-write access runs more than one
// accessor, so the argument would be consumed more than once.
struct Consuming {
  subscript(i: consuming Int) -> Int { return i }
  // expected-error@-1 {{'consuming' may not be used on a subscript parameter; a read-modify-write access would consume the argument more than once}}
  subscript(nc: consuming NC) -> Int { return nc.value }
  // expected-error@-1 {{'consuming' may not be used on a subscript parameter; a read-modify-write access would consume the argument more than once}}
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
