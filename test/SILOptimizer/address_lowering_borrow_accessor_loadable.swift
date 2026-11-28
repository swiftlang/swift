// RUN: %target-swift-frontend -emit-sil -sil-verify-all -disable-availability-checking -enable-sil-opaque-values -o /dev/null %s

// Subscripting an InlineArray or Span calls the stdlib's borrow accessor, whose
// result convention is @guaranteed_address: an object before AddressLowering,
// and a directly-returned address after it. When the element type is loadable
// the apply's result has no storage in the valueStorageMap, so AddressLowering
// has to reload it from the returned address; otherwise the address escapes into
// code that still expects an object.

func check<T>(_ t: T) {}

// The address reached `return`, which wants an object.
func returnElement(_ a: InlineArray<5, Int>) -> Int {
  a[0]
}

// The address became the source of a store while materializing the
// @in_guaranteed argument of a generic call.
func passElement(_ a: InlineArray<5, Int>) {
  check(a[0])
}

// The address reached `struct_extract`, an object-only operation.
func compareElement(_ a: InlineArray<5, UInt8>) -> Bool {
  a[0] == 0
}

func spanElement(_ s: Span<Int>) -> Int {
  s[0]
}

struct Element: Equatable { var name: String }

// A non-trivial loadable element cannot be borrowed in place: the returned
// address points into the temporary that self was materialized into for the
// call, which is torn down as soon as the call returns. The value is copied out
// of the address instead, and the copy is borrowed for uses that expect the
// result's guaranteed ownership.
func compareNontrivialElement(_ a: InlineArray<2, Element>) -> Bool {
  a[0] == Element(name: "string")
}

// The address reached `struct_extract`, which forwards guaranteed ownership and
// so cannot take the owned copy directly.
func nontrivialElementProperty(_ a: InlineArray<2, Element>) -> String {
  a[0].name
}

// Same, with the InlineArray in a local: self is materialized into a
// store_borrow temporary whose dealloc_stack directly follows the call, so the
// reload has to be emitted ahead of it.
func compareLocalNontrivialElement() -> Bool {
  let e = Element(name: "string")
  let a: InlineArray<2, Element> = [e, e]
  return a[0] == e
}

// An address-only element takes the other path: its result *is* mapped into the
// valueStorageMap and rewritten by the DefRewriter. Kept here so both paths stay
// covered by one test.
func returnAddressOnlyElement<T>(_ a: InlineArray<5, T>) -> T {
  a[0]
}
