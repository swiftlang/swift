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

// An address-only element takes the other path: its result *is* mapped into the
// valueStorageMap and rewritten by the DefRewriter. Kept here so both paths stay
// covered by one test.
func returnAddressOnlyElement<T>(_ a: InlineArray<5, T>) -> T {
  a[0]
}
