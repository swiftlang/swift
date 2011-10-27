// RUN: not %swift %s -verify

import swift

//===----------------------------------------------------------------------===//
// Tests and samples.
//===----------------------------------------------------------------------===//

/// UnsafePointerInt - This is UnsafePointer<Int>, we just don't have generics
/// yet.
///
/// This is a raw and unsafe pointer in Swift, corresponding to a raw
/// "__unsafe_unretained T*" in C.  Notably, it doesn't maintain lifetime, so it
/// can dangle.
struct /*[fragile]*/ UnsafePointerInt {
  // FIXME: intptr_t? magic to get a decent LLVM IR type?
  value : int32
}

// TODO: Add arithmetic operations for pointer arithmetic on UnsafePointerInt.


// TODO: Need a builtin operation to turn UnsafePointer into a swift lvalue.

/// unpackLValue - This should be magically pulled out of the builtin package.
func unpackLValue(x : lvalue) -> (UnsafePointerInt, RefcountPointer)


