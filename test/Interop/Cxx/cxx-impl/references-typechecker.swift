// RUN: %target-typecheck-verify-swift \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}references.h \
// RUN:   -I %S%{fs-sep}Inputs

// REQUIRES: swift_feature_CxxImplementation

import References


// A mutable reference is implemented by an `inout` parameter, a const
// reference by a plain by-value parameter, and a reference return by a
// non-optional pointer. An implementation with a reference parameter must be
// marked `@unsafe`.

@unsafe @cxx @implementation
func addOne(_ x: inout CInt) -> CInt {
  x += 1
  return x
}

@unsafe @cxx @implementation
func swapRefs(_ a: inout CInt, _ b: inout CInt) {
  let t = a
  a = b
  b = t
}

@unsafe @cxx @implementation
func readConstRef(_ x: CInt) -> CInt { return x * 10 }


// Reference returns

let refStorage: UnsafeMutablePointer<CInt> = .allocate(capacity: 1)
let ptrSlot: UnsafeMutablePointer<UnsafeMutablePointer<CInt>> = .allocate(capacity: 1)

@cxx @implementation
func mutableRefReturn() -> UnsafeMutablePointer<CInt> { return refStorage }

@cxx @implementation
func constRefReturn() -> UnsafePointer<CInt> { return UnsafePointer(refStorage) }

@cxx @implementation
func refToPtrReturn() -> UnsafeMutablePointer<UnsafeMutablePointer<CInt>> {
  return ptrSlot
}


// A reference to a pointer

@unsafe @cxx @implementation
func reseatPtr(_ p: inout UnsafeMutablePointer<CInt>?) { p = refStorage }


// Reference-ness participates in overload identity.

@unsafe @cxx @implementation
func refOverload(_ x: inout CInt) { x += 100 }

@unsafe @cxx @implementation
func refOverload(_ x: CInt) { referencesGlobal = x }

@cxx @implementation
func refOverload(_ p: UnsafeMutablePointer<CInt>) { p.pointee += 1000 }


// A method taking a reference

extension Accumulator {
  @unsafe @cxx @implementation
  func addTo(_ target: inout CInt) -> CInt {
    target += total
    return target
  }
}


// References to a struct

@unsafe @cxx @implementation
func bumpTotal(_ acc: inout Accumulator) { acc.total += 1 }

@unsafe @cxx @implementation
func readTotal(_ acc: Accumulator) -> CInt { return acc.total }


// An implementation with reference parameters that is not marked `@unsafe`
// is rejected, with a note for each reference parameter.

// expected-error@+2{{global function 'missingUnsafeParam' must be marked '@unsafe' to implement C++ function 'missingUnsafeParam'}}{{-1:1-1=@unsafe }}
@cxx @implementation
func missingUnsafeParam(
  _ x: inout CInt // expected-note{{C++ callers may violate exclusive access to 'inout' parameter 'x'}}
) -> CInt { return x }

// expected-error@+2{{global function 'missingUnsafeConstParam' must be marked '@unsafe' to implement C++ function 'missingUnsafeConstParam'}}{{-1:1-1=@unsafe }}
@cxx @implementation
func missingUnsafeConstParam(
  _ x: CInt // expected-note{{C++ callers may modify parameter 'x' through another reference during the call}}
) -> CInt { return x }

// expected-error@+2{{global function 'missingUnsafeMixed' must be marked '@unsafe' to implement C++ function 'missingUnsafeMixed'}}{{-1:1-1=@unsafe }}
@cxx @implementation
func missingUnsafeMixed(
  _ a: CInt,
  _ b: inout CInt, // expected-note{{C++ callers may violate exclusive access to 'inout' parameter 'b'}}
  _ c: CInt // expected-note{{C++ callers may modify parameter 'c' through another reference during the call}}
) { b += a + c }

// expected-error@+2{{global function 'missingUnsafeUnnamed' must be marked '@unsafe' to implement C++ function 'missingUnsafeUnnamed'}}{{-1:1-1=@unsafe }}
@cxx @implementation
func missingUnsafeUnnamed(
  _ x: inout CInt // expected-note{{C++ callers may violate exclusive access to 'inout' parameter 'x'}}
) -> CInt { return x }


// RValue references are rejected.

// expected-error@+2{{global function 'takesRvalueRef(consuming:)' cannot implement C++ function 'takesRvalueRef' because rvalue reference parameters and return types are not yet supported}}
@cxx @implementation
func takesRvalueRef(consuming x: consuming CInt) {}

// expected-error@+2{{global function 'returnsRvalueRef()' cannot implement C++ function 'returnsRvalueRef' because rvalue reference parameters and return types are not yet supported}}
@cxx @implementation
func returnsRvalueRef() -> UnsafeMutablePointer<CInt> { fatalError() }


// The pointer spelling of a reference parameter is rejected. The
// implementation must use the imported projection.

// expected-error@+2{{global function 'mismatchedSpelling' of type '(UnsafeMutablePointer<CInt>) -> CInt' (aka '(UnsafeMutablePointer<Int32>) -> Int32') does not match type '(inout CInt) -> CInt' (aka '(inout Int32) -> Int32') declared by the header}}
@cxx @implementation
func mismatchedSpelling(_ x: UnsafeMutablePointer<CInt>) -> CInt { return x.pointee }

// expected-error@+2{{global function 'mismatchedConstSpelling' of type '(UnsafePointer<CInt>) -> CInt' (aka '(UnsafePointer<Int32>) -> Int32') does not match type '(CInt) -> CInt' (aka '(Int32) -> Int32') declared by the header}}
@cxx @implementation
func mismatchedConstSpelling(_ x: UnsafePointer<CInt>) -> CInt { return x.pointee }
