// RUN: %target-typecheck-verify-swift \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}references.h \
// RUN:   -I %S%{fs-sep}Inputs

// REQUIRES: swift_feature_CxxImplementation

import References


// A reference is implemented the way the C++ ABI passes it: a mutable
// reference as an `UnsafeMutablePointer`, a const reference as an
// `UnsafePointer`, and a reference return as a non-optional pointer. Swift
// callers still use the imported projections (`inout` and by-value).

@cxx @implementation
func addOne(_ x: UnsafeMutablePointer<CInt>) -> CInt {
  x.pointee += 1
  return x.pointee
}

@cxx @implementation
func swapRefs(_ a: UnsafeMutablePointer<CInt>, _ b: UnsafeMutablePointer<CInt>) {
  let t = a.pointee
  a.pointee = b.pointee
  b.pointee = t
}

@cxx @implementation
func observe(_ a: UnsafeMutablePointer<CInt>, _ b: UnsafeMutablePointer<CInt>) -> CInt {
  a.pointee = 1
  b.pointee = 2
  return a.pointee
}

@cxx @implementation
func observeGlobal(_ x: UnsafeMutablePointer<CInt>) -> CInt {
  referencesGlobal = 5
  x.pointee = 7
  return referencesGlobal
}

@cxx @implementation
func readConstRef(_ x: UnsafePointer<CInt>) -> CInt { return x.pointee * 10 }


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

@cxx @implementation
func reseatPtr(_ p: UnsafeMutablePointer<UnsafeMutablePointer<CInt>?>) {
  p.pointee = refStorage
}


// Reference-ness participates in overload identity: `int &` and `const int &`
// import with distinct implementation spellings, so both overloads are
// implementable side by side.

@cxx @implementation
func refOverload(_ x: UnsafeMutablePointer<CInt>) { x.pointee += 100 }

@cxx @implementation
func refOverload(_ x: UnsafePointer<CInt>) { referencesGlobal = x.pointee }


// A method taking a reference

extension Accumulator {
  @cxx @implementation
  func addTo(_ target: UnsafeMutablePointer<CInt>) -> CInt {
    target.pointee += total
    return target.pointee
  }
}


// References to a struct

@cxx @implementation
func bumpTotal(_ acc: UnsafeMutablePointer<Accumulator>) { acc.pointee.total += 1 }

@cxx @implementation
func readTotal(_ acc: UnsafePointer<Accumulator>) -> CInt { return acc.pointee.total }

@cxx @implementation
func identityRef(_ acc: UnsafeMutablePointer<Accumulator>) -> UnsafeMutablePointer<Accumulator> {
  return acc
}


// A mutable reference and a nonnull pointer overload share one
// implementation spelling; the pair is rejected as ambiguous.

// expected-error@+1{{global function 'ambiguousRefOverload' could implement any of several imported overloads of 'ambiguousRefOverload' that have the same signature in Swift}}
@cxx @implementation
func ambiguousRefOverload(_ x: UnsafeMutablePointer<CInt>) {}


// Rvalue references are rejected: a `T &&` parameter imports as `consuming`,
// but the C++ caller destroys the referent after the call anyway, and a
// `T &&` return imports like `T &`.

// expected-error@+2{{global function 'takesRvalueRef(consuming:)' cannot implement C++ function 'takesRvalueRef' because rvalue reference parameters and return types are not yet supported}}
@cxx @implementation
func takesRvalueRef(consuming x: consuming CInt) {}

// expected-error@+2{{global function 'returnsRvalueRef()' cannot implement C++ function 'returnsRvalueRef' because rvalue reference parameters and return types are not yet supported}}
@cxx @implementation
func returnsRvalueRef() -> UnsafeMutablePointer<CInt> { fatalError() }


// Wrong spellings are rejected: `inout` is not representable, and the
// mismatch diagnostics name the expected pointer type.

// expected-error@+2{{global function cannot be marked '@cxx' because inout parameters cannot be represented in C++}}
@cxx @implementation
func mismatchedSpelling(_ x: inout CInt) -> CInt { return x }

// expected-error@+2{{global function 'mismatchedConstSpelling' of type '(CInt) -> CInt' (aka '(Int32) -> Int32') does not match type '(UnsafePointer<CInt>) -> CInt' (aka '(UnsafePointer<Int32>) -> Int32') declared by the header}}
@cxx @implementation
func mismatchedConstSpelling(_ x: CInt) -> CInt { return x }
