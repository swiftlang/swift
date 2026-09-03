// RUN: %target-typecheck-verify-swift \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -Xcc -Wno-nullability-completeness \
// RUN:   -I %S/Inputs

// REQUIRES: swift_feature_CxxImplementation

import Functions
import Matching


// Primitives

@cxx @implementation
func takesPrimitives(_ i: CInt, _ l: CLong, _ c: CChar, _ f: CFloat, _ d: CDouble, _ b: CBool) {}

@cxx @implementation
func returnsInt() -> CInt { return 0 }


// Pointers

@cxx @implementation
func takesPtrToInt(_ p: UnsafeMutablePointer<CInt>?) {}

@cxx @implementation
func takesNullablePtrToInt(_ p: UnsafeMutablePointer<CInt>?) {}

@cxx @implementation
func takesNonnullPtrToInt(_ p: UnsafeMutablePointer<CInt>) {}

@cxx @implementation
func takesPtrToVoid(_ p: UnsafeMutableRawPointer?) {}

@cxx @implementation
func takesNullablePtrToVoid(_ p: UnsafeMutableRawPointer?) {}

@cxx @implementation
func takesNonnullPtrToVoid(_ p: UnsafeMutableRawPointer) {}

@cxx @implementation
func takesPtrToConstInt(_ p: UnsafePointer<CInt>?) {}

@cxx @implementation
func takesNullablePtrToConstInt(_ p: UnsafePointer<CInt>?) {}

@cxx @implementation
func takesNonnullPtrToConstInt(_ p: UnsafePointer<CInt>) {}

@cxx @implementation
func takesFuncPtr(_ fn: (@convention(c) (CInt) -> CInt)?) {}

@cxx @implementation
func takesNullableFuncPtr(_ fn: (@convention(c) (CInt) -> CInt)?) {}

@cxx @implementation
func takesNonnullFuncPtr(_ fn: @convention(c) (CInt) -> CInt) {}

@cxx @implementation
func returnsPtrToInt() -> UnsafeMutablePointer<CInt>? { fatalError() }

@cxx @implementation
func returnsNullablePtrToInt() -> UnsafeMutablePointer<CInt>? { fatalError() }

@cxx @implementation
func returnsNonnullPtrToInt() -> UnsafeMutablePointer<CInt> { fatalError() }


// C++ references

// expected-error@+2{{global function 'takesConstRef' cannot implement C++ function 'takesConstRef' because reference parameters and return types are not yet supported}}
@cxx @implementation
func takesConstRef(_ x: Int32) -> Int32 { return x }

// expected-error@+2{{global function cannot be marked '@cxx' because inout parameters cannot be represented in C++}}
@cxx @implementation
func takesMutableRef(_ x: inout Int32) {}

// expected-error@+2{{global function 'returnsMutableRef()' cannot implement C++ function 'returnsMutableRef' because reference parameters and return types are not yet supported}}
@cxx @implementation
func returnsMutableRef() -> UnsafeMutablePointer<Int32> { fatalError() }


// Trivial struct

@cxx @implementation
func takesTrivialStruct(_ s: TrivialStruct) {}

@cxx @implementation
func returnsTrivialStruct() -> TrivialStruct { fatalError() }


// Non-trivial classes

// expected-error@+3{{global function cannot be marked '@cxx' because the type of the parameter cannot be represented in C++}}
// expected-note@+2{{non-trivial C++ classes cannot be represented in C++}}
@cxx @implementation
func takesNonTrivial(_ obj: NonTrivialClass) {}

// expected-error@+3{{global function cannot be marked '@cxx' because its result type cannot be represented in C++}}
// expected-note@+2{{non-trivial C++ classes cannot be represented in C++}}
@cxx @implementation
func returnsNonTrivial() -> NonTrivialClass { fatalError() }


// Swift String

// expected-error@+3{{global function cannot be marked '@cxx' because the type of the parameter cannot be represented in C++}}
// expected-note@+2{{Swift structs cannot be represented in C++}}
@cxx @implementation
func takesSwiftString(_ s: String) {}

// expected-error@+3{{global function cannot be marked '@cxx' because its result type cannot be represented in C++}}
// expected-note@+2{{Swift structs cannot be represented in C++}}
@cxx @implementation
func returnsSwiftString() -> String { return "" }


// Swift specials: async, throwing, generic

// expected-error@+1{{@cxx global function cannot be asynchronous}}
@cxx @implementation
func asyncFunc() async {}

// expected-error@+1{{raising errors from @cxx functions is not supported}}
@cxx @implementation
func throwingFunc() throws {}

// expected-error@+2{{global function cannot be marked '@cxx' because it has generic parameters}}
@cxx @implementation
func genericFunc<T>(_ x: T) -> T { return x }
