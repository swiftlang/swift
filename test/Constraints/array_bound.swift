// RUN: %swift -parse %s -verify

// Basic support for constant array bounds.
func constantArray() -> Int[] {
  return new Int[17]
}

// Basic support for non-constant array bounds.
func simpleArray(n : Int) -> Int[] {
  return new Int[n]
}

// Support for non-stdlib array bounds.
struct MyArrayBound : ArrayBound {
  typealias ArrayBoundType = UInt
  func getArrayBoundValue() -> UInt {
    return 17
  }
}

func nonStdlibArray(n : MyArrayBound) -> Int[] {
  return new Int[n]
}

// Support for arbitrary array bounds in generics.
func genericArray<T : ArrayBound>(n : T) -> Int[] {
  return new Int[n]
}

// Use ArrayBound-ness to resolve overloading.
func getBound() -> MyArrayBound {}
func getBound() -> Float {}

func overloadedArray() -> Int[] {
  return new Int[getBound()];
}
