// RUN: %target-typecheck-verify-swift

// If we have something like this, where the two conversions have different kinds:
//
//   Array<T> arg conv $T1
//   $T1 subtype UnsafePointer<T>
//
// We have to bind $T1 to UnsafePointer<T> and not Array<T>, because
// subtype constraints do not allow array-to-pointer conversions.

protocol Trivial {}

extension Optional: Trivial {}

extension UnsafePointer: Trivial {}

extension AnyHashable: Trivial {}

extension UnsafeRawPointer: Trivial {}

extension UnsafeMutableRawPointer: Trivial {}

func test_inference_through_implicit_conversion() {
  struct C : Hashable {}

  func id<T: Trivial>(_ t: T) -> T { return t }

  var arr: [C] = []
  let ptr: UnsafeMutablePointer<C> = UnsafeMutablePointer(bitPattern: 0)!
  let rawPtr: UnsafeMutableRawPointer = UnsafeMutableRawPointer(bitPattern: 0)!

  let _: C? = id(C()) // Ok -> argument is implicitly promoted into an optional
  let _: UnsafePointer<C> = id([C()]) // Ok - argument is implicitly converted to a pointer
  let _: UnsafeRawPointer = id([C()]) // Ok - argument is implicitly converted to a raw pointer
  let _: UnsafeMutableRawPointer = id(&arr) // Ok - inout Array<T> -> UnsafeMutableRawPointer
  let _: UnsafePointer<C> = id(ptr) // Ok - UnsafeMutablePointer<T> -> UnsafePointer<T>
  let _: UnsafeRawPointer = id(ptr) // Ok - UnsafeMutablePointer<T> -> UnsafeRawPointer
  let _: UnsafeRawPointer = id(rawPtr) // Ok - UnsafeMutableRawPointer -> UnsafeRawPointer
  let _: UnsafeMutableRawPointer = id(ptr) // Ok - UnsafeMutablePointer<T> -> UnsafeMutableRawPointer
  let _: AnyHashable = id(C()) // Ok - argument is implicitly converted to `AnyHashable` because it's Hashable
}

// Make sure that conformances transitively checked through implicit conversions work with conditional requirements
protocol TestCond {}

extension Optional : TestCond where Wrapped == Int? {}

func simple<T : TestCond>(_ x: T) -> T { x }

func overloaded<T: TestCond>(_ x: T) -> T { x }
func overloaded<T: TestCond>(_ x: String) -> T { fatalError() }

func overloaded_result() -> Int { 42 }
func overloaded_result() -> String { "" }

func test_arg_conformance_with_conditional_reqs(i: Int) {
  let _: Int?? = simple(i)
  let _: Int?? = overloaded(i)
  let _: Int?? = simple(overloaded_result())
  let _: Int?? = overloaded(overloaded_result())
}

// Reduced from Accelerate's swiftinterface

func processData<T>(_: T, _: (T) -> ()) {}

func transform(_: UnsafePointer<UInt8>) {}
func transformMut(_: UnsafeMutablePointer<UInt8>) {}
func transformOpt(_: UnsafePointer<UInt8>?) {}
func transformMutOpt(_: UnsafeMutablePointer<UInt8>?) {}

func test_argconv_vs_subtype(_ arr: inout [UInt8]) {
  processData(arr, transform)
  processData(&arr, transformMut)
  processData(arr, transformOpt)
  processData(&arr, transformMutOpt)
}
