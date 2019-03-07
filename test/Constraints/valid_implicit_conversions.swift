// RUN: %target-typecheck-verify-swift

func takesAutoclosure<T>(_ lhs: T, _ rhs: @autoclosure () throws -> T) {}
func takesUnsafeRawPointer(_ ptr: UnsafeRawPointer) {}
func takesUnsafeMutableRawPointer(_ ptr: UnsafeMutableRawPointer) {}
func takesUnsafePointer<T>(_ ptr: UnsafePointer<T>) {}
func takesUnsafeMutablePointer<T>(_ ptr: UnsafeMutablePointer<T>) {}
func takesUnsafePointerInt8(_ ptr: UnsafePointer<Int8>) {}
func takesUnsafePointerUInt8(_ ptr: UnsafePointer<UInt8>) {}
func takesUnsafePointerVoid(_ ptr: UnsafePointer<Void>) {} // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}

func test(
  _ rawPtr: UnsafeRawPointer,
  _ mutRawPtr: UnsafeMutableRawPointer,
  _ mutPtr: UnsafeMutablePointer<Int>,
  _ ptr: UnsafePointer<Int>
) {
  var i: Int = 0
  var a: [Int] = [0]
  let s = "string"

  takesUnsafeRawPointer(&i)
  takesUnsafeMutableRawPointer(&i)
  takesUnsafeMutablePointer(&i)
  takesUnsafePointer(&i)
  takesUnsafeRawPointer(&a)
  takesUnsafeMutableRawPointer(&a)
  takesUnsafeMutablePointer(&a)
  takesUnsafePointer(&a)

  takesUnsafeRawPointer(mutPtr)
  takesUnsafeMutableRawPointer(mutPtr)
  takesUnsafePointer(mutPtr)

  takesUnsafeRawPointer(mutRawPtr)

  takesUnsafeRawPointer(a)
  takesUnsafePointer(a)

  takesAutoclosure(rawPtr, mutPtr)
  takesAutoclosure(mutRawPtr, mutPtr)
  takesAutoclosure(ptr, mutPtr)
  takesAutoclosure(rawPtr, mutRawPtr)

  takesUnsafeRawPointer(s)
  takesUnsafePointerInt8(s)
  takesUnsafePointerUInt8(s)
  takesUnsafePointerVoid(s)
}
