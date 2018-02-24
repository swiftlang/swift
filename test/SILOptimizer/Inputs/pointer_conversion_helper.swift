// Opaque, unoptimizable functions to call.
func takesConstRawPointer(_ x: UnsafeRawPointer) {}
func takesOptConstRawPointer(_ x: UnsafeRawPointer?) {}
func takesMutableRawPointer(_ x: UnsafeMutableRawPointer) {}
func takesOptMutableRawPointer(_ x: UnsafeMutableRawPointer?) {}

// Opaque function for generating values
func get<T>() -> T { fatalError() }
