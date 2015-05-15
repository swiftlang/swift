// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -verify -I %S/Inputs %s

// XFAIL: linux

import cfuncs

func test_cfunc1(i: Int) {
  cfunc1() // okay
  cfunc1(i) // expected-error{{cannot invoke 'cfunc1' with an argument list of type '(Int)'}}
}

func test_cfunc2(i: Int) {
  let f = cfunc2(i, 17)
  _ = f as Float
  // FIXME: Should report this error: {{cannot convert the expression's type '$T3' to type 'CLong'}}
  cfunc2(b:17, a:i) // expected-error{{cannot invoke 'cfunc2' with an argument list of type '(b: Int, a: Int)'}} expected-note {{expected an argument list of type '(Int, Int32)'}}
}

func test_cfunc3_a() {
  let b = cfunc3( { (a : Double, b : Double) -> Double in a + b } )
  _ = b(1.5, 2.5) as Double
  _ = b as Double// expected-error{{'double_bin_op_block!' is not convertible to 'Double'}}
}

func test_cfunc3_b() {
  let b = cfunc3( { a, b in a + b } )
  _ = b(1.5, 2.5) as Double
  _ = b as Double// expected-error{{'double_bin_op_block!' is not convertible to 'Double'}}
}

func test_cfunc3_c() {
  let b = cfunc3({ $0 + $1 })
  _ = b(1.5, 2.5) as Double
  _ = b as Double// expected-error{{'double_bin_op_block!' is not convertible to 'Double'}}
}

func test_cfunc3_d() {
  let x: Double = 0
  let y: Double = 0
  _ = cfunc3(nil)?(x, y) as Double?
  _ = cfunc3(nil)!(x, y) as Double
}

func test_cfunc4() {
  // Okay: has no prototype, so assume no parameters.
  cfunc4()
}

func test_pow() {
  pow(1.5, 2.5)
}

func test_puts(s: String) {
  _ = s.withCString { puts($0) + 32 };
}

func test_fopen(filename: String) -> CInt {
  let file = filename.withCString { fopen($0, "r") }
  return file.memory.inode
}

func test_cfunc_in_swift() -> Int {
  return cfunc_in_swift(5)
}

func test_inline_available() {
  createSomething()
}

func test_pointer() {
  var i: CInt = 0
  var ia: [CInt] = [1, 2, 3]
  var f: CFloat = 0
  var fa: [CFloat] = [1, 2, 3]

  param_pointer(UnsafeMutablePointer<CInt>())
  param_pointer(&i)
  param_pointer(&ia)

  param_const_pointer(UnsafeMutablePointer<CInt>())
  param_const_pointer(&i)
  param_const_pointer(ia)
  param_const_pointer([1, 2, 3])

  param_void_pointer(UnsafeMutablePointer<Void>())
  param_void_pointer(UnsafeMutablePointer<CInt>())
  param_void_pointer(UnsafeMutablePointer<CFloat>())
  param_void_pointer(&i)
  param_void_pointer(&ia)
  param_void_pointer(&f)
  param_void_pointer(&fa)

  param_const_void_pointer(UnsafeMutablePointer<Void>())
  param_const_void_pointer(UnsafeMutablePointer<CInt>())
  param_const_void_pointer(UnsafeMutablePointer<CFloat>())
  param_const_void_pointer(UnsafePointer<Void>())
  param_const_void_pointer(UnsafePointer<CInt>())
  param_const_void_pointer(UnsafePointer<CFloat>())
  param_const_void_pointer(&i)
  param_const_void_pointer(ia)
  // FIXME: param_const_void_pointer([1, 2, 3])
  param_const_void_pointer(&f)
  param_const_void_pointer(fa)
  // FIXME: param_const_void_pointer([1.0, 2.0, 3.0])

  let op = COpaquePointer()
  opaque_pointer_param(op)
}

func test_decay() {
  decay_param_array(UnsafeMutablePointer<CInt>())
  var i: CInt = 0
  var a: [CInt] = [1, 2, 3]
  decay_param_array(&i)
  decay_param_array(&a)
  decay_param_const_array(UnsafeMutablePointer<CInt>())
  decay_param_const_array(&i)
  decay_param_const_array(a)
  decay_param_const_array([1, 2, 3])
}

func exit(_: Float) {} // expected-note {{found this candidate}}
func test_ambiguous() {
  exit(5) // expected-error {{ambiguous use of 'exit'}}
}

