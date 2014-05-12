// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %swift %clang-importer-sdk -parse -verify -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -I %S/Inputs %s
// RUN: ls -lR %t/clang-module-cache | FileCheck %s
// CHECK: cfuncs{{.*}}.pcm

import cfuncs

func test_cfunc1(i: Int) {
  cfunc1() // okay
  cfunc1(i) // expected-error{{cannot convert the expression's type '$T2' to type '() -> Void'}}
}

func test_cfunc2(i: Int) {
  var f = cfunc2(i, 17)
  var f2 : Float = f
  cfunc2(b:17, a:i) // expected-error{{cannot convert the expression's type '$T3' to type 'CLong'}}
}

func test_cfunc3_a() {
  var b = cfunc3( { (a : Double, b : Double) -> Double in a + b } )
  var d : Double = b(1.5, 2.5)
  var d1 : Double = b // expected-error{{'double_bin_op_block' is not convertible to 'Double'}}
}

func test_cfunc3_b() {
  var b = cfunc3( { a, b in a + b } )
  var d : Double = b(1.5, 2.5)
  var d1 : Double = b // expected-error{{'double_bin_op_block' is not convertible to 'Double'}}
}

func test_cfunc3_c() {
  var b = cfunc3({ $0 + $1 })
  var d : Double = b(1.5, 2.5)
  var d1 : Double = b // expected-error{{'double_bin_op_block' is not convertible to 'Double'}}
}

func test_cfunc3_d() {
  var x: Double = 0
  var y: Double = 0
  var z: Double? = cfunc3(nil)?(x, y)
  var w: Double = cfunc3(nil)!(x, y)
}

func test_cfunc4() {
  // Okay: has no prototype, so assume no parameters.
  cfunc4()
}

func test_pow() {
  pow(1.5, 2.5)
}

func test_puts(s: String) {
  var i = s.withCString { puts($0) + 32 };
}

func test_fopen(filename: String) -> CInt {
  var file = filename.withCString { fopen($0, "r") }
  return file.get().inode
}

func test_cfunc_in_swift() -> Int {
  return cfunc_in_swift(5)
}

func test_inline_available() {
  createSomething()
}

func test_pointer() {
  var i: CInt = 0
  var ia: CInt[] = [1, 2, 3]
  var f: CFloat = 0
  var fa: CFloat[] = [1, 2, 3]

  param_pointer(UnsafePointer<CInt>())
  param_pointer(&i)
  param_pointer(&ia)

  param_const_pointer(UnsafePointer<CInt>())
  param_const_pointer(&i)
  param_const_pointer(ia)
  param_const_pointer([1, 2, 3])

  param_void_pointer(COpaquePointer())
  param_void_pointer(UnsafePointer<CInt>())
  param_void_pointer(UnsafePointer<CFloat>())
  param_void_pointer(&i)
  param_void_pointer(&ia)
  param_void_pointer(&f)
  param_void_pointer(&fa)

  param_const_void_pointer(COpaquePointer())
  param_const_void_pointer(UnsafePointer<CInt>())
  param_const_void_pointer(UnsafePointer<CFloat>())
  param_const_void_pointer(&i)
  param_const_void_pointer(ia)
  param_const_void_pointer([1, 2, 3])
  param_const_void_pointer(&f)
  param_const_void_pointer(fa)
  param_const_void_pointer([1.0, 2.0, 3.0])
}

func test_decay() {
  decay_param_array(UnsafePointer<CInt>())
  var i: CInt = 0
  var a: CInt[] = [1, 2, 3]
  decay_param_array(&i)
  decay_param_array(&a)
  decay_param_const_array(UnsafePointer<CInt>())
  decay_param_const_array(&i)
  decay_param_const_array(a)
  decay_param_const_array([1, 2, 3])
}

func exit(_: Float) {} // expected-note {{found this candidate}}
func test_ambiguous() {
  exit(5) // expected-error {{ambiguous use of 'exit'}}
}

