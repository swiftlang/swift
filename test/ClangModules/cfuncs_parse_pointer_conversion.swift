// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %swift -enable-pointer-conversions %clang-importer-sdk -parse -verify -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -I %S/Inputs %s

import cfuncs
import ctypes

func test_pointer() {
  var i: CInt = 0
  var ia: [CInt] = [1, 2, 3]
  var f: CFloat = 0
  var fa: [CFloat] = [1, 2, 3]

  param_pointer(UnsafePointer<CInt>())
  param_pointer(&i)
  param_pointer(&ia)

  param_const_pointer(UnsafePointer<CInt>())
  param_const_pointer(&i)
  param_const_pointer(ia)
  param_const_pointer([1, 2, 3])

  param_void_pointer(UnsafePointer<Void>())
  param_void_pointer(UnsafePointer<CInt>())
  param_void_pointer(UnsafePointer<CFloat>())
  param_void_pointer(&i)
  param_void_pointer(&ia)
  param_void_pointer(&f)
  param_void_pointer(&fa)

  param_const_void_pointer(UnsafePointer<Void>())
  param_const_void_pointer(UnsafePointer<CInt>())
  param_const_void_pointer(UnsafePointer<CFloat>())
  param_const_void_pointer(&i)
  param_const_void_pointer(ia)
  // FIXME param_const_void_pointer([1, 2, 3])
  param_const_void_pointer(&f)
  param_const_void_pointer(fa)
  // FIXME param_const_void_pointer([1.0, 2.0, 3.0])

  var op = COpaquePointer()
  opaque_pointer_param(op)
}

func test_decay() {
  decay_param_array(UnsafePointer<CInt>())
  var i: CInt = 0
  var a: [CInt] = [1, 2, 3]
  decay_param_array(&i)
  decay_param_array(&a)
  decay_param_const_array(UnsafePointer<CInt>())
  decay_param_const_array(&i)
  decay_param_const_array(a)
  decay_param_const_array([1, 2, 3])
}

func testFunctionPointers() {
  let fp: CFunctionPointer = getFunctionPointer()
  useFunctionPointer(fp)
}
