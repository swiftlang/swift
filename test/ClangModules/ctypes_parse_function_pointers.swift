// RUN: %target-parse-verify-swift %clang-importer-sdk -enable-c-function-pointers

// XFAIL: linux

import ctypes

func testFunctionPointers() {
  let fp = getFunctionPointer()
  useFunctionPointer(fp)

  let explicitFP: @cc(cdecl) (CInt) -> CInt = fp

  let wrapper: FunctionPointerWrapper = FunctionPointerWrapper(a: nil, b: nil)
  let wrapper2 = FunctionPointerWrapper(a: fp, b: fp)
  useFunctionPointer(wrapper.a)
  let _: @cc(cdecl) (CInt) -> CInt = wrapper.b

  var anotherFP: @cc(cdecl) (CInt, CLong, UnsafeMutablePointer<Void>) -> Void
    = getFunctionPointer2()

  useFunctionPointer2(anotherFP)
  anotherFP = fp // expected-error {{cannot assign a value of type 'fptr!' to a value of type '@cc(cdecl) (CInt, CLong, UnsafeMutablePointer<Void>) -> Void'}}
}

