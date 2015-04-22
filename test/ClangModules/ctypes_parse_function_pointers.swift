// RUN: %target-parse-verify-swift %clang-importer-sdk -enable-c-function-pointers

import ctypes

func testFunctionPointers() {
  let fp = getFunctionPointer()
  useFunctionPointer(fp)

  let explicitFP: @convention(c) (CInt) -> CInt = fp

  let wrapper: FunctionPointerWrapper = FunctionPointerWrapper(a: nil, b: nil)
  let wrapper2 = FunctionPointerWrapper(a: fp, b: fp)
  useFunctionPointer(wrapper.a)
  let _: @convention(c) (CInt) -> CInt = wrapper.b

  var anotherFP: @convention(c) (CInt, CLong, UnsafeMutablePointer<Void>) -> Void
    = getFunctionPointer2()

  useFunctionPointer2(anotherFP)
  anotherFP = fp // expected-error {{cannot assign a value of type 'fptr!' to a value of type '@convention(c) (CInt, CLong, UnsafeMutablePointer<Void>) -> Void'}}
}

