// RUN: %target-swift-frontend -emit-silgen -parse-stdlib %s
// XFAIL: *

// SIL code with older, deprecated autodiff ABI for linear map context
// allocation should fail, complaining about receiving a 'Builtin.Word' when 
// a 'T.Type' is expected.

struct ExamplePullbackStruct<T: Differentiable> {
  var pb0: (T.TangentVector) -> T.TangentVector
}

@_silgen_name("test_context_builtins_bad")
func test_context_builtins_bad() {
  let pbStruct = ExamplePullbackStruct<Float>(pb0: { $0 })
  let context = Builtin.autoDiffCreateLinearMapContext(Builtin.sizeof(type(of: pbStruct)))
  let topLevelSubctxAddr = Builtin.autoDiffProjectTopLevelSubcontext(context)
  UnsafeMutableRawPointer(topLevelSubctxAddr).storeBytes(of: pbStruct, as: type(of: pbStruct))
  let newBuffer = Builtin.autoDiffAllocateSubcontext(context, Builtin.sizeof(type(of: pbStruct)))
  UnsafeMutableRawPointer(newBuffer).storeBytes(of: pbStruct, as: type(of: pbStruct))
}
