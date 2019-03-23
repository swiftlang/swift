// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 5 -g %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test
//
// `TensorArrayProtocol` tests.

import TensorFlow
import StdlibUnittest

var TensorArrayProtocolTests = TestSuite("TensorArrayProtocol")

struct Empty : TensorArrayProtocol {}

struct Simple : TensorArrayProtocol {
  var w, b: Tensor<Float>
}

struct Mixed : TensorArrayProtocol {
  // Mutable.
  var string: StringTensor
  var float: Tensor<Float>
  // Immutable.
  let int: Tensor<Int32>
}

struct Nested : TensorArrayProtocol {
  // Immutable.
  let simple: Simple
  // Mutable.
  var mixed: Mixed
}

struct Generic<T: TensorArrayProtocol, U: TensorArrayProtocol> : TensorArrayProtocol {
  var t: T
  var u: U
}

struct NestedGeneric<T: TensorArrayProtocol> {
  struct Nested<U: TensorArrayProtocol> {
    struct UltraNested<V: TensorArrayProtocol> : TensorArrayProtocol {
      var a: Generic<T, U>
      var b: Generic<U, V>
      var c: Generic<T, V>
    }
  }
}

TensorArrayProtocolTests.test("EmptyTensorHandleCount") {
  expectEqual(0, Empty()._tensorHandleCount)
}

TensorArrayProtocolTests.test("SimpleTensorHandleCount") {
  let w = Tensor<Float>(0.1)
  let b = Tensor<Float>(0.1)
  let simple = Simple(w: w, b: b)
  expectEqual(2, simple._tensorHandleCount)
}

TensorArrayProtocolTests.test("SimpleUnpackTensorHandles") {
  let w = Tensor<Float>(0.1)
  let b = Tensor<Float>(0.1)
  let simple = Simple(w: w, b: b)
  let buffer = UnsafeMutablePointer<CTensorHandle>.allocate(capacity: 2)
  simple._unpackTensorHandles(into: buffer)
  let expectedBuffer = UnsafeMutableBufferPointer<CTensorHandle>.allocate(capacity: 2)
  expectedBuffer.baseAddress?.initialize(to: w.handle._cTensorHandle)
  expectedBuffer.baseAddress?.advanced(by: 1).initialize(to: b.handle._cTensorHandle)
  expectEqual(expectedBuffer[0], buffer[0])
  expectEqual(expectedBuffer[1], buffer[1])
}

TensorArrayProtocolTests.test("MixedTensorHandleCount") {
  let string = StringTensor("Test")
  let float = Tensor<Float>(0.1)
  let int = Tensor<Int32>(1)
  let mixed = Mixed(string: string, float: float, int: int)
  expectEqual(3, mixed._tensorHandleCount)
}

TensorArrayProtocolTests.test("MixedUnpackTensorHandles") {
  let string = StringTensor("Test")
  let float = Tensor<Float>(0.1)
  let int = Tensor<Int32>(1)
  let mixed = Mixed(string: string, float: float, int: int)
  let buffer = UnsafeMutablePointer<CTensorHandle>.allocate(capacity: 3)
  mixed._unpackTensorHandles(into: buffer)
  let expectedBuffer = UnsafeMutableBufferPointer<CTensorHandle>.allocate(capacity: 3)
  expectedBuffer.baseAddress?.initialize(to: string.handle._cTensorHandle)
  expectedBuffer.baseAddress?.advanced(by: 1).initialize(to: float.handle._cTensorHandle)
  expectedBuffer.baseAddress?.advanced(by: 2).initialize(to: int.handle._cTensorHandle)
  expectEqual(expectedBuffer[0], buffer[0])
  expectEqual(expectedBuffer[1], buffer[1])
  expectEqual(expectedBuffer[2], buffer[2])
}

TensorArrayProtocolTests.test("NestedTensorHandleCount") {
  let w = Tensor<Float>(0.1)
  let b = Tensor<Float>(0.1)
  let simple = Simple(w: w, b: b)
  let string = StringTensor("Test")
  let float = Tensor<Float>(0.1)
  let int = Tensor<Int32>(1)
  let mixed = Mixed(string: string, float: float, int: int)
  let nested = Nested(simple: simple, mixed: mixed)
  expectEqual(5, nested._tensorHandleCount)
}

TensorArrayProtocolTests.test("NestedUnpackTensorHandles") {
  let w = Tensor<Float>(0.1)
  let b = Tensor<Float>(0.1)
  let simple = Simple(w: w, b: b)
  let string = StringTensor("Test")
  let float = Tensor<Float>(0.1)
  let int = Tensor<Int32>(1)
  let mixed = Mixed(string: string, float: float, int: int)
  let nested = Nested(simple: simple, mixed: mixed)
  let buffer = UnsafeMutablePointer<CTensorHandle>.allocate(capacity: 5)
  nested._unpackTensorHandles(into: buffer)
  let expectedBuffer = UnsafeMutableBufferPointer<CTensorHandle>.allocate(capacity: 5)
  expectedBuffer.baseAddress?.initialize(to: w.handle._cTensorHandle)
  expectedBuffer.baseAddress?.advanced(by: 1).initialize(to: b.handle._cTensorHandle)
  expectedBuffer.baseAddress?.advanced(by: 2).initialize(to: string.handle._cTensorHandle)
  expectedBuffer.baseAddress?.advanced(by: 3).initialize(to: float.handle._cTensorHandle)
  expectedBuffer.baseAddress?.advanced(by: 4).initialize(to: int.handle._cTensorHandle)
  expectEqual(expectedBuffer[0], buffer[0])
  expectEqual(expectedBuffer[1], buffer[1])
  expectEqual(expectedBuffer[2], buffer[2])
  expectEqual(expectedBuffer[3], buffer[3])
  expectEqual(expectedBuffer[4], buffer[4])
}

TensorArrayProtocolTests.test("GenericTensorHandleCount") {
  let w = Tensor<Float>(0.1)
  let b = Tensor<Float>(0.1)
  let simple = Simple(w: w, b: b)
  let string = StringTensor("Test")
  let float = Tensor<Float>(0.1)
  let int = Tensor<Int32>(1)
  let mixed = Mixed(string: string, float: float, int: int)
  let generic = Generic<Simple, Mixed>(t: simple, u: mixed)
  expectEqual(5, generic._tensorHandleCount)
}

TensorArrayProtocolTests.test("GenericUnpackTensorHandles") {
  let w = Tensor<Float>(0.1)
  let b = Tensor<Float>(0.1)
  let simple = Simple(w: w, b: b)
  let string = StringTensor("Test")
  let float = Tensor<Float>(0.1)
  let int = Tensor<Int32>(1)
  let mixed = Mixed(string: string, float: float, int: int)
  let generic = Generic<Simple, Mixed>(t: simple, u: mixed)
  let buffer = UnsafeMutablePointer<CTensorHandle>.allocate(capacity: 5)
  generic._unpackTensorHandles(into: buffer)
  let expectedBuffer = UnsafeMutableBufferPointer<CTensorHandle>.allocate(capacity: 5)
  expectedBuffer.baseAddress?.initialize(to: w.handle._cTensorHandle)
  expectedBuffer.baseAddress?.advanced(by: 1).initialize(to: b.handle._cTensorHandle)
  expectedBuffer.baseAddress?.advanced(by: 2).initialize(to: string.handle._cTensorHandle)
  expectedBuffer.baseAddress?.advanced(by: 3).initialize(to: float.handle._cTensorHandle)
  expectedBuffer.baseAddress?.advanced(by: 4).initialize(to: int.handle._cTensorHandle)
  expectEqual(expectedBuffer[0], buffer[0])
  expectEqual(expectedBuffer[1], buffer[1])
  expectEqual(expectedBuffer[2], buffer[2])
  expectEqual(expectedBuffer[3], buffer[3])
  expectEqual(expectedBuffer[4], buffer[4])
}

TensorArrayProtocolTests.test("NestedGenericTensorHandleCount") {
  let w = Tensor<Float>(0.1)
  let b = Tensor<Float>(0.1)
  let simple = Simple(w: w, b: b)
  let string = StringTensor("Test")
  let float = Tensor<Float>(0.1)
  let int = Tensor<Int32>(1)
  let mixed = Mixed(string: string, float: float, int: int)
  let genericSM = Generic<Simple, Mixed>(t: simple, u: mixed)
  let genericMF = Generic<Mixed, Tensor<Float>>(t: mixed, u: w)
  let genericSF = Generic<Simple, Tensor<Float>>(t: simple, u: w)
  let nested = NestedGeneric<Simple>.Nested<Mixed>.UltraNested<Tensor<Float>>(
    a: genericSM, b: genericMF, c: genericSF)
  expectEqual(12, nested._tensorHandleCount)
}

TensorArrayProtocolTests.test("NestedGenericUnpackTensorHandles") {
  let w = Tensor<Float>(0.1)
  let b = Tensor<Float>(0.1)
  let simple = Simple(w: w, b: b)
  let string = StringTensor("Test")
  let float = Tensor<Float>(0.1)
  let int = Tensor<Int32>(1)
  let mixed = Mixed(string: string, float: float, int: int)
  let genericSM = Generic<Simple, Mixed>(t: simple, u: mixed)
  let genericMF = Generic<Mixed, Tensor<Float>>(t: mixed, u: w)
  let genericSF = Generic<Simple, Tensor<Float>>(t: simple, u: w)
  let generic = NestedGeneric.Nested.UltraNested(a: genericSM, b: genericMF, c: genericSF)
  let buffer = UnsafeMutablePointer<CTensorHandle>.allocate(capacity: 12)
  generic._unpackTensorHandles(into: buffer)
  let expectedBuffer = UnsafeMutableBufferPointer<CTensorHandle>.allocate(capacity: 12)
  expectedBuffer.baseAddress?.initialize(to: w.handle._cTensorHandle)
  expectedBuffer.baseAddress?.advanced(by: 1).initialize(to: b.handle._cTensorHandle)
  expectedBuffer.baseAddress?.advanced(by: 2).initialize(to: string.handle._cTensorHandle)
  expectedBuffer.baseAddress?.advanced(by: 3).initialize(to: float.handle._cTensorHandle)
  expectedBuffer.baseAddress?.advanced(by: 4).initialize(to: int.handle._cTensorHandle)
  expectedBuffer.baseAddress?.advanced(by: 5).initialize(to: string.handle._cTensorHandle)
  expectedBuffer.baseAddress?.advanced(by: 6).initialize(to: float.handle._cTensorHandle)
  expectedBuffer.baseAddress?.advanced(by: 7).initialize(to: int.handle._cTensorHandle)
  expectedBuffer.baseAddress?.advanced(by: 8).initialize(to: w.handle._cTensorHandle)
  expectedBuffer.baseAddress?.advanced(by: 9).initialize(to: w.handle._cTensorHandle)
  expectedBuffer.baseAddress?.advanced(by: 10).initialize(to: b.handle._cTensorHandle)
  expectedBuffer.baseAddress?.advanced(by: 11).initialize(to: w.handle._cTensorHandle)
  expectEqual(expectedBuffer[0], buffer[0])
  expectEqual(expectedBuffer[1], buffer[1])
  expectEqual(expectedBuffer[2], buffer[2])
  expectEqual(expectedBuffer[3], buffer[3])
  expectEqual(expectedBuffer[4], buffer[4])
  expectEqual(expectedBuffer[5], buffer[5])
  expectEqual(expectedBuffer[6], buffer[6])
  expectEqual(expectedBuffer[7], buffer[7])
  expectEqual(expectedBuffer[8], buffer[8])
  expectEqual(expectedBuffer[9], buffer[9])
  expectEqual(expectedBuffer[10], buffer[10])
  expectEqual(expectedBuffer[11], buffer[11])
}

runAllTests()
