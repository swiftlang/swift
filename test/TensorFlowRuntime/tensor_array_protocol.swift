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
  expectedBuffer.initialize(from: [w.handle._cTensorHandle, b.handle._cTensorHandle])
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
  expectedBuffer.initialize(
    from: [string.handle._cTensorHandle, float.handle._cTensorHandle, 
           int.handle._cTensorHandle])
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
  expectedBuffer.initialize(
    from: [w.handle._cTensorHandle, b.handle._cTensorHandle, 
           string.handle._cTensorHandle, float.handle._cTensorHandle, 
           int.handle._cTensorHandle])
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
  expectedBuffer.initialize(
    from: [w.handle._cTensorHandle, b.handle._cTensorHandle, 
           string.handle._cTensorHandle, float.handle._cTensorHandle, 
           int.handle._cTensorHandle])
  expectEqual(expectedBuffer[0], buffer[0])
  expectEqual(expectedBuffer[1], buffer[1])
  expectEqual(expectedBuffer[2], buffer[2])
  expectEqual(expectedBuffer[3], buffer[3])
  expectEqual(expectedBuffer[4], buffer[4])
}

TensorArrayProtocolTests.test("NestedGenericTensorHandleCount") {
  struct NestedGeneric {
    func function() {
      struct UltraNested<T: TensorArrayProtocol, V: TensorArrayProtocol> : TensorArrayProtocol {
        var a: Generic<T, V>
        var b: Generic<V, T>
      }
      let w = Tensor<Float>(0.1)
      let b = Tensor<Float>(0.1)
      let simple = Simple(w: w, b: b)
      let string = StringTensor("Test")
      let float = Tensor<Float>(0.1)
      let int = Tensor<Int32>(1)
      let mixed = Mixed(string: string, float: float, int: int)
      let genericSM = Generic<Simple, Mixed>(t: simple, u: mixed)
      let genericMS = Generic<Mixed, Simple>(t: mixed, u: simple)
      let nested = UltraNested<Simple, Mixed>(a: genericSM, b: genericMS)
      expectEqual(10, nested._tensorHandleCount)
    }
  }

  NestedGeneric().function()
}

TensorArrayProtocolTests.test("NestedGenericUnpackTensorHandles") {
  struct NestedGeneric {
    func function() {
      struct UltraNested<T: TensorArrayProtocol, V: TensorArrayProtocol> : TensorArrayProtocol {
        var a: Generic<T, V>
        var b: Generic<V, T>
      }
      let w = Tensor<Float>(0.1)
      let b = Tensor<Float>(0.1)
      let simple = Simple(w: w, b: b)
      let string = StringTensor("Test")
      let float = Tensor<Float>(0.1)
      let int = Tensor<Int32>(1)
      let mixed = Mixed(string: string, float: float, int: int)
      let genericSM = Generic<Simple, Mixed>(t: simple, u: mixed)
      let genericMS = Generic<Mixed, Simple>(t: mixed, u: simple)
      let nested = UltraNested<Simple, Mixed>(a: genericSM, b: genericMS)
      let buffer = UnsafeMutablePointer<CTensorHandle>.allocate(capacity: 10)
      nested._unpackTensorHandles(into: buffer)
      let expectedBuffer = UnsafeMutableBufferPointer<CTensorHandle>.allocate(capacity: 10)
      expectedBuffer.initialize(
        from: [w.handle._cTensorHandle, b.handle._cTensorHandle, 
               string.handle._cTensorHandle, float.handle._cTensorHandle, 
               int.handle._cTensorHandle, string.handle._cTensorHandle, 
               float.handle._cTensorHandle, int.handle._cTensorHandle, 
               w.handle._cTensorHandle, b.handle._cTensorHandle])
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
    }
  }

  NestedGeneric().function()
}

runAllTests()
