// RUN: %target-run-simple-swift(-target %target-swift-5.9-abi-triple)

// REQUIRES: executable_test

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// This test verifies runtime behavior of structs with pack expansion tuple
// properties, ensuring that definite initialization correctly handles
// the initialization and that values are accessible at runtime.

import StdlibUnittest

var suite = TestSuite("VariadicGenericTupleInit")

// MARK: - Basic struct with pack expansion tuple

struct BasicPackTuple<each T> {
  let values: (repeat each T)

  init(_ values: repeat each T) {
    self.values = (repeat each values)
  }
}

suite.test("BasicPackTuple single element") {
  let s = BasicPackTuple(42)
  expectEqual(42, s.values)
}

suite.test("BasicPackTuple multiple elements") {
  let s = BasicPackTuple(1, "hello", true)
  expectEqual(1, s.values.0)
  expectEqual("hello", s.values.1)
  expectEqual(true, s.values.2)
}

// MARK: - Struct with mixed properties

struct MixedProperties<each T> {
  let prefix: Int
  let packed: (repeat each T)
  let suffix: String

  init(prefix: Int, suffix: String, _ values: repeat each T) {
    self.prefix = prefix
    self.packed = (repeat each values)
    self.suffix = suffix
  }
}

suite.test("MixedProperties") {
  let s = MixedProperties(prefix: 10, suffix: "end", 3.14, "middle")
  expectEqual(10, s.prefix)
  expectEqual(3.14, s.packed.0)
  expectEqual("middle", s.packed.1)
  expectEqual("end", s.suffix)
}

// MARK: - Conditional initialization

struct ConditionalInit<each T> {
  let values: (repeat each T)
  let flag: Bool

  init(condition: Bool, _ values: repeat each T) {
    self.flag = condition
    if condition {
      self.values = (repeat each values)
    } else {
      self.values = (repeat each values)
    }
  }
}

suite.test("ConditionalInit true branch") {
  let s = ConditionalInit(condition: true, "a", "b")
  expectTrue(s.flag)
  expectEqual("a", s.values.0)
  expectEqual("b", s.values.1)
}

suite.test("ConditionalInit false branch") {
  let s = ConditionalInit(condition: false, 1, 2, 3)
  expectFalse(s.flag)
  expectEqual(1, s.values.0)
  expectEqual(2, s.values.1)
  expectEqual(3, s.values.2)
}

// MARK: - Nested struct

struct Outer<each T> {
  struct Inner {
    let data: (repeat each T)

    init(_ values: repeat each T) {
      self.data = (repeat each values)
    }
  }

  let inner: Inner

  init(_ values: repeat each T) {
    self.inner = Inner(repeat each values)
  }
}

suite.test("Nested struct") {
  let s = Outer(100, 200)
  expectEqual(100, s.inner.data.0)
  expectEqual(200, s.inner.data.1)
}

// MARK: - Class with pack expansion tuple

class ClassWithPackTuple<each T> {
  let values: (repeat each T)

  init(_ values: repeat each T) {
    self.values = (repeat each values)
  }
}

suite.test("Class with pack tuple") {
  let c = ClassWithPackTuple("x", "y", "z")
  expectEqual("x", c.values.0)
  expectEqual("y", c.values.1)
  expectEqual("z", c.values.2)
}

// MARK: - Failable initializer

struct FailablePackInit<each T> {
  let values: (repeat each T)

  init?(_ values: repeat each T, shouldFail: Bool) {
    if shouldFail {
      return nil
    }
    self.values = (repeat each values)
  }
}

suite.test("Failable init success") {
  let s = FailablePackInit(1, 2, shouldFail: false)
  expectNotNil(s)
  expectEqual(1, s!.values.0)
  expectEqual(2, s!.values.1)
}

suite.test("Failable init failure") {
  let s = FailablePackInit(1, 2, shouldFail: true)
  expectNil(s)
}

// MARK: - Throwing initializer

struct ThrowingPackInit<each T> {
  let values: (repeat each T)

  init(_ values: repeat each T) throws {
    self.values = (repeat each values)
  }
}

suite.test("Throwing init") {
  do {
    let s = try ThrowingPackInit("a", "b")
    expectEqual("a", s.values.0)
    expectEqual("b", s.values.1)
  } catch {
    expectUnreachable("Should not throw")
  }
}

// MARK: - Throwing initializer with pack expansion tuple (crash case)
// This is a minimal reproducer for the DI crash. The crash occurs when:
// 1. Struct has multiple properties including a pack expansion tuple
// 2. Throwing initializer where a throwing call comes AFTER the pack tuple assignment
// 3. DI generates cleanup code that incorrectly uses tuple_element_addr on the pack tuple

func produceValue<T>(type: T.Type) -> T {
  fatalError("not called in test")
}

func produceInt() throws -> Int {
  return 42
}

struct ThrowingContainer<each Input> {
  let inputs: (repeat each Input)
  let tag: Int

  init() throws {
    self.inputs = (repeat produceValue(type: (each Input).self))
    self.tag = try produceInt()
  }

  init(inputs: repeat each Input, tag: Int) {
    self.inputs = (repeat each inputs)
    self.tag = tag
  }
}

suite.test("Throwing init with pack tuple - success path") {
  // Use the non-throwing init for runtime test
  let c = ThrowingContainer<Int, String>(inputs: 42, "test", tag: 99)
  expectEqual(42, c.inputs.0)
  expectEqual("test", c.inputs.1)
  expectEqual(99, c.tag)
}

runAllTests()
