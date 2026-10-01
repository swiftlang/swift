// RUN: %target-swift-frontend -emit-sil %s -target %target-swift-5.9-abi-triple | %FileCheck %s

// This test verifies that definite initialization correctly handles
// structs with tuple properties containing pack expansions.
// The DI pass must treat such tuples as single opaque elements rather
// than trying to flatten them into individual element addresses.

// MARK: - Basic struct with pack expansion tuple

struct BasicPackTuple<each T> {
  let values: (repeat each T)

  // CHECK-LABEL: sil hidden @$s31definite_init_variadic_generics14BasicPackTupleV
  // CHECK: tuple_pack_element_addr
  init(_ values: repeat each T) {
    self.values = (repeat each values)
  }
}

// MARK: - Struct with multiple properties including pack expansion tuple

struct MixedProperties<each T> {
  let prefix: Int
  let packed: (repeat each T)
  let suffix: String

  // CHECK-LABEL: sil hidden @$s31definite_init_variadic_generics15MixedPropertiesV
  // CHECK: tuple_pack_element_addr
  init(prefix: Int, suffix: String, _ values: repeat each T) {
    self.prefix = prefix
    self.packed = (repeat each values)
    self.suffix = suffix
  }
}

// MARK: - Struct with pack expansion tuple initialized in different branches

struct ConditionalInit<each T> {
  let values: (repeat each T)
  let flag: Bool

  // CHECK-LABEL: sil hidden @$s31definite_init_variadic_generics15ConditionalInitV
  // CHECK: tuple_pack_element_addr
  init(condition: Bool, _ values: repeat each T) {
    self.flag = condition
    if condition {
      self.values = (repeat each values)
    } else {
      self.values = (repeat each values)
    }
  }
}

// MARK: - Nested struct containing pack expansion tuple

struct Outer<each T> {
  struct Inner {
    let data: (repeat each T)

    // CHECK-LABEL: sil hidden @$s31definite_init_variadic_generics5OuterV5InnerV
    // CHECK: tuple_pack_element_addr
    init(_ values: repeat each T) {
      self.data = (repeat each values)
    }
  }

  let inner: Inner

  init(_ values: repeat each T) {
    self.inner = Inner(repeat each values)
  }
}

// MARK: - Class with pack expansion tuple property

class ClassWithPackTuple<each T> {
  let values: (repeat each T)

  // CHECK-LABEL: sil hidden {{.*}}@$s31definite_init_variadic_generics18ClassWithPackTupleC
  // CHECK: tuple_pack_element_addr
  init(_ values: repeat each T) {
    self.values = (repeat each values)
  }
}

// MARK: - Failable initializer with pack expansion tuple

struct FailablePackInit<each T> {
  let values: (repeat each T)

  // CHECK-LABEL: sil hidden @$s31definite_init_variadic_generics16FailablePackInitV
  // CHECK: tuple_pack_element_addr
  init?(_ values: repeat each T, shouldFail: Bool) {
    if shouldFail {
      return nil
    }
    self.values = (repeat each values)
  }
}

// MARK: - Struct with throwing initializer and pack expansion tuple

struct ThrowingPackInit<each T> {
  let values: (repeat each T)

  // CHECK-LABEL: sil hidden @$s31definite_init_variadic_generics16ThrowingPackInitV
  // CHECK: tuple_pack_element_addr
  init(_ values: repeat each T) throws {
    self.values = (repeat each values)
  }
}

// MARK: - Crash case: throwing call AFTER pack tuple assignment
// This is the minimal reproducer for the DI crash. The crash occurs when:
// 1. Struct has multiple properties including a pack expansion tuple
// 2. Pack tuple is assigned first
// 3. A throwing call comes AFTER the pack tuple assignment
// DI must generate cleanup code to destroy the pack tuple if the throw occurs,
// and was incorrectly using tuple_element_addr instead of treating it as opaque.

func produceValue<T>(type: T.Type) -> T { fatalError() }
func throwingCall() throws -> Int { return 0 }

struct ThrowingAfterPackInit<each T> {
  let packed: (repeat each T)
  let other: Int

  // CHECK-LABEL: sil hidden @$s31definite_init_variadic_generics21ThrowingAfterPackInitV
  // CHECK: tuple_pack_element_addr
  init() throws {
    self.packed = (repeat produceValue(type: (each T).self))
    self.other = try throwingCall()
  }
}

// MARK: - Test instantiations to ensure code generation works

func testInstantiations() {
  _ = BasicPackTuple(1, "hello", 3.14)
  _ = MixedProperties(prefix: 42, suffix: "end", true, 2.0)
  _ = ConditionalInit(condition: true, "a", "b")
  _ = Outer(1, 2, 3)
  _ = ClassWithPackTuple("x", "y")
  _ = FailablePackInit(1, 2, shouldFail: false)
  _ = try? ThrowingPackInit(true, false)
}
