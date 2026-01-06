// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking)
// REQUIRES: executable_test

// UNSUPPORTED: CPU=arm64e
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest

let testSuite = TestSuite("CheckedCreateType")

struct Variadic<each T> {
  struct Nested<U, each V: Equatable> {}
}

struct Value<let N: Int, let M: Int> {}

@_silgen_name("swift_allocateMetadataPack")
func allocateMetadataPack(
  _ packPointer: UnsafeRawPointer,
  _ packCount: UInt
) -> UnsafeRawPointer

@_silgen_name("_swift_instantiateCheckedGenericMetadata")
func _instantiateCheckedGenericMetadata(
  _ descriptor: UnsafeRawPointer,
  _ genericArgs: UnsafeRawPointer,
  _ genericArgsSize: UInt
) -> Any.Type?

func metaPointer(_ x: Any.Type) -> UnsafeRawPointer {
  unsafeBitCast(x, to: UnsafeRawPointer.self)
}

testSuite.test("_swift_instantiateCheckedGenericMetadata non-variadic") {
  let dictMeta = unsafeBitCast(
    [Int: Int].self as Any.Type,
    to: UnsafeRawPointer.self
  )
  let dictDesc = dictMeta.load(
    fromByteOffset: MemoryLayout<Int>.size,
    as: UnsafeRawPointer.self
  )

  let dictGenericArgs: [Any.Type] = [String.self, Double.self]

  dictGenericArgs.withUnsafeBufferPointer {
    let newDict = _instantiateCheckedGenericMetadata(
      dictDesc,
      UnsafeRawPointer($0.baseAddress!),
      UInt($0.count)
    )

    expectTrue(newDict == [String: Double].self)
  }
}

testSuite.test("_swift_instantiateCheckedGenericMetadata variadic") {
  let variMeta = unsafeBitCast(
    Variadic< >.self as Any.Type,
    to: UnsafeRawPointer.self
  )
  let variDesc = variMeta.load(
    fromByteOffset: MemoryLayout<Int>.size,
    as: UnsafeRawPointer.self
  )

  let variPack: [Any.Type] = [Int.self, Int8.self, UInt8.self]

  variPack.withUnsafeBufferPointer { pack in
    let packPointer = allocateMetadataPack(
      UnsafeRawPointer(pack.baseAddress!),
      UInt(pack.count)
    )
    let genericArgs = [packPointer]

    genericArgs.withUnsafeBufferPointer { genericArgs in
      let newVari = _instantiateCheckedGenericMetadata(
        variDesc,
        UnsafeRawPointer(genericArgs.baseAddress!),
        UInt(genericArgs.count)
      )

      expectTrue(newVari == Variadic<Int, Int8, UInt8>.self)
    }
  }
}

testSuite.test("_swift_instantiateCheckedGenericMetadata variadic nested with requirements") {
  let nestedMeta = unsafeBitCast(
    Variadic< >.Nested<()>.self as Any.Type,
    to: UnsafeRawPointer.self
  )
  let nestedDesc = nestedMeta.load(
    fromByteOffset: MemoryLayout<Int>.size,
    as: UnsafeRawPointer.self
  )

  let variPack: [Any.Type] = [String.self, [Int].self, UInt64.self]

  let nestedPack: [Any.Type] = [Int.self, Substring.self, Bool.self]

  nestedPack.withUnsafeBufferPointer { nestedPack in
    variPack.withUnsafeBufferPointer { variPack in
      let nestedGenericArgs = [
        allocateMetadataPack(
          UnsafeRawPointer(variPack.baseAddress!),
          UInt(variPack.count)
        ),
        metaPointer(Int16.self),
        allocateMetadataPack(
          UnsafeRawPointer(nestedPack.baseAddress!),
          UInt(nestedPack.count)
        )
      ]

      nestedGenericArgs.withUnsafeBufferPointer { nestedGenericArgs in

        let newNested = _instantiateCheckedGenericMetadata(
          nestedDesc,
          UnsafeRawPointer(nestedGenericArgs.baseAddress!),
          UInt(nestedGenericArgs.count)
        )

        expectTrue(newNested == Variadic<String, [Int], UInt64>.Nested<Int16, Int, Substring, Bool>.self)
      }
    }
  }
}

struct Generic<T> {}
struct Generic2<T, U> {}
struct Generic3<T, U, V> {}
struct Generic4<T, U, V, W> {}
struct Generic5<T, U, V, each W> {}

extension Generic where T == Int {
  struct Nested {}
}

extension Generic2 where T == U? {
  struct Nested {}
}

extension Generic3 where T == U, U == V.Element, V: Collection {
  struct Nested {}
}

extension Generic4 where T == U, U == V.Element, V == W, W: Collection {
  struct Nested {}
}

extension Generic5 where T== U, U == V.Element, V: Collection, repeat each W: Equatable {
  struct Nested {}
}

testSuite.test("_swift_instantiateCheckedGenericMetadata concrete generic types (same type conretized)") {
  let nestedMeta1 = metaPointer(Generic<Int>.Nested.self)
  let nestedDesc1 = nestedMeta1.load(
    fromByteOffset: MemoryLayout<Int>.size,
    as: UnsafeRawPointer.self
  )

  let genericArgs1: [UnsafeRawPointer?] = []

  genericArgs1.withUnsafeBufferPointer {
    let nested = _instantiateCheckedGenericMetadata(
      nestedDesc1,
      UnsafeRawPointer($0.baseAddress!),
      UInt($0.count)
    )

    expectTrue(nested == Generic<Int>.Nested.self)
  }

  let nestedMeta2 = metaPointer(Generic2<Int?, Int>.Nested.self)
  let nestedDesc2 = nestedMeta2.load(
    fromByteOffset: MemoryLayout<Int>.size,
    as: UnsafeRawPointer.self
  )

  let genericArgs2 = [metaPointer(String.self)]

  genericArgs2.withUnsafeBufferPointer {
    let nested = _instantiateCheckedGenericMetadata(
      nestedDesc2,
      UnsafeRawPointer($0.baseAddress!),
      UInt($0.count)
    )

    expectTrue(nested == Generic2<String?, String>.Nested.self)
  }

  let nestedMeta3 = metaPointer(Generic3<Int, Int, [Int]>.Nested.self)
  let nestedDesc3 = nestedMeta3.load(
    fromByteOffset: MemoryLayout<Int>.size,
    as: UnsafeRawPointer.self
  )

  // Fails the constraint `T == V.Element`
  let genericArgs3a = [metaPointer(String.self), metaPointer([Int].self)]

  genericArgs3a.withUnsafeBufferPointer {
    let nested = _instantiateCheckedGenericMetadata(
      nestedDesc3,
      UnsafeRawPointer($0.baseAddress!),
      UInt($0.count)
    )

    expectNil(nested)
  }

  // T == String (U)
  // U == V.Element
  // V: Collection
  //
  // T == String == V.Element
  // V == [String]
  let genericArgs3b = [metaPointer(String.self), metaPointer([String].self)]

  genericArgs3b.withUnsafeBufferPointer {
    let nested = _instantiateCheckedGenericMetadata(
      nestedDesc3,
      UnsafeRawPointer($0.baseAddress!),
      UInt($0.count)
    )

    expectTrue(nested == Generic3<String, String, [String]>.Nested.self)
  }

  let nestedMeta4 = metaPointer(Generic4<(), (), [()], [()]>.Nested.self)
  let nestedDesc4 = nestedMeta4.load(
    fromByteOffset: MemoryLayout<Int>.size,
    as: UnsafeRawPointer.self
  )

  let genericArgs4 = [metaPointer(Int.self), metaPointer([Int].self)]

  genericArgs4.withUnsafeBufferPointer {
    let nested = _instantiateCheckedGenericMetadata(
      nestedDesc4,
      UnsafeRawPointer($0.baseAddress!),
      UInt($0.count)
    )

    expectTrue(nested == Generic4<Int, Int, [Int], [Int]>.Nested.self)
  }

  let nestedMeta5 = metaPointer(Generic5<(), (), [()]>.Nested.self)
  let nestedDesc5 = nestedMeta5.load(
    fromByteOffset: MemoryLayout<Int>.size,
    as: UnsafeRawPointer.self
  )

  let nested5Pack: [Any.Type] = [String.self, Double.self]

  nested5Pack.withUnsafeBufferPointer {
    let genericArgs5 = [
      metaPointer(Int.self),
      metaPointer([Int].self),
      allocateMetadataPack(UnsafeRawPointer($0.baseAddress!), UInt($0.count))
    ]

    genericArgs5.withUnsafeBufferPointer {
      let nested = _instantiateCheckedGenericMetadata(
        nestedDesc5,
        UnsafeRawPointer($0.baseAddress!),
        UInt($0.count)
      )

      expectTrue(nested == Generic5<Int, Int, [Int], String, Double>.Nested.self)
    }
  }
}

extension Value where N == M {
  struct NestedNEqualsM {}
}

testSuite.test("_swift_instantiateCheckedGenericMetadata value generics") {
  let nestedMeta1 = metaPointer(Value<0, 0>.NestedNEqualsM.self)
  let nestedDesc1 = nestedMeta1.load(
    fromByteOffset: MemoryLayout<Int>.size,
    as: UnsafeRawPointer.self
  )

  let genericArgs1: [Int] = [123]

  genericArgs1.withUnsafeBufferPointer {
    let nested = _instantiateCheckedGenericMetadata(
      nestedDesc1,
      UnsafeRawPointer($0.baseAddress!),
      UInt($0.count)
    )

    expectTrue(nested == Value<123, 123>.NestedNEqualsM.self)
  }
}

runAllTests()
