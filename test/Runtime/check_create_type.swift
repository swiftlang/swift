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

testSuite.test("_swift_checkedCreateType non-variadic") {
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

testSuite.test("_swift_checkedCreateType variadic") {
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

testSuite.test("_swift_checkedCreateType variadic nested with requirements") {
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

runAllTests()
