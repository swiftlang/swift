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

@_silgen_name("_swift_checkedCreateType")
func _checkedCreateType(
  _ descriptor: UnsafeRawPointer,
  _ genericArgs: UnsafeRawPointer,
  _ genericArgsSize: UInt,
  _ packCounts: UnsafeRawPointer?,
  _ packCountsSize: UInt
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
    let newDict = _checkedCreateType(
      dictDesc,
      UnsafeRawPointer($0.baseAddress!),
      UInt($0.count),
      nil,
      0
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
  let variPackCounts: [Int32] = [3]

  variPack.withUnsafeBufferPointer { pack in
    let genericArgs = [UnsafeRawPointer(pack.baseAddress!)]

    genericArgs.withUnsafeBufferPointer { genericArgs in
      variPackCounts.withUnsafeBufferPointer { packCounts in
        let newVari = _checkedCreateType(
          variDesc,
          UnsafeRawPointer(genericArgs.baseAddress!),
          UInt(genericArgs.count),
          UnsafeRawPointer(packCounts.baseAddress!),
          UInt(packCounts.count)
        )

        expectTrue(newVari == Variadic<Int, Int8, UInt8>.self)
      }
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
        UnsafeRawPointer(variPack.baseAddress!),
        metaPointer(Int16.self),
        UnsafeRawPointer(nestedPack.baseAddress!)
      ]

      nestedGenericArgs.withUnsafeBufferPointer { nestedGenericArgs in
        // 3 for each T, -1 for U, and 3 for each V
        let nestedPackCounts: [Int32] = [3, -1, 3]

        nestedPackCounts.withUnsafeBufferPointer { nestedPackCounts in
          let newNested = _checkedCreateType(
            nestedDesc,
            UnsafeRawPointer(nestedGenericArgs.baseAddress!),
            UInt(nestedGenericArgs.count),
            UnsafeRawPointer(nestedPackCounts.baseAddress!),
            UInt(nestedPackCounts.count)
          )

          expectTrue(newNested == Variadic<String, [Int], UInt64>.Nested<Int16, Int, Substring, Bool>.self)
        }
      }
    }
  }
}

runAllTests()
