// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: reflection
// UNSUPPORTED: freestanding
// XFAIL: OS=wasi

import StdlibUnittest
import _Runtime

let suite = TestSuite("TupleMetadata")

if #available(SwiftStdlib 5.9, *) {
  suite.test("Basic") {
    // Tuple

    let tuple = Metadata((name: String, age: Int, [Double], lastThing: UInt64).self).tuple

    let offsets = tuple.elements.map { $0.offset }

    expectEqual(offsets, [0, 16, 24, 32])

    let metadatas = tuple.elements.map { $0.metadata }

    expectEqual(metadatas, [
      Metadata(String.self),
      Metadata(Int.self),
      Metadata([Double].self),
      Metadata(UInt64.self)
    ])

    let labels = tuple.elements.map { $0.label }

    expectEqual(labels, ["name", "age", "", "lastThing"])

    // Tuple2

    let tuple2 = Metadata((Int, String, Double).self).tuple

    let offsets2 = tuple2.elements.map { $0.offset }

    expectEqual(offsets2, [0, 8, 24])

    let metadatas2 = tuple2.elements.map { $0.metadata }

    expectEqual(metadatas2, [
      Metadata(Int.self),
      Metadata(String.self),
      Metadata(Double.self)
    ])

    let labels2 = tuple2.elements.map { $0.label }

    expectEqual(labels2, ["", "", ""])
  }
}

runAllTests()
