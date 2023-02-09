// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import _Runtime

let suite = TestSuite("Metadata")

struct Dog {}

enum Cat {}

class Fish {}

if #available(SwiftStdlib 5.9, *) {
  suite.test("Basic") {
    let structMetadata = Metadata(Dog.self)
    let enumMetadata = Metadata(Cat.self)
    let classMetadata = Metadata(Fish.self)
    let existentialMetadata = Metadata((any Equatable).self)
    let functionMetadata = Metadata((() -> ()).self)
    let metatypeMetadata = Metadata(Int.Type.self)
    let tupleMetadata = Metadata(Void.self)

//===----------------------------------------------------------------------===//
// Kinds
//===----------------------------------------------------------------------===//

    expectEqual(structMetadata.kind, .struct)
    expectEqual(enumMetadata.kind, .enum)
    expectEqual(classMetadata.kind, .class)
    expectEqual(existentialMetadata.kind, .existential)
    expectEqual(functionMetadata.kind, .function)
    expectEqual(metatypeMetadata.kind, .metatype)
    expectEqual(tupleMetadata.kind, .tuple)

//===----------------------------------------------------------------------===//
// Metadata Equality
//===----------------------------------------------------------------------===//

    expectEqual(structMetadata, structMetadata)
    expectNotEqual(structMetadata, Metadata(Int.self))
    expectNotEqual(structMetadata, classMetadata)
  }
}

runAllTests()
