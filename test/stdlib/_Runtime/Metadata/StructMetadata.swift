// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: reflection_runtime
// UNSUPPORTED: freestanding

import StdlibUnittest
import _Runtime

let suite = TestSuite("StructMetadata")

struct Dog {
  let name: String
  let age: Int
}

struct GenericDog<T> {
  let one: T
  let age: Int
  let two: T
}

if #available(SwiftStdlib 5.9, *) {
  suite.test("Non-generic struct") {
    let dog = Metadata(Dog.self).struct

    expectTrue(dog.fieldOffsets.elementsEqual([0, 16]))
  }

  suite.test("Generic struct") {
    let genericDog = Metadata(GenericDog<String>.self).struct

    expectTrue(genericDog.fieldOffsets.elementsEqual([0, 16, 24]))
  }
}

runAllTests()
