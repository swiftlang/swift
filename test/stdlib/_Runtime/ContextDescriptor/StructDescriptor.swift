// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: reflection
// UNSUPPORTED: freestanding

import StdlibUnittest
import _Runtime

let suite = TestSuite("StructDescriptor")

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
    let descriptor = dog.descriptor
    
    expectEqual(descriptor.base.name, "Dog")
  }
  
  suite.test("Generic struct") {
    let dog = Metadata(GenericDog<Int>.self).struct
    let descriptor = dog.descriptor
    
    expectEqual(descriptor.base.name, "GenericDog")
  }
}

runAllTests()
