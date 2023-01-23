// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-feature -Xfrontend RuntimeDiscoverableAttrs)
// REQUIRES: executable_test
// REQUIRES: reflection

import Reflection

import StdlibUnittest

let suite = TestSuite("Attributes")

@runtimeMetadata
struct Greeting {
  let description: String

  init<T, U>(attachedTo: KeyPath<T, U>, _ description: String) {
    self.description = description
  }
}

extension Greeting: Equatable {}

struct Struct {
  @Greeting("Hello!")
  let property: Int
}

extension Struct: Equatable {}

if #available(SwiftStdlib 5.9, *) {
  suite.test("basic") {
    let empty = Attribute.allInstances(of: Struct.self)
    expectEqualSequence(empty, [])

    let instances = Attribute.allInstances(of: Greeting.self)

    expectEqualSequence(instances, [
      Greeting(attachedTo: \Struct.property, "Hello!")
    ])

    let instances2 = Attribute.allInstances(of: Greeting.self)
    expectEqualSequence(instances2, [
      Greeting(attachedTo: \Struct.property, "Hello!")
    ])
  }
}

runAllTests()
