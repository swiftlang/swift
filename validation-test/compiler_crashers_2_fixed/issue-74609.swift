// RUN: %target-swift-frontend -target %target-swift-5.9-abi-triple -emit-ir %s

struct Repeater<each Element> {
  let elements: (repeat () -> each Element)
}

struct Box<T> {}

let repeater: Repeater<Box<Int>>
