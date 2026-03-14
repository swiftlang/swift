// RUN: %target-swift-emit-silgen -verify %s

struct Block {}

class Story {
  var computed: [Block] {
    get { return [] }
    set {}
  }

  func test() {
    swap(&self.computed[0], &self.computed[1]) // expected-error{{invalid aliasing}} expected-note{{concurrent writeback}}
  }
}
