// RUN: %target-swift-frontend -emit-sil -verify %s

struct Block {}

class Story {
  final var finalStored = [Block]()
  var overridableStored = [Block]()
  var computed: [Block] {
    get { return [] }
    set {}
  }

  func test() {
    swap(&self.finalStored[0], &self.finalStored[1])
    swap(&self.overridableStored[0], &self.overridableStored[1])
    swap(&self.computed[0], &self.computed[1]) // expected-error{{invalid aliasing}} expected-note{{concurrent writeback}}
  }
}

protocol StoriedType {
  var protocolRequirement: [Block] { get set }
}

func testProtocol<T: StoriedType>(inout x: T) {
  swap(&x.protocolRequirement[0], &x.protocolRequirement[1])
}
