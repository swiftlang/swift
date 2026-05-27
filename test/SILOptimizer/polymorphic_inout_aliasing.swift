// RUN: %target-swift-emit-sil -verify %s

struct Block {}

class Story {
  final var finalStored = [Block]()
  var overridableStored = [Block]()

  func test() {
    // expected-error@+2 {{overlapping accesses to 'finalStored', but modification requires exclusive access; consider calling MutableCollection.swapAt(_:_:)}}
    // expected-note@+1 {{conflicting access is here}}
    swap(&self.finalStored[0], &self.finalStored[1])
    swap(&self.overridableStored[0], &self.overridableStored[1])
  }
}
