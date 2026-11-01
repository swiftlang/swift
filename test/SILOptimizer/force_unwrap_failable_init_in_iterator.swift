// RUN: %target-swift-frontend -O -parse-as-library -module-name test -emit-sil %s | %FileCheck %s

// An iterator whose `next()` force-unwraps a failable initializer of an element
// holding a reference doesn't retain or release the guaranteed reference for
// each element it passes on as guaranteed.

public final class Owner {}

public struct Element {
  public let owner: Owner
  public let id: Int

  public init?(owner: Owner, id: Int) {
    guard id >= 0 else { return nil }
    self.owner = owner
    self.id = id
  }
}

public struct FailableIterator: IteratorProtocol {
  let owner: Owner
  var index = 0

  public init(owner: Owner) { self.owner = owner }

  public mutating func next() -> Element? {
    guard index < count else { return nil }
    defer { index += 1 }
    return Element(owner: owner, id: index)!
  }
}

var count: Int {
  @inline(never) get { 8 }
}

@_optimize(none) @inline(never)
func use(_ element: Element) {}

// CHECK-LABEL: sil [noinline] @$s4test15iterateFailableyyAA5OwnerCF :
// CHECK-NOT:     strong_retain
// CHECK-NOT:     strong_release
// CHECK-NOT:     retain_value
// CHECK-NOT:     release_value
// CHECK:       } // end sil function '$s4test15iterateFailableyyAA5OwnerCF'
@inline(never)
public func iterateFailable(_ owner: Owner) {
  var iterator = FailableIterator(owner: owner)
  while let element = iterator.next() {
    use(element)
  }
}
