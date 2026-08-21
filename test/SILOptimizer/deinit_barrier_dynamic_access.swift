// RUN: %target-run-simple-swift(-O) | %FileCheck %s

// REQUIRES: executable_test

// rdar://183126067
// The lifetime of `c1` must not be shortened to end before the `sum()` loop, because
// `Container.deinit` writes the `next` fields which that loop is reading.
// The loop reads those fields through dynamically enforced access scopes, so the
// `end_access` instructions must be deinit barriers.

final class Node {
  var value: Int
  var next: Node?

  init(_ v: Int) { value = v }
}

final class Container {
  var head: Node?
  var tail: Node?

  func append(_ value: Int) {
    let node = Node(value)
    if let t = tail {
      t.next = node
      tail = node
    } else {
      head = node
      tail = node
    }
  }

  func sum() -> Int {
    var result = 0
    var current = head
    while let c = current {
      result += c.value
      current = c.next
    }
    return result
  }

  deinit {
    // Unlink the nodes to avoid a deep recursive deallocation.
    var current = head
    while let c = current {
      let n = c.next
      c.next = nil
      current = n
    }
  }
}

@inline(never)
func testSumIsNotAffectedByDeinit() -> Int {
  let c1 = Container()
  for i in 1..<10 {
    c1.append(i)
  }
  return c1.sum()
}

// CHECK: result=45
print("result=\(testSumIsNotAffectedByDeinit())")
