// RUN: %target-run-simple-swift

// REQUIRES: executable_test

class Node {
  var next: Node?
}

var first: Node? = nil
for _ in 1...3_000_000 {
    let next = Node()
    next.next = first
    first = next
}

@inline(never)
func deallocList() {
    first = nil
}
deallocList()