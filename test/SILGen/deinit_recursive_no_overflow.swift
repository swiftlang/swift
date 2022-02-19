// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

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
