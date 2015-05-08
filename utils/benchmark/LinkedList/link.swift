@asmname("mach_absolute_time") func __mach_absolute_time__() -> UInt64

@final class Node {
  var next: Node?
  var data: Int

  init(n: Node?, d: Int) {
    next = n
    data = d
  }
}

print("Creating list\n")
var head = Node(nil, 0)
for i in 0..100 {
  head = Node(head, i)
}

let start = __mach_absolute_time__()
print("Summing list\n")
var sum = 0
var ptr = head
for i in 0..100000 {
  ptr = head
  while let nxt = ptr.next {
    sum += ptr.data
    ptr = nxt
  }
}

let delta = __mach_absolute_time__() - start
print("sum = \(sum)\n")
print("\(delta) nanoseconds.")
