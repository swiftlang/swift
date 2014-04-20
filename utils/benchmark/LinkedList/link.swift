@asmname("mach_absolute_time") func __mach_absolute_time__() -> UInt64

@final class Node {
  var next : Optional<Node>
  var data : Int

  init(n : Optional<Node>, d : Int) {
    next = n
    data = d
  }
}

print("Creating list\n")
var head = Node(nil, 0)
for i in 0...1000000 {
  head = Node(head, i)
}

let start = __mach_absolute_time__()
print("Summing list\n")
var sum = 0
while let nxt = head.next {
  sum += head.data
  head = nxt
}

let delta = __mach_absolute_time__() - start
print("sum = \(sum)\n")
println("\(delta) nanoseconds.")

