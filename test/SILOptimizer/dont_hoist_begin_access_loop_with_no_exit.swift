// RUN: %target-swiftc_driver -O %s

// Issue URL
// Don't crash failing to sink the corresponding end_access after
// hoisting the begin_access when the loop has no exit.

var i = 0
while true {
  i += 1
}
