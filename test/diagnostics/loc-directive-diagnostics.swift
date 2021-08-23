// RUN: not %target-swift-frontend -typecheck %s 2>&1 | %FileCheck %s

#sourceLocation(file: "anything.swift", line: 1)
func 1() {}
#sourceLocation()

// CHECK: anything.swift:1:6: error: function name
