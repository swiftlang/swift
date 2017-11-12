func markUsed<T>(_ t: T) {}
func f() {
  if 1==1 {
#sourceLocation(file: "abc.swift", line: 42)
    markUsed("Hello World")
#sourceLocation()
  }
  markUsed("Test")
#sourceLocation(file: "abc.swift", line: 142)
  markUsed("abc again")
#sourceLocation(file: "def.swift", line:  142)
  markUsed("jump directly to def")
}

// RUN: %target-swift-frontend -primary-file %s -S -g -o - | %FileCheck %s
// CHECK: .file	[[MAIN:.*]] "{{.*}}line-directive.swift"
// CHECK: .loc	[[MAIN]] 1
// CHECK: .file	[[ABC:.*]] "abc.swift"
// CHECK: .loc	[[ABC]] 42
// CHECK: .loc	[[MAIN]] 8
// CHECK: .loc	[[ABC]] 142
// CHECK: .file	[[DEF:.*]] "def.swift"
// CHECK: .loc	[[DEF]] 142
// CHECK: .asciz "{{.*}}test/DebugInfo"
