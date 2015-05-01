func markUsed<T>(t: T) {}
func f() {
  if 1==1 {
#line 42 "abc.swift"
    markUsed("Hello World")
#line
  }
  markUsed("Test")
#line 142 "abc.swift"
  markUsed("abc again")
#line 142 "def.swift"
  markUsed("jump directly to def")
}

// RUN: %target-swift-frontend -primary-file %s -S -g -o - | FileCheck %s
// CHECK: .file	[[MAIN:.*]] "{{.*}}line-directive.swift"
// CHECK: .loc	[[MAIN]] 1
// CHECK: .file	[[ABC:.*]] "abc.swift"
// CHECK: .loc	[[ABC]] 42
// CHECK: .loc	[[MAIN]] 8
// CHECK: .loc	[[ABC]] 142
// CHECK: .file	[[DEF:.*]] "def.swift"
// CHECK: .loc	[[DEF]] 142
// CHECK: .asciz "{{.*}}test/DebugInfo"
