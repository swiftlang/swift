func f() {
  if 1==1 {
#line 42 "abc.swift"
    println("Hello World")
#line
  }
  println("Test")
#line 142 "abc.swift"
  println("abc again")
#line 142 "def.swift"
  println("jump directly to def")
}

// RUN: %target-swift-frontend -primary-file %s -S -g -o - | FileCheck %s
// CHECK: .file	[[MAIN:.*]] "{{.*}}line-directive.swift"
// CHECK: .loc	[[MAIN]] 1
// CHECK: .file	[[ABC:.*]] "{{.*}}abc.swift"
// CHECK: .loc	[[ABC]] 42
// CHECK: .loc	[[MAIN]] 7
// CHECK: .loc	[[ABC]] 142
// CHECK: .file	[[DEF:.*]] "{{.*}}def.swift"
// CHECK: .loc	[[DEF]] 142
