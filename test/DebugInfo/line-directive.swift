func f() {
  if 1==1 {
#line 42 "abc.swift"
    println("Hello World")
#line
  }
  println("Test")
}

// RUN: %swift -target x86_64-apple-darwin %s -S -g -o - | FileCheck %s
// CHECK: .file	[[MAIN:.*]] "{{.*}}line-directive.swift"
// CHECK: .loc	[[MAIN]] 1
// CHECK: .file	[[ABC:.*]] "{{.*}}abc.swift"
// CHECK: .loc	[[ABC]] 42
// CHECK: .loc	[[MAIN]] 7
