// RUN: %target-build-swift -emit-sib %s -o %t.sib
// RUN: %target-build-swift %t.sib -o %t
// RUN: %target-run %t | FileCheck %s

// RUN: %target-build-swift -c %t.sib -o %t.o
// RUN: %target-build-swift %t.o -o %t
// RUN: %target-run %t | FileCheck %s

// RUN: %target-build-swift -emit-sibgen %s -o %t.sib
// RUN: %target-build-swift %t.sib -o %t
// RUN: %target-run %t | FileCheck %s

// RUN: %target-build-swift -c %t.sib -o %t.o
// RUN: %target-build-swift %t.o -o %t
// RUN: %target-run %t | FileCheck %s

// CHECK: Hello World
// CHECK: Hello Bob, today is Tuesday.

func greet(name: String, _ day: String) -> String {
  return "Hello \(name), today is \(day)."
}

println("Hello World")
println(greet("Bob", "Tuesday"))
