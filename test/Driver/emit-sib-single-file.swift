// RUN: %target-swiftc_driver -emit-sib %s -o %t.sib
// RUN: %target-swiftc_driver %t.sib -o %t
// RUN: %target-run %t | FileCheck %s

// RUN: %target-swiftc_driver -c %t.sib -o %t.o
// RUN: %target-swiftc_driver %t.o -o %t
// RUN: %target-run %t | FileCheck %s

// RUN: %target-swiftc_driver -emit-sibgen %s -o %t.sib
// RUN: %target-swiftc_driver %t.sib -o %t
// RUN: %target-run %t | FileCheck %s

// RUN: %target-swiftc_driver -c %t.sib -o %t.o
// RUN: %target-swiftc_driver %t.o -o %t
// RUN: %target-run %t | FileCheck %s

// CHECK: Hello World
// CHECK: Hello Bob, today is Tuesday.

func greet(name: String, day: String) -> String {
  return "Hello \(name), today is \(day)."
}

println("Hello World")
println(greet("Bob", "Tuesday"))
