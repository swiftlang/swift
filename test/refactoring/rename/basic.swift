struct S1 {}
func foo1(a: S1) {}
class C1 {}
func foo2(c : C1) {}
enum E1 {}
func foo3(e : E1) {}
func foo4(a : S1, b : C1, c: E1) { foo4(a: a, b: b, c :c) }

func test() {
  struct SLocal {
    init(x: S1) {}
  }
  func local(a: SLocal) {}
  local(a: SLocal(x: S1()))
}

guard let top = Optional.some("top") else {
  fatalError()
}
print(top)

protocol P1 {}
struct Test {
  var test: P1 {
    struct SP1: P1 {}
    return SP1()
  }
}

// REQUIRES: swift_swift_parser
// RUN: %empty-directory(%t)
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=2:15 > %t/S1.swift
// RUN: diff -u %S/Outputs/basic_ranges/S1.swift.expected %t/S1.swift
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=4:16 > %t/C1.swift
// RUN: diff -u %S/Outputs/basic_ranges/C1.swift.expected %t/C1.swift
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=6:16 > %t/E1.swift
// RUN: diff -u %S/Outputs/basic_ranges/E1.swift.expected %t/E1.swift
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=7:38 > %t/foo4.swift
// RUN: diff -u %S/Outputs/basic_ranges/foo4.swift.expected %t/foo4.swift
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=1:9 > %t/S1.swift
// RUN: diff -u %S/Outputs/basic_ranges/S1.swift.expected %t/S1.swift
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=3:8 > %t/C1.swift
// RUN: diff -u %S/Outputs/basic_ranges/C1.swift.expected %t/C1.swift
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=5:7 > %t/E1.swift
// RUN: diff -u %S/Outputs/basic_ranges/E1.swift.expected %t/E1.swift
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=7:7 > %t/foo4.swift
// RUN: diff -u %S/Outputs/basic_ranges/foo4.swift.expected %t/foo4.swift
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=10:10 > %t/SLocal.swift
// RUN: diff -u %S/Outputs/basic_ranges/SLocal.swift.expected %t/SLocal.swift
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=11:5 > %t/SLocal_init.swift
// RUN: diff -u %S/Outputs/basic_ranges/SLocal_init.swift.expected %t/SLocal_init.swift
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=13:8 > %t/local.swift
// RUN: diff -u %S/Outputs/basic_ranges/local.swift.expected %t/local.swift
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=20:7 > %t/top_level.swift
// RUN: diff -u %S/Outputs/basic_ranges/top_level.swift.expected %t/top_level.swift
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=26:12 > %t/SP1.swift
// RUN: diff -u %S/Outputs/basic_ranges/SP1.swift.expected %t/SP1.swift
