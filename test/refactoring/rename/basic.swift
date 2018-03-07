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

// RUN: %empty-directory(%t.result)
// RUN: %refactor -rename -source-filename %s -pos=2:15 -new-name new_S1 >> %t.result/S1.swift
// RUN: diff -u %S/Outputs/basic/S1.swift.expected %t.result/S1.swift
// RUN: %refactor -rename -source-filename %s -pos=4:16 -new-name new_c1>> %t.result/C1.swift
// RUN: diff -u %S/Outputs/basic/C1.swift.expected %t.result/C1.swift
// RUN: %refactor -rename -source-filename %s -pos=6:16 -new-name new_e1 >> %t.result/E1.swift
// RUN: diff -u %S/Outputs/basic/E1.swift.expected %t.result/E1.swift
// RUN: %refactor -rename -source-filename %s -pos=7:38 -new-name 'new_foo4(a:b:c:)' >> %t.result/foo4.swift
// RUN: diff -u %S/Outputs/basic/foo4.swift.expected %t.result/foo4.swift
// RUN: %refactor -rename -source-filename %s -pos=1:9 -new-name new_S1 > %t.result/S1.swift
// RUN: diff -u %S/Outputs/basic/S1.swift.expected %t.result/S1.swift
// RUN: %refactor -rename -source-filename %s -pos=3:8 -new-name new_c1 > %t.result/C1.swift
// RUN: diff -u %S/Outputs/basic/C1.swift.expected %t.result/C1.swift
// RUN: %refactor -rename -source-filename %s -pos=5:7 -new-name new_e1 > %t.result/E1.swift
// RUN: diff -u %S/Outputs/basic/E1.swift.expected %t.result/E1.swift
// RUN: %refactor -rename -source-filename %s -pos=7:7 -new-name 'new_foo4(a:b:c:)' > %t.result/foo4.swift
// RUN: diff -u %S/Outputs/basic/foo4.swift.expected %t.result/foo4.swift
// RUN: %refactor -rename -source-filename %s -pos=7:7 -new-name 'new_foo4(new_a:b:_:)' > %t.result/foo4_multi.swift
// RUN: diff -u %S/Outputs/basic/foo4_multi.swift.expected %t.result/foo4_multi.swift
// RUN: %refactor -rename -source-filename %s -pos=10:10 -new-name new_SLocal >> %t.result/SLocal.swift
// RUN: diff -u %S/Outputs/basic/SLocal.swift.expected %t.result/SLocal.swift
// RUN: %refactor -rename -source-filename %s -pos=11:5 -new-name 'init(y:)' >> %t.result/SLocal_init.swift
// RUN: diff -u %S/Outputs/basic/SLocal_init.swift.expected %t.result/SLocal_init.swift
// RUN: %refactor -rename -source-filename %s -pos=13:8 -new-name 'new_local(b:)' >> %t.result/local.swift
// RUN: diff -u %S/Outputs/basic/local.swift.expected %t.result/local.swift
// RUN: %refactor -rename -source-filename %s -pos=20:7 -new-name 'bottom' > %t.result/top_level.swift
// RUN: diff -u %S/Outputs/basic/top_level.swift.expected %t.result/top_level.swift
// RUN: %empty-directory(%t.ranges)
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=2:15 > %t.ranges/S1.swift
// RUN: diff -u %S/Outputs/basic_ranges/S1.swift.expected %t.ranges/S1.swift
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=4:16 > %t.ranges/C1.swift
// RUN: diff -u %S/Outputs/basic_ranges/C1.swift.expected %t.ranges/C1.swift
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=6:16 > %t.ranges/E1.swift
// RUN: diff -u %S/Outputs/basic_ranges/E1.swift.expected %t.ranges/E1.swift
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=7:38 > %t.ranges/foo4.swift
// RUN: diff -u %S/Outputs/basic_ranges/foo4.swift.expected %t.ranges/foo4.swift
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=1:9 > %t.ranges/S1.swift
// RUN: diff -u %S/Outputs/basic_ranges/S1.swift.expected %t.ranges/S1.swift
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=3:8 > %t.ranges/C1.swift
// RUN: diff -u %S/Outputs/basic_ranges/C1.swift.expected %t.ranges/C1.swift
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=5:7 > %t.ranges/E1.swift
// RUN: diff -u %S/Outputs/basic_ranges/E1.swift.expected %t.ranges/E1.swift
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=7:7 > %t.ranges/foo4.swift
// RUN: diff -u %S/Outputs/basic_ranges/foo4.swift.expected %t.ranges/foo4.swift
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=10:10 > %t.ranges/SLocal.swift
// RUN: diff -u %S/Outputs/basic_ranges/SLocal.swift.expected %t.ranges/SLocal.swift
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=11:5 > %t.ranges/SLocal_init.swift
// RUN: diff -u %S/Outputs/basic_ranges/SLocal_init.swift.expected %t.ranges/SLocal_init.swift
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=13:8 > %t.ranges/local.swift
// RUN: diff -u %S/Outputs/basic_ranges/local.swift.expected %t.ranges/local.swift
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=20:7 > %t.result/top_level.swift
// RUN: diff -u %S/Outputs/basic_ranges/top_level.swift.expected %t.result/top_level.swift
