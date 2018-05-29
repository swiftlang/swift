struct Foo {
  static func foo(a: () -> Int) {}
  func qux(x: Int, y: () -> Int ) {}
}

func testTrailingClosure() -> String {
  Foo.foo(a: { 1 })
  Foo.bar(a: { print(3); return 1 })
  Foo().qux(x: 1, y: { 1 })
  let _ = Foo().quux(x: 1, y: { 1 })

  [1,2,3]
    .filter({ $0 % 2 == 0 })
    .map({ $0 + 1 })
}
// RUN: %empty-directory(%t.result)

// RUN: %refactor -trailingclosure -source-filename %s -pos=7:3 > %t.result/L7.swift
// RUN: diff -u %S/Outputs/basic/L7.swift.expected %t.result/L7.swift

// RUN: %refactor -trailingclosure -source-filename %s -pos=8:11 > %t.result/L8.swift
// RUN: diff -u %S/Outputs/basic/L8.swift.expected %t.result/L8.swift

// RUN: %refactor -trailingclosure -source-filename %s -pos=9:8 > %t.result/L9.swift
// RUN: diff -u %S/Outputs/basic/L9.swift.expected %t.result/L9.swift

// RUN: %refactor -trailingclosure -source-filename %s -pos=10:17 > %t.result/L10.swift
// RUN: diff -u %S/Outputs/basic/L10.swift.expected %t.result/L10.swift

// RUN: %refactor -trailingclosure -source-filename %s -pos=13:5 > %t.result/L13.swift
// RUN: diff -u %S/Outputs/basic/L13.swift.expected %t.result/L13.swift
// RUN: %refactor -trailingclosure -source-filename %s -pos=14:5 > %t.result/L14.swift
// RUN: diff -u %S/Outputs/basic/L14.swift.expected %t.result/L14.swift

