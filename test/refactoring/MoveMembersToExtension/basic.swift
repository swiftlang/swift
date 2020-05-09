class Foo {
  /// This is just some documentation
  func methodWithoutParameters() {
  }

  func methodWithParameters(param1: Foo, param2: Foo) {
  }

  @discardableResult
  private func privateMethodWithAnnotation() -> Foo? {
    return nil
  }

  var anInstanceVariable: String?

  func justAnotherMethod() {
  }
}

class OtherClass {
  func foo() {
  }

  var computedVariable: Int {
    return 0
  }

  var computedVariable2: Int {
    get { return 0 }
    set { }
  }

  deinit { print("deinit") }
}

enum MyEnum {
  case foo, bar
  case baz
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -move-to-extension -source-filename %s -pos=2:1 -end-pos=5:1 > %t.result/L2-5.swift
// RUN: diff -u %S/Outputs/L2-5.swift.expected %t.result/L2-5.swift
// RUN: %empty-directory(%t.result)
// RUN: %refactor -move-to-extension -source-filename %s -pos=3:1 -end-pos=5:1 > %t.result/L3-5.swift
// RUN: diff -u %S/Outputs/L3-5.swift.expected %t.result/L3-5.swift
// RUN: %empty-directory(%t.result)
// RUN: %refactor -move-to-extension -source-filename %s -pos=6:1 -end-pos=8:1 > %t.result/L6-8.swift
// RUN: diff -u %S/Outputs/L6-8.swift.expected %t.result/L6-8.swift
// RUN: %empty-directory(%t.result)
// RUN: %refactor -move-to-extension -source-filename %s -pos=3:1 -end-pos=13:1 > %t.result/L3-13.swift
// RUN: diff -u %S/Outputs/L3-13.swift.expected %t.result/L3-13.swift
// RUN: %empty-directory(%t.result)
// RUN: %refactor -move-to-extension -source-filename %s -pos=24:1 -end-pos=27:1 > %t.result/L24-27.swift
// RUN: diff -u %S/Outputs/L24-27.swift.expected %t.result/L24-27.swift
// RUN: not %refactor -move-to-extension -source-filename %s -pos=3:1 -end-pos=4:1
// RUN: not %refactor -move-to-extension -source-filename %s -pos=4:1 -end-pos=8:1
// RUN: not %refactor -move-to-extension -source-filename %s -pos=1:1 -end-pos=14:1
// RUN: not %refactor -move-to-extension -source-filename %s -pos=14:1 -end-pos=15:1
// RUN: not %refactor -move-to-extension -source-filename %s -pos=15:1 -end-pos=23:1
// RUN: not %refactor -move-to-extension -source-filename %s -pos=24:29 -end-pos=27:1
// RUN: not %refactor -move-to-extension -source-filename %s -pos=29:1 -end-pos=30:1
// RUN: not %refactor -move-to-extension -source-filename %s -pos=30:1 -end-pos=31:1
// RUN: not %refactor -move-to-extension -source-filename %s -pos=33:1 -end-pos=34:1
// RUN: not %refactor -move-to-extension -source-filename %s -pos=37:1 -end-pos=37:1
// RUN: not %refactor -move-to-extension -source-filename %s -pos=37:8 -end-pos=37:16
