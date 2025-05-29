func foo() {
  var aa = 3
  aa = aa + 1
  _ = "before \(aa) after"
  struct S {
    lazy var lazyVal: Int = {
      let myVal = 0
      return myVal
     }()
  }
  return 1
}

// REQUIRES: swift_swift_parser
// RUN: %empty-directory(%t.result)
// RUN: %sourcekitd-test -req=find-local-rename-ranges -pos=2:8 %s -- %s | %FileCheck %s --check-prefix CHECK-aa

// CHECK-aa: source.edit.kind.active:
// CHECK-aa-NEXT:   2:7-2:9 source.refactoring.range.kind.basename
// CHECK-aa-NEXT: source.edit.kind.active:
// CHECK-aa-NEXT:   3:3-3:5 source.refactoring.range.kind.basename
// CHECK-aa-NEXT: source.edit.kind.active:
// CHECK-aa-NEXT:   3:8-3:10 source.refactoring.range.kind.basename
// CHECK-aa-NEXT: source.edit.kind.active:
// CHECK-aa-NEXT:   4:17-4:19 source.refactoring.range.kind.basename


// RUN: %sourcekitd-test -req=find-local-rename-ranges -pos=7:11 %s -- %s | %FileCheck %s --check-prefix CHECK-myVal

// CHECK-myVal: source.edit.kind.active:
// CHECK-myVal-NEXT:   7:11-7:16 source.refactoring.range.kind.basename
// CHECK-myVal-NEXT: source.edit.kind.active:
// CHECK-myVal-NEXT:   8:14-8:19 source.refactoring.range.kind.basename


// REQUIRES: OS=macosx || OS=linux-gnu
