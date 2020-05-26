// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-executable %s -g -o %t/local_types -emit-module

// RUN: sed -ne '/\/\/ *DEMANGLE-TYPE: /s/\/\/ *DEMANGLE-TYPE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test-with-sdk %t/local_types -type-from-mangled=%t/input | %FileCheck %s --check-prefix=CHECK-TYPE

// RUN: sed -ne '/\/\/ *DEMANGLE-DECL: /s/\/\/ *DEMANGLE-DECL: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test-with-sdk %t/local_types -decl-from-mangled=%t/input | %FileCheck %s --check-prefix=CHECK-DECL

func blackHole(_: Any...) {}

func foo() {
  struct Outer  {
    struct Inner { }
    struct GenericInner<T> { }
  }

  do {
    let x1 = Outer()
    let x2 = Outer.Inner()
    let x3 = Outer.GenericInner<Int>()

    blackHole(x1, x2, x3)
  }

  do {
    let x1 = Outer.self
    let x2 = Outer.Inner.self
    let x3 = Outer.GenericInner<Int>.self

    blackHole(x1, x2, x3)
  }
}

// DEMANGLE-TYPE: $s11local_types3fooyyF5OuterL_VD
// CHECK-TYPE: Outer

// DEMANGLE-TYPE: $s11local_types3fooyyF5OuterL_V5InnerVD
// CHECK-TYPE: Outer.Inner

// DEMANGLE-TYPE: $s11local_types3fooyyF5OuterL_V12GenericInnerVy_SiGD
// CHECK-TYPE: Outer.GenericInner<Int>

// DEMANGLE-TYPE: $s11local_types3fooyyF5OuterL_VmD
// CHECK-TYPE: Outer.Type

// DEMANGLE-TYPE: $s11local_types3fooyyF5OuterL_V5InnerVmD
// CHECK-TYPE: Outer.Inner.Type

// DEMANGLE-TYPE: $s11local_types3fooyyF5OuterL_V12GenericInnerVy_SiGmD
// CHECK-TYPE: Outer.GenericInner<Int>.Type


// DEMANGLE-DECL: $s11local_types3fooyyF5OuterL_V
// CHECK-DECL: local_types.(file).foo().Outer

// DEMANGLE-DECL: $s11local_types3fooyyF5OuterL_V5InnerV
// CHECK-DECL: local_types.(file).foo().Outer.Inner

// DEMANGLE-DECL: $s11local_types3fooyyF5OuterL_V12GenericInnerV
// CHECK-DECL: local_types.(file).foo().Outer.GenericInner