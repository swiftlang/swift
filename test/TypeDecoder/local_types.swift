// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-executable %s -g -o %t/local_types -emit-module
// RUN: sed -ne '/\/\/ *DEMANGLE: /s/\/\/ *DEMANGLE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test %t/local_types -type-from-mangled=%t/input | %FileCheck %s

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

// DEMANGLE: $s11local_types3fooyyF5OuterL_VD
// CHECK: Outer

// DEMANGLE: $s11local_types3fooyyF5OuterL_V5InnerVD
// CHECK: Outer.Inner

// DEMANGLE: $s11local_types3fooyyF5OuterL_V12GenericInnerVy_SiGD
// CHECK: Outer.GenericInner<Int>

// DEMANGLE: $s11local_types3fooyyF5OuterL_VmD
// CHECK: Outer.Type

// DEMANGLE: $s11local_types3fooyyF5OuterL_V5InnerVmD
// CHECK: Outer.Inner.Type

// DEMANGLE: $s11local_types3fooyyF5OuterL_V12GenericInnerVy_SiGmD
// CHECK: Outer.GenericInner<Int>.Type
