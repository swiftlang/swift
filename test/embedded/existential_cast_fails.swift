// RUN: %empty-directory(%t)

// RUN: %target-clang -x c -c %S/Inputs/unbuffered-putchar.c -o %t/unbuffered-putchar.o

// RUN: %target-build-swift -DT1 -enable-experimental-feature Embedded \
// RUN:  -parse-as-library -Xlinker %t/unbuffered-putchar.o \
// RUN:  -enable-experimental-feature EmbeddedExistentials \
// RUN:  -wmo -runtime-compatibility-version none %s  -o %t/t1.out
// RUN: %target-not-crash %target-run %t/t1.out 2>&1 | %FileCheck %s --check-prefix=CHECK-T1

// RUN: %target-build-swift -DT2 -enable-experimental-feature Embedded \
// RUN:  -parse-as-library -Xlinker %t/unbuffered-putchar.o \
// RUN:  -enable-experimental-feature EmbeddedExistentials \
// RUN:  -wmo -runtime-compatibility-version none %s  -o %t/t2.out
// RUN: %target-not-crash %target-run %t/t2.out 2>&1 | %FileCheck %s --check-prefix=CHECK-T2

// RUN: %target-build-swift -DT3 -enable-experimental-feature Embedded \
// RUN:  -parse-as-library -Xlinker %t/unbuffered-putchar.o \
// RUN:  -enable-experimental-feature EmbeddedExistentials \
// RUN:  -wmo -runtime-compatibility-version none %s  -o %t/t3.out
// RUN: %target-not-crash %target-run %t/t3.out 2>&1 | %FileCheck %s --check-prefix=CHECK-T3

// RUN: %target-build-swift -DT4 -enable-experimental-feature Embedded \
// RUN:  -parse-as-library -Xlinker %t/unbuffered-putchar.o \
// RUN:  -enable-experimental-feature EmbeddedExistentials \
// RUN:  -wmo -runtime-compatibility-version none %s  -o %t/t4.out
// RUN: %target-not-crash %target-run %t/t4.out 2>&1 | %FileCheck %s --check-prefix=CHECK-T4

// REQUIRES: swift_test_mode_optimize_none
// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedExistentials

// REQUIRES: OS=macosx || OS=wasip1

class CP {
  func foo() { print("foo called") }
  deinit {
      print("deinit called")
  }
}

class C : CP {
  override func foo() { print("C.foo called") }
}

class CP2 {
  func foo() { print("CP2.foo called") }
  deinit {
      print("deinit called")
  }
}

struct StructWithClass {
  var c = C()
}


struct LargeStructWithClass {
  var c = C()
  var t = (0, 1, 2, 3, 4, 5, 6, 7, 8)
  func foo() { c.foo() }
}

struct LargetMyStruct {
  var l = LargeStructWithClass()
}

func test1(_ p: any Any) {
  print("test any as! CP")
  let c = p as! CP
  c.foo()
}

func test2(_ p: any Any) {
  print("test any as! LargeStructWithClass")
  let c = p as! LargeStructWithClass
  c.foo()
}

@main
struct Main {
  static func main() {
#if T1
   test1(StructWithClass())
// CHECK-T1: test any as! CP
// CHECK-T1: failed cast
#endif

#if T2
   test1(CP2())
// CHECK-T2: test any as! CP
// CHECK-T2: failed cast
#endif

#if T3
   test2(StructWithClass())
// CHECK-T3: test any as! LargeStructWithClass
// CHECK-T3: failed cast
#endif

#if T4
   test2(CP2())
// CHECK-T4: test any as! LargeStructWithClass
// CHECK-T4: failed cast
#endif
  }
}
