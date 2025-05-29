// RUN: %empty-directory(%t)
// RUN: %target-clang -x c -c %S/Inputs/unbuffered-putchar.c -o %t/unbuffered-putchar.o

// RUN: %target-build-swift -enable-experimental-feature Embedded -wmo %s -Xlinker %t/unbuffered-putchar.o -o %t/a.out
// RUN: not --crash %t/a.out 2>&1 | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize_none
// REQUIRES: swift_feature_Embedded

var sink: ()->() = {}

func dontEscape(f: () -> ()) {
  withoutActuallyEscaping(f) {
    $0()
  }
}

func dontEscape2(f: () -> ()) {
  withoutActuallyEscaping(f) {
    sink = $0
    sink()
    sink = {}
  }
}

func letEscape(f: () -> ()) {
  withoutActuallyEscaping(f) {
    sink = $0
    sink()
  }
}

dontEscape(f: { print("A") })
dontEscape2(f: { print("B") })
letEscape(f: { print("C") })

// CHECK: A
// CHECK: B
// CHECK: C
// CHECK: non-escaping closure escaped
