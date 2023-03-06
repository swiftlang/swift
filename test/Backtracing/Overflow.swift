// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -Onone -g -o %t/Overflow
// RUN: %target-codesign %t/Overflow
// RUN: (env SWIFT_BACKTRACE=enable=yes,cache=no %target-run %t/Overflow || true) | %FileCheck %s
// RUN: (env SWIFT_BACKTRACE=preset=friendly,enable=yes,cache=no %target-run %t/Overflow || true) | %FileCheck %s --check-prefix FRIENDLY

// REQUIRES: executable_test
// REQUIRES: backtracing
// REQUIRES: OS=macosx
var x: UInt = 0

func level1() {
  level2()
}

func level2() {
  level3()
}

func level3() {
  level4()
}

func level4() {
  level5()
}

func level5() {
  print("About to overflow")

  x -= 1
}

@main
struct Overflow {
  static func main() {
    level1()
  }
}

// CHECK: *** Swift runtime failure: arithmetic overflow ***

// CHECK: Thread 0 crashed:

// CHECK:      0 [inlined] [system] 0x{{[0-9a-f]+}} Swift runtime failure: arithmetic overflow in Overflow at {{.*}}/<compiler-generated>
// CHECK-NEXT: 1                    0x{{[0-9a-f]+}} level5() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift:31:5
// CHECK-NEXT: 2 [ra]               0x{{[0-9a-f]+}} level4() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift:25:3
// CHECK-NEXT: 3 [ra]               0x{{[0-9a-f]+}} level3() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift:21:3
// CHECK-NEXT: 4 [ra]               0x{{[0-9a-f]+}} level2() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift:17:3
// CHECK-NEXT: 5 [ra]               0x{{[0-9a-f]+}} level1() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift:13:3
// CHECK-NEXT: 6 [ra]               0x{{[0-9a-f]+}} static Overflow.main() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift:37:5
// CHECK-NEXT: 7 [ra] [system]      0x{{[0-9a-f]+}} static Overflow.$main() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift:34:1
// CHECK-NEXT: 8 [ra] [system]      0x{{[0-9a-f]+}} main + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift

// CHECK: Registers:

// CHECK: Images ({{[0-9]+}} omitted):

// CHECK: {{0x[0-9a-f]+}}–{{0x[0-9a-f]+}}{{ +}}{{[0-9a-f]+}}{{ +}}Overflow{{ +}}{{.*}}/Overflow

// FRIENDLY: *** Swift runtime failure: arithmetic overflow ***

// FRIENDLY: Thread 0 crashed:

// FRIENDLY: 0 level5() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift:31:5

// FRIENDLY: 29|   print("About to overflow")
// FRIENDLY-NEXT: 30|
// FRIENDLY-NEXT: 31|   x -= 1
// FRIENDLY-NEXT:   |     ^
// FRIENDLY-NEXT: 32| }

// FRIENDLY: 1 level4() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift:25:3

// FRIENDLY: 23|
// FRIENDLY-NEXT: 24| func level4() {
// FRIENDLY-NEXT: 25|   level5()
// FRIENDLY-NEXT:   |   ^
// FRIENDLY-NEXT: 26| }
// FRIENDLY-NEXT: 27|

// FRIENDLY: 2 level3() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift:21:3

// FRIENDLY: 19|
// FRIENDLY-NEXT: 20| func level3() {
// FRIENDLY-NEXT: 21|   level4()
// FRIENDLY-NEXT:   |   ^
// FRIENDLY-NEXT: 22| }
// FRIENDLY-NEXT: 23|

// FRIENDLY: 3 level2() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift:17:3

// FRIENDLY: 15|
// FRIENDLY-NEXT: 16| func level2() {
// FRIENDLY-NEXT: 17|   level3()
// FRIENDLY-NEXT:   |   ^
// FRIENDLY-NEXT: 18| }
// FRIENDLY-NEXT: 19|

// FRIENDLY: 4 level1() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift:13:3

// FRIENDLY: 11|
// FRIENDLY-NEXT: 12| func level1() {
// FRIENDLY-NEXT: 13|   level2()
// FRIENDLY-NEXT:   |   ^
// FRIENDLY-NEXT: 14| }
// FRIENDLY-NEXT: 15|

// FRIENDLY: 5 static Overflow.main() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift

