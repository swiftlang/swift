// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -Onone -g -o %t/Overflow
// RUN: %target-codesign %t/Overflow
// RUN: (env SWIFT_BACKTRACE=enable=yes,cache=no %target-run %t/Overflow || true) | %FileCheck %s
// RUN: (env SWIFT_BACKTRACE=preset=friendly,enable=yes,cache=no %target-run %t/Overflow || true) | %FileCheck %s --check-prefix FRIENDLY

// REQUIRES: executable_test
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
// CHECK-NEXT: 1                    0x{{[0-9a-f]+}} level5() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift:30:5
// CHECK-NEXT: 2 [ra]               0x{{[0-9a-f]+}} level4() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift:24:3
// CHECK-NEXT: 3 [ra]               0x{{[0-9a-f]+}} level3() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift:20:3
// CHECK-NEXT: 4 [ra]               0x{{[0-9a-f]+}} level2() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift:16:3
// CHECK-NEXT: 5 [ra]               0x{{[0-9a-f]+}} level1() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift:12:3
// CHECK-NEXT: 6 [ra]               0x{{[0-9a-f]+}} static Overflow.main() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift:36:5
// CHECK-NEXT: 7 [ra] [system]      0x{{[0-9a-f]+}} static Overflow.$main() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift:33:1
// CHECK-NEXT: 8 [ra] [system]      0x{{[0-9a-f]+}} main + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift

// CHECK: Registers:

// CHECK: Images ({{[0-9]+}} omitted):

// CHECK: {{0x[0-9a-f]+}}–{{0x[0-9a-f]+}}{{ +}}{{[0-9a-f]+}}{{ +}}Overflow{{ +}}{{.*}}/Overflow

// FRIENDLY: *** Swift runtime failure: arithmetic overflow ***

// FRIENDLY: Thread 0 crashed:

// FRIENDLY: 0 level5() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift:30:5

// FRIENDLY: 28|   print("About to overflow")
// FRIENDLY-NEXT: 29|
// FRIENDLY-NEXT: 30|   x -= 1
// FRIENDLY-NEXT:   |     ^
// FRIENDLY-NEXT: 31| }

// FRIENDLY: 1 level4() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift:24:3

// FRIENDLY: 22|
// FRIENDLY-NEXT: 23| func level4() {
// FRIENDLY-NEXT: 24|   level5()
// FRIENDLY-NEXT:   |   ^
// FRIENDLY-NEXT: 25| }
// FRIENDLY-NEXT: 26|

// FRIENDLY: 2 level3() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift:20:3

// FRIENDLY: 18|
// FRIENDLY-NEXT: 19| func level3() {
// FRIENDLY-NEXT: 20|   level4()
// FRIENDLY-NEXT:   |   ^
// FRIENDLY-NEXT: 21| }
// FRIENDLY-NEXT: 22|

// FRIENDLY: 3 level2() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift:16:3

// FRIENDLY: 14|
// FRIENDLY-NEXT: 15| func level2() {
// FRIENDLY-NEXT: 16|   level3()
// FRIENDLY-NEXT:   |   ^
// FRIENDLY-NEXT: 17| }
// FRIENDLY-NEXT: 18|

// FRIENDLY: 4 level1() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift:12:3

// FRIENDLY: 10|
// FRIENDLY-NEXT: 11| func level1() {
// FRIENDLY-NEXT: 12|   level2()
// FRIENDLY-NEXT:   |   ^
// FRIENDLY-NEXT: 13| }
// FRIENDLY-NEXT: 14|

// FRIENDLY: 5 static Overflow.main() + {{[0-9]+}} in Overflow at {{.*}}/Overflow.swift

