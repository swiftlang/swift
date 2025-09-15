// RUN: %target-swift-frontend -parse-as-library -disable-readonly-static-objects -O -emit-ir  %s | %FileCheck %s
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib
// REQUIRES: swift_in_compiler
// REQUIRES: PTRSIZE=64

// This is an end-to-end test to ensure that the optimizer generates
// a simple literal for character literals.

// We generate this as an LLVM constant global directly, no runtime heap
// allocation. Match that.
// CHECK-LABEL: @"{{.*}}charArrayy{{.*}}" ={{.*}} global {{.*}}ContiguousArrayStorage{{.*}} {{.*}}{ i64 97 }{{.*}}{ i64 98 }{{.*}}{ i64 99 }{{.*}}{ i64 100 }{{.*}}
//
// CHECK-LABEL: define {{.*}}charArray
// CHECK:  {{.*}} = tail call ptr @swift_initStaticObject({{.*}} @"{{.*}}charArrayy{{.*}}"
// CHECK: ret
public func charArray(_ i: Int) -> [Character] {
  return [ "a", "b", "c", "d" ]
}

// NOTE: 97 = 'a'
// NOTE: -2233785415175766016 = 0xE1 = 0xE0 (ASCII discrim) | 0x01 (count)
// NOTE: On Android AArch64, this is shifted right by one byte: 0x00E1 = 63331869759897600
//
// CHECK-LABEL: define {{.*}}singleChar
// CHECK-NEXT: entry:
// CHECK-NEXT: ret { i64, ptr } { i64 97, ptr inttoptr (i64 {{-2233785415175766016|63331869759897600}} to ptr) }
public func singleChar() -> Character {
  return "a"
}

// NOTE: 10852326 = 0xE6 0x97 0xA5 (little endian), the encoding of U+65E5
// NOTE: -6701356245527298048 = 0xA3 = 0xA0 (non-ASCII discrim) | 0x03 (count)
// NOTE: On Android AArch64, this is shifted right by one byte: 0x00A3 = 45880421203836928
//
// CHECK-LABEL: define {{.*}}singleNonAsciiChar
// CHECK-NEXT: entry:
// CHECK-NEXT: ret { i64, ptr } { i64 10852326, ptr inttoptr (i64 {{-6701356245527298048|45880421203836928}} to ptr) }
public func singleNonAsciiChar() -> Character {
  return "日"
}

// NOTE: -9223372036854775808 = 0x80 = immortal large discrim
// NOTE: On Android AArch64, this is shifted right by one byte: 0x0080 = 36028797018963968
// NOTE: 1152921504606847001 = 25 (code unit length) | `isTailAllocated` perf flag
//
// CHECK-LABEL: define {{.*}}singleNonSmolChar
// CHECK-NEXT: entry:
// CHECK-DAG:   1152921504606847001
// CHECK-DAG:   @".str.25.\F0\9F\91\A9\E2\80\8D\F0\9F\91\A9\E2\80\8D\F0\9F\91\A6\E2\80\8D\F0\9F\91\A6"
// CHECK-DAG:   {{-9223372036854775808|36028797018963968}}
public func singleNonSmolChar() -> Character {
  return "👩‍👩‍👦‍👦"
}

