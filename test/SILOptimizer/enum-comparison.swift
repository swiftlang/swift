// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O -module-name=test %s -o %t/a.out
// RUN: %target-build-swift -O -module-name=test %s -emit-ir | %FileCheck %s
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --check-prefix=OUT

// REQUIRES: executable_test,optimized_stdlib
// REQUIRES: PTRSIZE=64

enum E: String {
  case a, b, c, long_case_name_for_testing, d, e
}

// CHECK-LABEL: define {{.*}} i1 @"$s4test9compareeqySbAA1EO_ADtF"(i8 %0, i8 %1)
// CHECK:         %2 = icmp eq i8 %0, %1
// CHECK-NEXT:    ret i1 %2
@inline(never)
func compareeq(_ a: E, _ b: E) -> Bool {
  return a == b
}

// CHECK-LABEL: define {{.*}} i1 @"$s4test9compareneySbAA1EO_ADtF"(i8 %0, i8 %1)
// CHECK:         %2 = icmp ne i8 %0, %1
// CHECK-NEXT:    ret i1 %2
@inline(never)
func comparene(_ a: E, _ b: E) -> Bool {
  return a != b
}

enum LargeEnum: Equatable {
  case a1, a2, a3, a4, a5, a6, a7, a8, a9
  case b1, b2, b3, b4, b5, b6, b7, b8, b9
  case c1, c2, c3, c4, c5, c6, c7, c8, c9
  case d1, d2, d3, d4, d5, d6, d7, d8, d9
  case e1(Int64), e2(Int64), e3(Int64), e4(Int64), e5(Int64), e6(Int64), e7(Int64), e8(Int64), e9(Int64)
  case f1, f2, f3, f4, f5, f6, f7, f8, f9
  case g1, g2, g3, g4, g5, g6, g7, g8, g9
}

// CHECK-LABEL: define {{.*}} i1 @"$s4test8compare1ySbAA9LargeEnumOF"(i64 %0, i8 %1)
// CHECK:       entry:
// CHECK-NEXT:    icmp
// CHECK-NEXT:    icmp
// CHECK-NEXT:    {{(and|select)}}
// CHECK-NEXT:    ret
@inline(never)
func compare1(_ x: LargeEnum) -> Bool {
  return x == .b2
}

// CHECK-LABEL: define {{.*}} i1 @"$s4test8compare2ySbAA9LargeEnumOF"(i64 %0, i8 %1)
// CHECK:       entry:
// CHECK-NEXT:    icmp
// CHECK-NEXT:    icmp
// CHECK-NEXT:    {{(and|select)}}
// CHECK-NEXT:    ret
@inline(never)
func compare2(_ x: LargeEnum) -> Bool {
  return .f2 == x
}

// CHECK-LABEL: define {{.*}} i1 @"$s4test8compare3ySbAA9LargeEnumOF"(i64 %0, i8 %1)
// CHECK:       entry:
// CHECK-NEXT:    icmp
// CHECK-NEXT:    icmp
// CHECK-NEXT:    {{(and|select)}}
// CHECK-NEXT:    ret
@inline(never)
func compare3(_ x: LargeEnum) -> Bool {
  return .e2(27) == x
}

// CHECK-LABEL: define {{.*}} i1 @"$s4test8compare4ySbAA9LargeEnumOF"(i64 %0, i8 %1)
// CHECK:       entry:
// CHECK-NEXT:    icmp
// CHECK-NEXT:    icmp
// CHECK-NEXT:    {{(and|select)}}
// CHECK-NEXT:    ret
@inline(never)
func compare4(_ x: LargeEnum) -> Bool {
  return x == .e3(28)
}

enum CustomRawValue: RawRepresentable {
  case a, b, c

  init(rawValue: Int) {
    self = .a
  }

  @inline(never)
  var rawValue: Int {
    print(0)
    return 0
  }
}

// CHECK-LABEL: define {{.*}} i1 @"$s4test8compare5ySbAA14CustomRawValueOF"(i8 %0)
// CHECK:       entry:
// CHECK-NEXT:    call
@inline(never)
func compare5(_ x: CustomRawValue) -> Bool {
  return x == .b
}

// OUT: 1: false
print("1: \(compareeq(.c, .long_case_name_for_testing))")

// OUT: 2: true
print("2: \(compareeq(.c, .c))")

// OUT: 3: true
print("3: \(comparene(.c, .long_case_name_for_testing))")

// OUT: 4: false
print("4: \(comparene(.c, .c))")

// OUT: 5: false
print("5: \(compare1(.b1))")

// OUT: 6: true
print("6: \(compare1(.b2))")

// OUT: 7: false
print("7: \(compare2(.b1))")

// OUT: 8: true
print("8: \(compare2(.f2))")

// OUT: 9: true
print("9: \(compare3(.e2(27)))")

// OUT: 10: false
print("10: \(compare3(.e2(28)))")

// OUT: 11: true
print("11: \(compare4(.e3(28)))")

// OUT: 12: false
print("12: \(compare4(.e3(27)))")

// OUT: 13: true
print("13: \(compare5(.a))")

