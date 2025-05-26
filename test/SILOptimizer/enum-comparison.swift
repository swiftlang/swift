// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O -module-name=test %s -o %t/a.out
// RUN: %target-build-swift -O -module-name=test %s -emit-ir | %FileCheck %s
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --check-prefix=OUT

// REQUIRES: executable_test,optimized_stdlib

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


// OUT: 1: false
print("1: \(compareeq(.c, .long_case_name_for_testing))")

// OUT: 2: true
print("2: \(compareeq(.c, .c))")

// OUT: 3: true
print("3: \(comparene(.c, .long_case_name_for_testing))")

// OUT: 4: false
print("4: \(comparene(.c, .c))")

