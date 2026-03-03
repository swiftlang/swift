// RUN: %target-build-swift -parse-as-library -O %s -module-name=test -emit-sil | %FileCheck %s

// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -parse-as-library -O -module-name=test %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-OUTPUT

// REQUIRES: executable_test,swift_stdlib_no_asserts,optimized_stdlib
// UNSUPPORTED: use_os_stdlib


@available(SwiftStdlib 5.8, *)
public enum StaticIntStringEnum {
  case s(StaticString)
  case i(StaticBigInt)

  // CHECK-LABEL: sil_global hidden @$s4test19StaticIntStringEnumO1xACvpZ : $StaticIntStringEnum = {
  static var x = Self.i(27)
}

@available(SwiftStdlib 5.8, *)
@main
struct Main {
  static func main() {
    // CHECK-OUTPUT: StaticIntStringEnum: i(+0x1B)
    print("StaticIntStringEnum:", StaticIntStringEnum.x)
  }
}

