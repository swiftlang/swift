// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-stdlib -whole-module-optimization -D LIBRARY -emit-module -o %t/Library.swiftmodule
// RUN: %target-build-swift %s -parse-stdlib -whole-module-optimization -D LIBRARY -emit-bc -o %t/Library.bc
// RUN: %target-build-swift %s -parse-as-library -D CLIENT -I %t -o %t/R1 -O            && %target-codesign %t/R1 && %target-run %t/R1 | %FileCheck %s
// RUN: %target-build-swift %s -parse-as-library -D CLIENT -I %t -o %t/R2 -O -D FOO_BAR && %target-codesign %t/R2 && %target-run %t/R2 | %FileCheck %s --check-prefix CHECK-FOO-BAR

// REQUIRES: executable_test

#if LIBRARY

  @_alwaysEmitIntoClient
  public func ifdefFooBar() -> Builtin.Int1 {
    return Builtin.ifdef_FOO_BAR()
  }

#endif

#if CLIENT

  import Library

  @_cdecl("main")
  func main() -> Int32 {
    print("Hello")
    print("\(Bool(_builtinBooleanLiteral: ifdefFooBar()))")
    // CHECK: Hello
    // CHECK: false

    // CHECK-FOO-BAR: Hello
    // CHECK-FOO-BAR: true
    return 0
  }

#endif
