// RUN: %empty-directory(%t)

// RUN: %target-build-swift %s -profile-generate -profile-coverage-mapping -o %t/main

// RUN: %target-codesign %t/main
// RUN: env %env-LLVM_PROFILE_FILE=%t/default.profraw %target-run %t/main

// RUN: %llvm-profdata merge %t/default.profraw -o %t/default.profdata
// RUN: %llvm-cov export -summary-only %t/main -instr-profile=%t/default.profdata | %FileCheck --check-prefix SUMMARY %s
// RUN: %llvm-cov show %t/main -instr-profile=%t/default.profdata | %FileCheck --implicit-check-not "{{[ ]*\|[ ]*[0-9]+}}" %s

// REQUIRES: profile_runtime
// REQUIRES: executable_test

// The line count here excludes the inactive regions.
// SUMMARY: "lines":{"count":41,"covered":0

// This test works by marking active lines with the pattern. The implicit
// CHECK-NOT in the FileCheck invocation ensures everything else is a skipped
// region (or doesn't have any coverage mapping such as these comments).

func poundIfDecl() -> Int {  // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
  #if true
    #if true
    return 1                 // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
    #else
    return 2
    #endif
  #else
    #if true
    return 3
    #else
    return 4
    #endif
  #endif
}                            // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}

func poundIfMember(
  _ x: [Int]
) -> [Int] {                 // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
  return x                   // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
  #if false
    .map { $0 + 1 }
  #endif
}                            // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}

func poundIfAttr() -> Any {  // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
#if false
  @objc
#endif
  class C {}                 // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
  return C()                 // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
}                            // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}

func poundIfSwitch(
  _ x: Bool
) -> Int {                   // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
  switch x {                 // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
  #if true
  case true:                 // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
    return 0                 // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
  #else
  case false:
    return 0
  #endif
  #if false
  case false:
    return 1
  #endif
  case false:                // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
    return 0                 // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
  }                          // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
}                            // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}

func poundIfMember() {       // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
  struct S {                 // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
#if false
    var foo: Int
#else
    var foo: Int             // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
#endif
  }                          // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
}                            // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}

func poundIfDecl2() -> Int { // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
  #if true
  struct S {                 // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
    var foo: Int             // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
  }                          // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
  #else
  struct S {
    var foo: Int
  }
  #endif
  return S(foo: 0).foo       // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
}                            // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}

func emptyPoundIf1(
) -> Int {                   // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
  #if false
  #endif
  return 0                   // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
}                            // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}

func emptyPoundIf2(
) -> Int {                   // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
  #if true
  #endif
  return 0                   // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
}                            // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}

func emptyPoundIf3(
) -> Int {                   // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
  #if true
  #else
  #endif
  return 0                   // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
}                            // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}

func poundEndIfSameLine(
) -> Int {                   // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
  #if true
  return 0                   // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
  #else
  return 1#endif
}                            // CHECK: [[@LINE]]{{[ ]*\|[ ]*[0-9]+}}
