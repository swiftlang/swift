// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -Onone -g -o %t/FatalError
// RUN: %target-codesign %t/FatalError
// RUN: (env SWIFT_BACKTRACE=enable=yes,cache=no %target-run %t/FatalError 2>&1 || true) | %FileCheck %s

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: asan
// REQUIRES: executable_test
// REQUIRES: backtracing
// REQUIRES: OS=linux-gnu

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
  fatalError("Going to crash")
}

@main
struct FatalError {
  static func main() {
    level1()
  }
}

// CHECK-NOT: Current stack trace:

// CHECK: *** Program crashed: {{Illegal instruction|System trap}} at 0x{{[0-9a-f]+}} ***

// CHECK: Thread 0 {{(".*" )?}}crashed:

// CHECK:      0               0x{{[0-9a-f]+}} _assertionFailure(_:_:file:line:flags:) + {{[0-9]+}} in libswiftCore.{{so|dylib}}
// CHECK-NEXT: 1 [ra]          0x{{[0-9a-f]+}} level5() + {{[0-9]+}} in FatalError at {{.*}}/FatalError.swift:30:3
// CHECK-NEXT: 2 [ra]          0x{{[0-9a-f]+}} level4() + {{[0-9]+}} in FatalError at {{.*}}/FatalError.swift:26:3
// CHECK-NEXT: 3 [ra]          0x{{[0-9a-f]+}} level3() + {{[0-9]+}} in FatalError at {{.*}}/FatalError.swift:22:3
// CHECK-NEXT: 4 [ra]          0x{{[0-9a-f]+}} level2() + {{[0-9]+}} in FatalError at {{.*}}/FatalError.swift:18:3
// CHECK-NEXT: 5 [ra]          0x{{[0-9a-f]+}} level1() + {{[0-9]+}} in FatalError at {{.*}}/FatalError.swift:14:3
// CHECK-NEXT: 6 [ra]          0x{{[0-9a-f]+}} static FatalError.main() + {{[0-9]+}} in FatalError at {{.*}}/FatalError.swift:36:5
// CHECK-NEXT: 7 [ra] [system] 0x{{[0-9a-f]+}} static FatalError.$main() + {{[0-9]+}} in FatalError at {{.*}}/<compiler-generated>
// CHECK-NEXT: 8 [ra] [system] 0x{{[0-9a-f]+}} main + {{[0-9]+}} in FatalError at {{.*}}/FatalError.swift
