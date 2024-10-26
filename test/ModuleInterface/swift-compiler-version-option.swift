// RUN: %empty-directory(%t)

/// Build the libraries.
// RUN: %target-swift-frontend %s \
// RUN:   -module-name Lib \
// RUN:   -emit-module-path %t/Lib.swiftmodule \
// RUN:   -emit-module-interface-path %t/Lib.swiftinterface \
// RUN:   -enable-library-evolution \
// RUN:   -swift-version 6

// RUN: %target-swift-typecheck-module-from-interface(%t/Lib.swiftinterface)

/// Check option in swiftinterface
// RUN: cat %t/Lib.swiftinterface | %FileCheck --check-prefix=CHECK-OPTION %s
// CHECK-OPTION: swift-module-flags-ignorable:
// CHECK-SAME-OPTION: -swift-compiler-version {{.*}}

/// Check option in swiftmodule
// RUN: llvm-bcanalyzer --dump %t/Lib.swiftmodule | %FileCheck --check-prefix=CHECK-MODULE-OPTION %s
// CHECK-MODULE-OPTION: <OPTIONS_BLOCK
// CHECK-MODULE-OPTION: <SWIFT_COMPILER_VERSION abbrevid={{.*}}/> blob data = '{{.*}}'
// CHECK-MODULE-OPTION: </OPTIONS_BLOCK>

// Drop and rebuilt swiftmodule to make sure that the version is inferred from the interface file.
// RUN: rm %t/Lib.swiftmodule
// RUN: %target-swift-frontend -compile-module-from-interface %t/Lib.swiftinterface -o %t/Lib.swiftmodule -module-name Lib

/// Check option in swiftmodule
// RUN: llvm-bcanalyzer --dump %t/Lib.swiftmodule | %FileCheck --check-prefix=CHECK-REBUILT-MODULE-OPTION %s
// CHECK-REBUILT-MODULE-OPTION: <OPTIONS_BLOCK
// CHECK-REBUILT-MODULE-OPTION: <SWIFT_COMPILER_VERSION abbrevid={{.*}}/> blob data = '{{.*}}'
// CHECK-REBUILT-MODULE-OPTION: </OPTIONS_BLOCK>

public struct S {
    public var test: Int = 42
}
