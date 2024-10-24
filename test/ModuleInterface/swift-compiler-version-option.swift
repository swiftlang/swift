// RUN: %empty-directory(%t)

/// Build the libraries.
// RUN: %target-swift-frontend %s \
// RUN:   -emit-module-path %t/Lib.swiftmodule \
// RUN:   -emit-module-interface-path %t/Lib.swiftinterface \
// RUN:   -enable-library-evolution -swift-version 6 \
// RUN:   -swift-compiler-version 6.0.0.1

// RUN: %target-swift-typecheck-module-from-interface(%t/Lib.swiftinterface)

/// Check option in swiftinterface
// RUN: cat %t/Lib.swiftinterface | %FileCheck --check-prefix=CHECK-OPTION %s
// CHECK-OPTION: swift-module-flags:
// CHECK-SAME-OPTION: -swift-compiler-version 6.0.0.1

/// Check option in swiftmodule
// RUN: llvm-bcanalyzer --dump %t/Lib.swiftmodule | %FileCheck --check-prefix=CHECK-MODULE-OPTION %s
// CHECK-MODULE-OPTION: <OPTIONS_BLOCK
// CHECK-MODULE-OPTION: <SWIFT_COMPILER_VERSION abbrevid={{.*}}/> blob data = '6.0.0.1'
// CHECK-MODULE-OPTION: </OPTIONS_BLOCK>

public struct S {
    public var test: Int = 42
}
