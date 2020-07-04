// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/module1.swift -parse-as-library -o %t
// RUN: %target-swift-frontend -emit-module-summary %S/Inputs/module1.swift -parse-as-library -o %t
// RUN: %target-swift-frontend -emit-module %S/Inputs/module2.swift -parse-as-library -I %t -o %t
// RUN: %target-swift-frontend -emit-module-summary %S/Inputs/module2.swift -parse-as-library -I %t -o %t
// RUN: %target-swift-frontend -emit-module-summary %s -I %t -o %t
// RUN: llvm-bcanalyzer -dump %t/module-summary.swiftmodule.summary | %FileCheck %s
// CHECK: <FUNCTION_SUMMARY
// CHECK: <METADATA {{.+}} op0=1305169934332876051/> blob data = '$s4main10publicFuncyyF'

// RUN: %swift_frontend_plain -cross-module-opt %t/module-summary.swiftmodule.summary %t/module1.swiftmodule.summary %t/module2.swiftmodule.summary -o %t/merged-module.summary

import module2

func foo() { bar(0) }
func bar(_ i: Int) {
    if (i == 0) { return  }
    foo()
}

func callTwice() {
    foo()
    bar(0)
}

func callExternalFunc() {
    _ = module2Func()
}

public func publicFunc() {
    foo()
}

public struct X {
    func method1() {
        publicFunc()
    }
}
