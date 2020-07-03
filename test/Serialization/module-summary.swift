// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-summary %s -o %t
// RUN: llvm-bcanalyzer -dump %t/*.swiftmodule.summary | %FileCheck %s
// CHECK: <FUNCTION_SUMMARY
// CHECK: <METADATA {{.+}} op0=1305169934332876051/> blob data = '$s4main10publicFuncyyF'

func foo() { bar(0) }
func bar(_ i: Int) {
    if (i == 0) { return  }
    foo()
}

public func publicFunc() {
    foo()
}

public struct X {
    func method1() {
        publicFunc()
    }
}
