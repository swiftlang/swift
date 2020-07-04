// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/module1.swift -parse-as-library -o %t
// RUN: %target-swift-frontend -emit-module-summary %S/Inputs/module1.swift -parse-as-library -o %t
// RUN: %target-swift-frontend -emit-module %S/Inputs/module2.swift -parse-as-library -I %t -o %t
// RUN: %target-swift-frontend -emit-module-summary %S/Inputs/module2.swift -parse-as-library -I %t -o %t
// RUN: %target-swift-frontend -emit-module-summary %s -I %t -o %t
// RUN: llvm-bcanalyzer -dump %t/module-summary.swiftmodule.summary | %FileCheck %s --check-prefix MAIN-CHECK

// MAIN-CHECK: <METADATA {{.+}} op0=1305169934332876051/> blob data = '$s4main10publicFuncyyF'

// MAIN-CHECK:        <METADATA {{.+}} op0=7308225924950623125/> blob data = '$s7module20A4FuncSiyF'
// MAIN-CHECK-NEXT: </FUNCTION_SUMMARY>


// MAIN-CHECK:        <METADATA {{.+}} op0=-1760476766911317517/> blob data = '$s4main9callTwiceyyF'
// MAIN-CHECK-NEXT:   <CALL_GRAPH_EDGE {{.+}} op0=0 op1=-8492700279074763592/>
// MAIN-CHECK-NEXT:   <CALL_GRAPH_EDGE {{.+}} op0=0 op1=-432276440123806562/>
// MAIN-CHECK-NEXT: </FUNCTION_SUMMARY>


// RUN: %swift_frontend_plain -cross-module-opt %t/module-summary.swiftmodule.summary %t/module1.swiftmodule.summary %t/module2.swiftmodule.summary -o %t/merged-module.summary
// RUN: llvm-bcanalyzer -dump %t/merged-module.summary | %FileCheck %s --check-prefix MERGED-CHECK

// MERGED-CHECK:        <METADATA {{.+}} op0=1546188077662747336/> blob data = '$s7module10A4FuncSiyF'
// MERGED-CHECK-NEXT: </FUNCTION_SUMMARY>

// MERGED-CHECK:        <METADATA {{.+}} op0=7308225924950623125/> blob data = '$s7module20A4FuncSiyF'
// MERGED-CHECK-NEXT:   <CALL_GRAPH_EDGE abbrevid=5 op0=0 op1=1426602696/>
// MERGED-CHECK-NEXT: </FUNCTION_SUMMARY>

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
