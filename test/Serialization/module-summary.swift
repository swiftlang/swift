// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/module1.swift -emit-module-summary-path %t/module1.swiftmodule.summary -parse-as-library -o %t
// RUN: %target-swift-frontend -emit-module %S/Inputs/module2.swift -emit-module-summary-path %t/module2.swiftmodule.summary -parse-as-library -I %t -o %t
// RUN: %target-swift-frontend -emit-sil %s -emit-module-summary-path %t/module-summary.swiftmodule.summary -I %t > /dev/null
// RUN: llvm-bcanalyzer -dump %t/module-summary.swiftmodule.summary | %FileCheck %s --check-prefix MAIN-CHECK

// MAIN-CHECK: <METADATA {{.+}} op0=1305169934332876051 op1=0/> blob data = '$s4main10publicFuncyyF'


// Ensure that call graph edge has correct function guid
// MAIN-CHECK:        <METADATA {{.+}} op0=7308225924950623125 op1=0/> blob data = '$s7module20A4FuncSiyF'
// MAIN-CHECK:        <METADATA {{.+}} op0=-2624081020897602054 op1=0/> blob data = 'main'
// MAIN-CHECK-NEXT:   <CALL_GRAPH_EDGE {{.+}} op0=0 op1=1305169934332876051/>
// MAIN-CHECK-NEXT: </FUNCTION_SUMMARY>


// MAIN-CHECK:        <METADATA {{.+}} op0=-1760476766911317517 op1=0/> blob data = '$s4main9callTwiceyyF'
// MAIN-CHECK-NEXT:   <CALL_GRAPH_EDGE {{.+}} op0=0 op1=-8492700279074763592/>
// MAIN-CHECK-NEXT:   <CALL_GRAPH_EDGE {{.+}} op0=0 op1=-432276440123806562/>
// MAIN-CHECK-NEXT: </FUNCTION_SUMMARY>

// RUN: %swift_frontend_plain -cross-module-opt %t/module-summary.swiftmodule.summary %t/module1.swiftmodule.summary %t/module2.swiftmodule.summary -o %t/merged-module.summary
// RUN: llvm-bcanalyzer -dump %t/merged-module.summary | %FileCheck %s --check-prefix MERGED-CHECK

// MERGED-CHECK:        <METADATA {{.+}} op0=1546188077662747336 op1=0/> blob data = '$s7module10A4FuncSiyF'
// MERGED-CHECK-NEXT: </FUNCTION_SUMMARY>

// MERGED-CHECK:        <METADATA {{.+}} op0=7308225924950623125 op1=0/> blob data = '$s7module20A4FuncSiyF'
// MERGED-CHECK-NEXT:   <CALL_GRAPH_EDGE abbrevid=5 op0=0 op1=1546188077662747336/>
// MERGED-CHECK-NEXT: </FUNCTION_SUMMARY>

// RUN: llvm-bcanalyzer -dump %t/merged-module.summary | %FileCheck %s --check-prefix LIVE-CHECK

// LIVE-CHECK-DAG: <METADATA abbrevid=4 op0=1305169934332876051 op1=1/> blob data = '$s4main10publicFuncyyF'
// LIVE-CHECK-DAG: <METADATA abbrevid=4 op0=-8492700279074763592 op1=1/> blob data = '$s4main3fooyyF'
// LIVE-CHECK-DAG: <METADATA abbrevid=4 op0=-2624081020897602054 op1=1/> blob data = 'main'
// LIVE-CHECK-DAG: <METADATA abbrevid=4 op0=-432276440123806562 op1=1/> blob data = '$s4main3baryySiF'

// RUN: %target-swift-frontend -emit-sil %s -I %t -o %t/main.sil
// RUN: %target-sil-opt -emit-sorted-sil %t/main.sil -module-summary-path %t/merged-module.summary --sil-cross-deadfuncelim -I %t | %FileCheck %s --check-prefix DEADFUNC-CHECK
// RUN: %target-swift-frontend -emit-sil %s -module-summary-path %t/merged-module.summary -I %t -O | %FileCheck %s --check-prefix DEADFUNC-CHECK
// DEADFUNC-CHECK-DAG: @$s4main10publicFuncyyF
// DEADFUNC-CHECK-DAG: @$s4main3baryySiF
// DEADFUNC-CHECK-DAG: @main

// DEADFUNC-CHECK-NOT: @$s4main16callExternalFuncyyF
// DEADFUNC-CHECK-NOT: @$sSi2eeoiySbSi_SitFZ
// DEADFUNC-CHECK-NOT: @$s7module20A4FuncSiyF
// DEADFUNC-CHECK-NOT: @$sSi22_builtinIntegerLiteralSiBI_tcfC
// DEADFUNC-CHECK-NOT: @$s4main9callTwiceyyF


import module2

func foo() { bar(0) }
func bar(_ i: Int) {
    if (i == 0) { return }
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

publicFunc()
