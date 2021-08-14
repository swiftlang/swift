// BEGIN TheModule.swift
public struct Machine {
    public init() {}
}

public func machineFunc() {}

// BEGIN main.swift
import TheModule
public struct MyMachine {}

struct Local {
    var machineHidden: Bool { true }
    func hideMachine() -> Void {}

    func test() {
        #^COMPLETE,,M,Ma,Mac,Mach^#
    }
}

// RUN: %empty-directory(%t/src)
// RUN: %{python} %utils/split_file.py -o %t/src %s

// RUN: %empty-directory(%t/Modules)
// RUN: %target-swift-frontend -emit-module %t/src/TheModule.swift -module-name TheModule -o %t/Modules/TheModule.swiftmodule

// RUN: %complete-test -tok=COMPLETE %t/src/main.swift -- -target %target-triple -I %t/Modules | %FileCheck --check-prefix=CHECK %s 

// CHECK-LABEL: Results for filterText:  [
// CHECK: hideMachine()
// CHECK: machineHidden
// CHECK: MyMachine
// CHECK: ]

// CHECK-LABEL: Results for filterText: M [
// CHECK: MyMachine
// CHECK: Machine
// CHECK: machineHidden
// CHECK: machineFunc()
// CHECK: ]

// CHECK-LABEL: Results for filterText: Ma [
// CHECK: Machine
// CHECK: machineHidden
// CHECK: hideMachine()
// CHECK: machineFunc()
// CHECK: MyMachine
// CHECK: ]

// CHECK-LABEL: Results for filterText: Mac [
// CHECK: Machine
// CHECK: machineHidden
// CHECK: machineFunc()
// CHECK: hideMachine()
// CHECK: MyMachine
// CHECK: ]

// CHECK-LABEL: Results for filterText: Mach [
// CHECK: Machine
// CHECK: machineHidden
// CHECK: machineFunc()
// CHECK: hideMachine()
// CHECK: MyMachine
// CHECK: ]

