// RUN: %target-swift-emit-silgen -g -Xllvm -sil-print-debuginfo %s | %FileCheck %s

class A {
    func foo() {
        { [weak self] in
// Check that column 14 -- the start "guard" -- is used as the debug location,
// as other locations are considered implicit.
// CHECK:       switch_enum
// CHECK-SAME:  guardlet_debuginfo.swift":[[@LINE+1]]:13
            guard let self else { return }
            print(self)
        }()
    }
}

let a = A()
a.foo()
