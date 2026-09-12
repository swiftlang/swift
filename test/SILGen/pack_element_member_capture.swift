// RUN: %target-swift-emit-silgen %s | %FileCheck %s

// Test that pack element captures that reference member expressions correctly
// capture the base variable (e.g., self) in enclosing closures.

protocol P {
    associatedtype Value
    func get() -> Value
}

struct S: P {
    typealias Value = Int
    func get() -> Int { 42 }
}

final class Ops<each M: P> {
    let items: (repeat each M)

    init(items: (repeat each M)) {
        self.items = items
    }

    // This test case verifies that when a pack element capture `(each self.items)`
    // is used inside an immediately-invoked closure within a pack expansion,
    // the outer closure correctly captures `self` so it's available when emitting
    // the inner closure's captures.
    func test() -> [(repeat (each M).Value)] {
        return (0..<2).map { index in
            var counter = 0
            return (repeat {
                counter += 1
                return (each self.items).get()
            }())
        }
    }
}

// CHECK-LABEL: sil hidden [ossa] @$s27pack_element_member_capture3OpsC4testSay5ValueQzxQp_tGyF
// CHECK: function_ref @$s27pack_element_member_capture3OpsC4testSay5ValueQzxQp_tGyFAFxQp_tSiXEfU_
// The outer closure should capture self
// CHECK-LABEL: sil private [ossa] @$s27pack_element_member_capture3OpsC4testSay5ValueQzxQp_tGyFAFxQp_tSiXEfU_
// The closure captures both index and self
// CHECK: bb0({{.*}}, {{.*}}, {{.*}}, [[SELF:%[0-9]+]] : @closureCapture @guaranteed $Ops<repeat each M>):
// CHECK: debug_value [[SELF]], let, name "self"
