// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

// This test verifies that closures with parameter types using pack generics
// from an enclosing context compile without crashing.
//
// Previously, this would crash with:
// Assertion failed: (origPackType.matchesPack(substPackType)),
// function PackElementGenerator, file AbstractionPattern.cpp

struct Box<each T> {}

func takeClosure<each U>(_: (repeat each U), _: (Box<repeat each U>) -> Void) {}

// CHECK-LABEL: sil hidden [ossa] @$s28pack_closure_parameter_crash4testyyF
// CHECK: function_ref
// CHECK: } // end sil function '$s28pack_closure_parameter_crash4testyyF'
func test() {
    takeClosure((1, ""), { _ in })
}

// More complex case with protocol method returning pack-parameterized type
protocol Plugin {
    func handle<each T>(event: Box<repeat each T>) -> [Box<repeat each T>]
}

struct MyPlugin: Plugin {
    func handle<each T>(event: Box<repeat each T>) -> [Box<repeat each T>] { [] }
}

struct Processor<each P: Plugin> {
    let plugins: (repeat each P)

    func process<each Input>(
        event: Box<repeat each Input>,
        execute: (Box<repeat each Input>) -> Void
    ) {
        for plugin in repeat each plugins {
            for action in plugin.handle(event: event) {
                execute(action)
            }
        }
    }
}

// CHECK-LABEL: sil hidden [ossa] @$s28pack_closure_parameter_crash12testComplexyyF
// CHECK: } // end sil function '$s28pack_closure_parameter_crash12testComplexyyF'
func testComplex() {
    let p = Processor(plugins: (MyPlugin(),))
    p.process(event: Box<Int, String>()) { _ in }
}
