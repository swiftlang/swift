// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test
// TODO: SIL optimizations cause a miscompile of deinit rdar://105798769
// REQUIRES: swift_test_mode_optimize_none

class C {
    var value: Int
    init(value: Int) { self.value = value }

    deinit { print("C died \(value)") }
}

struct Butt: ~Copyable {
    static var myButt: () -> () = {}

    init(value: Int) { self._value = C(value: value) }

    // TODO: work around crash when move-only type has a deinit and no
    // nontrivial fields by putting a class here

    // TODO: work around crash when we export a setter from a move-only type
    // by making this stored property private
    private var _value: C

    var value: Int { return _value.value }

    deinit { print("Butt died \(value)") }
}

func foo() {
    let butt = Butt(value: 42)

    Butt.myButt = { print(butt.value) }
}

func bar() {
    // CHECK: 42
    Butt.myButt()
    // CHECK-NEXT: done calling
    print("done calling")
    // CHECK-NEXT: Butt died 42
    // CHECK-NEXT: C died 42
    Butt.myButt = {}
    // CHECK-NEXT: done replacing closure
    print("done replacing closure")
}

foo()
bar()
