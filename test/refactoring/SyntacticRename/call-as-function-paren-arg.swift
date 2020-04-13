struct Adder {
    var base: Int
    func /*test:def*/callAsFunction(_ x: Int) -> Int {
        return base + x
    }
}

let add3 = Adder(base: 3)
_ = add3/*test:call*/(10)
_ = add3 . /*test:call*/callAsFunction(10)
_ = add3 . /*test:ref*/callAsFunction(_:)
_ = add3 . /*test:ref*/callAsFunction

// RUN: %empty-directory(%t.ranges)
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="test" -is-function-like -old-name "callAsFunction(_:)" >> %t.ranges/call-as-function-paren-arg.swift.expected
// RUN: diff -u %S/FindRangeOutputs/call-as-function-paren-arg.swift.expected %t.ranges/call-as-function-paren-arg.swift.expected
