struct Adder {
    var base: Int
    func /*test:def*/callAsFunction(x: Int, y: Int) -> Adder {
        return self
    }
}

let add3 = Adder(base: 3)
_ = add3/*test:call*/(x: 10, y: 11)
_ = add3 . /*test:call*/callAsFunction(x: 10, y: 11)
_ = add3 . /*test:ref*/callAsFunction(x:y:)
_ = add3 . /*test:ref*/callAsFunction
_ = (add3 . /*test:call*/callAsFunction())/*test:call*/(x: 10, y: 11)

// REQUIRES: swift_swift_parser
// RUN: %empty-directory(%t.ranges)
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="test" -is-function-like -old-name "callAsFunction(x:y:)" >> %t.ranges/call-as-function.swift.expected
// RUN: diff -u %S/Outputs/call-as-function.swift.expected %t.ranges/call-as-function.swift.expected
