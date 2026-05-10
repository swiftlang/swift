struct Adder {
    var base: Int

    func callAsFunction(x: Int, y: Int) -> Int {
        return base + x + y
    }
}

let /*test:def*/add3 = Adder(base: 3)
/*test:ref*/add3(x: 10, y: 11)
let blah = /*test:ref*/add3.callAsFunction(x:y:)

// REQUIRES: swift_swift_parser
// RUN: %empty-directory(%t.ranges)
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="test" -old-name "add3" >> %t.ranges/call-as-function-base.swift.expected
// RUN: diff -u %S/Outputs/call-as-function-base.swift.expected %t.ranges/call-as-function-base.swift.expected



