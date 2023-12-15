struct Test {
    var base: Int
    /*test:def*/init(base: Int) {}
}

_ = /*test:call*/Test(base: 3)
_ = Test . /*test:call*/init(base: 3)
_ = Test . /*test:ref*/init
_ = Test . /*test:ref*/init(base:)


// REQUIRES: swift_swift_parser
// RUN: %empty-directory(%t.ranges)
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="test" -is-function-like -old-name "init(base:)" >> %t.ranges/init.swift.expected
// RUN: diff -u %S/Outputs/init.swift.expected %t.ranges/init.swift.expected

