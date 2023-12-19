struct Foo {
    /*test:def*/subscript(`x` y: Int) -> Int { fatalError() }
}
Foo()/*test:ref*/[`x`: 10]

// REQUIRES: swift_swift_parser
// RUN: %empty-directory(%t.ranges)
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="test" -is-function-like -old-name "subscript(x:)" >> %t.ranges/backticks-noncollapsible-separate-arglabel.swift.expected
// RUN: diff -u %S/Outputs/backticks-noncollapsible-separate-arglabel.swift.expected %t.ranges/backticks-noncollapsible-separate-arglabel.swift.expected
