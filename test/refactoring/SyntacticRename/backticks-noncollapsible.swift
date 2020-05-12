struct Foo {
    /*test:def*/subscript(`x`: Int) -> Int { fatalError() }
}
Foo()/*test:ref*/[10]

// RUN: %empty-directory(%t.ranges)
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="test" -is-function-like -old-name "subscript(_:)" >> %t.ranges/backticks-noncollapsible.swift.expected
// RUN: diff -u %S/FindRangeOutputs/backticks-noncollapsible.swift.expected %t.ranges/backticks-noncollapsible.swift.expected
