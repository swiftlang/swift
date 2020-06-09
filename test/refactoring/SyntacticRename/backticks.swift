func /*test:def*/`foo`(`x`: Int) {}
/*test:call*/`foo`(`x`: 2)
_ = /*test:ref*/`foo`(`x`:)

// RUN: %empty-directory(%t.ranges)
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="test" -is-function-like -old-name "foo(x:)" >> %t.ranges/backticks.swift.expected
// RUN: diff -u %S/FindRangeOutputs/backticks.swift.expected %t.ranges/backticks.swift.expected
