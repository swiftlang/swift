func /*test:def*/test(x: () -> (), _ xx: () -> (), y: () -> (), z: ()->()) {}

/*test:call*/test(x: {}, {}, y: {}) {}
/*test:call*/test(x: {}, {}) {} z: {}
/*test:call*/test(x: {}) {} y: {} z: {}
/*test:call*/test {} _: {} y: {} z: {}


// REQUIRES: swift_swift_parser
// RUN: %empty-directory(%t.ranges)
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="test" -is-function-like -old-name "test(x:_:y:z:)" >> %t.ranges/multiple-trailing-closures.swift.expected
// RUN: diff -u %S/Outputs/multiple-trailing-closures.swift.expected %t.ranges/multiple-trailing-closures.swift.expected
