func /*test:def*/test(x: () -> () = {}, _ xx: () -> () = {}, y: () -> () = {}, z: ()->() = {}) {}

/*test:call*/test(x: {}, {}) {}
/*test:call*/test(x: {}) {} z: {}
/*test:call*/test(x: {}, {}) {}
/*test:call*/test {} y: {}
/*test:call*/test(y: {}) {}


// REQUIRES: swift_swift_parser
// RUN: %empty-directory(%t.ranges)
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="test" -is-function-like -old-name "test(x:_:y:z:)" >> %t.ranges/multiple-trailing-closures-defaulted.swift.expected
// RUN: diff -u %S/Outputs/multiple-trailing-closures-defaulted.swift.expected %t.ranges/multiple-trailing-closures-defaulted.swift.expected
