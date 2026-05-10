// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %refactor -find-rename-ranges -source-filename %t/input.swift -pos="test" -old-name "+(x:y:)" -new-name "-(x:y:)" > %t/output.txt
// RUN: diff -u %t/expected.swift %t/output.txt

// REQUIRES: swift_swift_parser

//--- input.swift

struct Foo {}
func /*test:def*/+(x: Foo, y: Foo) {}
Foo() /*test:ref*/+ Foo()

//--- expected.swift

struct Foo {}
func /*test:def*/<base>+</base>(x: Foo, y: Foo) {}
Foo() /*test:ref*/<base>+</base> Foo()

