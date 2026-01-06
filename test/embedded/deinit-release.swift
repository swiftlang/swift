// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -enable-experimental-feature Embedded -O -c -o %t/main.o
// RUN: %target-clang %target-clang-resource-dir-opt %t/main.o -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded

class C {}

struct Foo {
    let foo: C = C()
    let bar: C = C()
}

class Bar {}
class SubBar: Bar {
    var qwe = Foo()
}

var bar: SubBar? = SubBar()
bar = nil
print("OK!")

// CHECK: OK!
