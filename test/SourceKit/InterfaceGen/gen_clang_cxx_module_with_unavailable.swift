// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %sourcekitd-test -req=interface-gen -module CxxModule -- -Xfrontend -disable-implicit-concurrency-module-import -Xfrontend -disable-implicit-string-processing-module-import -Xfrontend -enable-experimental-cxx-interop -I %t/Inputs -target %target-triple %clang-importer-sdk-nosource | %FileCheck %s

//--- Inputs/module.modulemap
module CxxModule {
    header "headerA.h"
    requires cplusplus
}

//--- Inputs/headerA.h

template<class T>
struct X {
public:
    int x;
    X(): x(0) {}

    int *xy();
};

using XofInt = X<int>;

//--- test.swift

import CxxModule

func testMe() {
    var v = XofInt();
    print(v.x);
}

// CHECK: @available(*, unavailable, message: "Un-specialized class templates are not currently supported. Please use a specialization of this type.")
// CHECK-NEXT: public struct X<T> {
// CHECK-NEXT: }
// CHECK: public struct X<CInt> {
// CHECK: public typealias XofInt = X<CInt>
