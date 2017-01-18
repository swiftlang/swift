extension JSValue {
    public init() {

    }
}


// RUN: %sourcekitd-test -req=complete -pos=6:1 %s -- %s %S/Inputs/complete_typealias_different_file_2.swift | %FileCheck %s
// CHECK: "Foo"
