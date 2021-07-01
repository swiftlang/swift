// RUN: %target-swiftc_driver -Osize -emit-sil %s -o /dev/null -Xfrontend -verify
// REQUIRES: optimized_stdlib,swift_stdlib_no_asserts

// Make sure we emit remarks on nominal types

@inline(never)
func callPrint(_ s: String) { print(s) }

var global: String = "123"

@_assemblyVision
struct Struct {
    func printMe() {
        callPrint(global) // expected-remark {{begin exclusive access to value of type '}}
                          // expected-note @-6 {{of 'global'}}
                          // expected-remark @-2 {{end exclusive access to value of type '}}
                          // expected-note @-8 {{of 'global'}}
                          // expected-remark @-4 {{retain of type '}}
                          // expected-note @-10 {{of 'global'}}
                          // expected-remark @-6 {{release of type '}}
                          // expected-note @-12 {{of 'global}}
    }
}

// Negative test
struct Struct2 {
    func callPrintMe() {
        callPrint(global)
    }
}

@_assemblyVision
enum Enum {
    func callPrintMe() {
        callPrint(global) // expected-remark {{begin exclusive access to value of type '}}
                          // expected-note @-27 {{of 'global'}}
                          // expected-remark @-2 {{end exclusive access to value of type '}}
                          // expected-note @-29 {{of 'global'}}
                          // expected-remark @-4 {{retain of type '}}
                          // expected-note @-31 {{of 'global'}}
                          // expected-remark @-6 {{release of type '}}
                          // expected-note @-33 {{of 'global}}
    }
}

// Negative test
enum Enum2 {
    func callPrintMe() {
        callPrint("I am callPrinting 1")
    }
}
