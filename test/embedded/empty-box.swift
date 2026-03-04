// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -wmo %target-embedded-posix-shim) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded

public struct ZeroSizedStruct {}

var escape: (()->())? = nil

public func sink<T>(t: inout T) {

}

public func foo() {
    var s = ZeroSizedStruct() 
    escape = {
        sink(t: &s)
    }
}

foo()
print("OK!")
// CHECK: OK!
