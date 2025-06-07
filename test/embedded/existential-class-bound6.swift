// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo -O) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo -Osize) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public var global: AnyObject? = nil

func test(anyObject: AnyObject) {
    global = anyObject
}

class MyClass {}
class MyOtherClass {}
protocol MyProto: AnyObject {}
extension MyClass: MyProto {}
extension MyOtherClass: MyProto {}

@main
struct Main {
    static func main() {
        test(anyObject: MyClass())
        test(anyObject: MyOtherClass())
        test(anyObject: MyClass() as AnyObject)
        test(anyObject: MyOtherClass() as AnyObject)
        test(anyObject: MyClass() as MyProto)
        test(anyObject: MyOtherClass() as MyProto)
        print("OK!")
        // CHECK: OK!
    }
}
