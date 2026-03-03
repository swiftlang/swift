// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo -swift-version 5) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo -O -swift-version 5) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo -Osize -swift-version 5) | %FileCheck %s

// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo -swift-version 6) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo -O -swift-version 6) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo -Osize -swift-version 6) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

protocol P: AnyObject {}

class C: P {}

@main
struct Main {
    static func main() {
        let p1: any P = C()
        let p2: any P = C()
        let p3 = p1

        // CHECK: false
        print(p1 === p2)
        print(p2 === p1)
        // CHECK: true
        print(p1 === p3)
        print(p3 === p1)
        // CHECK: false
        print(p2 === p3)
        print(p3 === p2)
   }
}

