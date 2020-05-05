// RUN: %target-run-simple-swift(-parse-as-library) | %FileCheck %s
// REQUIRES: executable_test

protocol P {
}
extension P {
    static func main() {
        print("P.main")
    }
}
struct S {
    static func main() {
        print("S.main though neither throw")
    }
}

// CHECK: S.main though neither throw
@main
extension S : P {}

