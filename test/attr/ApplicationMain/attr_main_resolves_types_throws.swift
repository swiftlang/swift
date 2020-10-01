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
    static func main() throws {
        print("S.main though throwing")
    }
}

// CHECK: S.main though throwing
@main
extension S : P {}

