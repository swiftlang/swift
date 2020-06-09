// RUN: %target-run-simple-swift(-parse-as-library) | %FileCheck %s
// REQUIRES: executable_test

protocol P {
}
extension P {
    static func main() throws {
        print("P.main")
    }
}
struct S {
    static func main() {
        print("S.main")
    }
}

// CHECK: S.main
@main
extension S : P {}



