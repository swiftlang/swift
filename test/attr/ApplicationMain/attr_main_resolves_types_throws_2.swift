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
    static func main() throws {
        print("S.main though both throw")
    }
}

// CHECK: S.main though both throw
@main
extension S : P {}


