// RUN: %target-typecheck-verify-swift
enum a<b> {}
struct X {}
extension a where b == X {
    typealias c = d
    // expected-error@-1{{use of undeclared type 'd'}}
    func e<f>() -> c {}
}
