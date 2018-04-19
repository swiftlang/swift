// RUN: %target-typecheck-verify-swift

extension Range {
    func f(_ value: Bound) -> Bound {
        return max(lowerBound, min(value, upperBound))
    }
}

protocol ContainsMinMax {}
extension ContainsMinMax {
    func max() {}
    func min() {}
}

func foo(_: Int, _: Int) {}
// expected-note@-1 {{'foo' declared here}}

protocol ContainsFoo {}
extension ContainsFoo {
    func foo() {}
}

struct NonConditional: ContainsMinMax, ContainsFoo {}

extension NonConditional {
    func f() {
        _ = max(1, 2)
        // expected-error@-1{{use of 'max' refers to instance method}}
        // expected-note@-2{{use 'Swift.' to reference the global function}}
        _ = min(3, 4)
        // expected-error@-1{{use of 'min' refers to instance method}}
        // expected-note@-2{{use 'Swift.' to reference the global function}}
        _ = foo(5, 6)
        // expected-error@-1{{use of 'foo' refers to instance method}}
        // expected-note@-2{{use 'name_lookup_min_max_conditional_conformance.' to reference the global function}}
    }
}

struct Conditional<T> {}
extension Conditional: ContainsMinMax where T: ContainsMinMax {}
extension Conditional: ContainsFoo where T: ContainsFoo {}

extension Conditional {
    func f() {
        _ = max(1, 2)
        _ = min(3, 4)
        _ = foo(5, 6)
        // expected-error@-1{{type 'T' does not conform to protocol 'ContainsFoo'}}
    }
}
