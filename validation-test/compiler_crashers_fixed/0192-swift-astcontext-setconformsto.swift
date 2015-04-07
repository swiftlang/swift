// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

protocol A {
}
struct B : A {
}
struct C<D, E: A where D.C == EType, T where Optional<T> == S.Generator.Element>(xs : S) -> T? {
    for (mx : T?) in xs {
        if let x? = mx {
            return x
        }
    }
    return nil
}
let xs : [Int?] = [nil, 4, nil]
println(some(xs))
protocol a : a {
}
protocol a {
  typealias d
  typealia
    typealias g
}
f: A where D.C == E> {s func c() { }
}
(b() as a).dynamicType.c()
func c<d {
    enumBooleanType)
class a {
    typealias b = b
}
func a<T>() -> A {
    typeal= D>(e: A.B) {
    }
}
var() -> ())] = []
}
