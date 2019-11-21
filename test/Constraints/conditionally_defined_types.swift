// RUN: %target-typecheck-verify-swift

protocol P {}
struct X: P {}
struct Y {}

protocol AssociatedType {
    associatedtype T
}
struct Z1: AssociatedType {
    typealias T = X
}
struct Z2: AssociatedType {
    typealias T = Y
}

struct SameType<T> {}
extension SameType where T == X { // expected-note 13{{requirement specified as 'T' == 'X' [with T = Y]}}
    typealias TypeAlias1 = T
    typealias TypeAlias2 = Y
    typealias TypeAlias3<U> = (T, U) // expected-note {{requirement specified as 'T' == 'X' [with T = Y]}}

    struct Decl1 {}
    enum Decl2 {}
    class Decl3 {}
    struct Decl4<U> {} // expected-note 17 {{requirement specified as 'T' == 'X' [with T = Y]}}
    enum Decl5<U: P> {} // expected-note {{requirement specified as 'T' == 'X' [with T = Y]}}
}

let _ = SameType<X>.TypeAlias1.self
let _ = SameType<X>.TypeAlias2.self
let _ = SameType<X>.TypeAlias3<X>.self
let _ = SameType<X>.Decl1.self
let _ = SameType<X>.Decl2.self
let _ = SameType<X>.Decl3.self
let _ = SameType<X>.Decl4<X>.self
let _ = SameType<X>.Decl5<X>.self

let _ = SameType<Y>.TypeAlias1.self // expected-error {{'SameType<Y>.TypeAlias1' (aka 'X') requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.TypeAlias2.self // expected-error {{'SameType<Y>.TypeAlias2' (aka 'Y') requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.TypeAlias3<X>.self // expected-error {{'SameType<Y>.TypeAlias3' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl1.self // expected-error {{'SameType<Y>.Decl1' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl2.self // expected-error {{'SameType<Y>.Decl2' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl3.self // expected-error {{'SameType<Y>.Decl3' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl4<X>.self // expected-error {{'SameType<Y>.Decl4' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl5<X>.self // expected-error {{'SameType<Y>.Decl5' requires the types 'Y' and 'X' be equivalent}}

extension SameType: AssociatedType where T == X {}
// expected-note@-1 {{requirement specified as 'T' == 'X' [with T = Y]}}

let _ = SameType<X>.T.self
let _ = SameType<Y>.T.self // expected-error {{'SameType<Y>.T' (aka 'X') requires the types 'Y' and 'X' be equivalent}}


struct Conforms<T> {}
extension Conforms where T: P {
    typealias TypeAlias1 = T
    typealias TypeAlias2 = Y
    typealias TypeAlias3<U> = (T, U)

    struct Decl1 {}
    enum Decl2 {}
    class Decl3 {}
    struct Decl4<U> {}
    enum Decl5<U: P> {}
}

let _ = Conforms<X>.TypeAlias1.self
let _ = Conforms<X>.TypeAlias2.self
let _ = Conforms<X>.TypeAlias3<X>.self
let _ = Conforms<X>.Decl1.self
let _ = Conforms<X>.Decl2.self
let _ = Conforms<X>.Decl3.self
let _ = Conforms<X>.Decl4<X>.self
let _ = Conforms<X>.Decl5<X>.self

let _ = Conforms<Y>.TypeAlias1.self // expected-error {{type 'Y' does not conform to protocol 'P'}}
let _ = Conforms<Y>.TypeAlias2.self // expected-error {{type 'Y' does not conform to protocol 'P'}}
let _ = Conforms<Y>.TypeAlias3<X>.self // expected-error {{type 'Y' does not conform to protocol 'P'}}
let _ = Conforms<Y>.Decl1.self // expected-error {{type 'Y' does not conform to protocol 'P'}}
let _ = Conforms<Y>.Decl2.self // expected-error {{type 'Y' does not conform to protocol 'P'}}
let _ = Conforms<Y>.Decl3.self // expected-error {{type 'Y' does not conform to protocol 'P'}}
let _ = Conforms<Y>.Decl4<X>.self // expected-error {{type 'Y' does not conform to protocol 'P'}}
let _ = Conforms<Y>.Decl5<X>.self // expected-error {{type 'Y' does not conform to protocol 'P'}}

extension Conforms: AssociatedType where T: P {}

let _ = Conforms<Y>.T.self // expected-error {{type 'Y' does not conform to protocol 'P'}}

let _ = Conforms<X>.T.self

// Now, even more nesting!

extension SameType.Decl1 {
    typealias TypeAlias1 = T
    typealias TypeAlias2 = Y
    typealias TypeAlias3<U> = (T, U)

    struct Decl1 {}
    enum Decl2 {}
    class Decl3 {}
    struct Decl4<U> {}
    enum Decl5<U: P> {}
}

let _ = SameType<X>.Decl1.TypeAlias1.self
let _ = SameType<X>.Decl1.TypeAlias2.self
let _ = SameType<X>.Decl1.TypeAlias3<X>.self
let _ = SameType<X>.Decl1.Decl1.self
let _ = SameType<X>.Decl1.Decl2.self
let _ = SameType<X>.Decl1.Decl3.self
let _ = SameType<X>.Decl1.Decl4<X>.self
let _ = SameType<X>.Decl1.Decl5<X>.self

let _ = SameType<Y>.Decl1.TypeAlias1.self // expected-error {{'SameType<Y>.Decl1' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl1.TypeAlias2.self // expected-error {{'SameType<Y>.Decl1' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl1.TypeAlias3<X>.self // expected-error {{'SameType<Y>.Decl1' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl1.Decl1.self // expected-error {{'SameType<Y>.Decl1' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl1.Decl2.self // expected-error {{'SameType<Y>.Decl1' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl1.Decl3.self // expected-error {{'SameType<Y>.Decl1' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl1.Decl4<X>.self // expected-error {{'SameType<Y>.Decl1' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl1.Decl5<X>.self // expected-error {{'SameType<Y>.Decl1' requires the types 'Y' and 'X' be equivalent}}

extension SameType.Decl4 where U == X { // expected-note 5 {{requirement specified as 'U' == 'X' [with U = Y]}}
    typealias TypeAlias1 = T
    typealias TypeAlias2 = Y
    typealias TypeAlias3<V> = (T, U, V) // expected-note {{requirement specified as 'U' == 'X' [with U = Y]}}

    struct Decl1 {}
    enum Decl2 {}
    class Decl3 {}
    struct Decl4<V> {} // expected-note {{requirement specified as 'U' == 'X' [with U = Y]}}
    enum Decl5<V: P> {} // expected-note {{requirement specified as 'U' == 'X' [with U = Y]}}
}

// All the combinations

let _ = SameType<X>.Decl4<X>.TypeAlias1.self
let _ = SameType<X>.Decl4<X>.TypeAlias2.self
let _ = SameType<X>.Decl4<X>.TypeAlias3<X>.self
let _ = SameType<X>.Decl4<X>.Decl1.self
let _ = SameType<X>.Decl4<X>.Decl2.self
let _ = SameType<X>.Decl4<X>.Decl3.self
let _ = SameType<X>.Decl4<X>.Decl4<X>.self
let _ = SameType<X>.Decl4<X>.Decl5<X>.self

let _ = SameType<X>.Decl4<Y>.TypeAlias1.self // expected-error {{'SameType<X>.Decl4<Y>.TypeAlias1' (aka 'X') requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<X>.Decl4<Y>.TypeAlias2.self // expected-error {{'SameType<X>.Decl4<Y>.TypeAlias2' (aka 'Y') requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<X>.Decl4<Y>.TypeAlias3<X>.self // expected-error {{'SameType<X>.Decl4<Y>.TypeAlias3' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<X>.Decl4<Y>.Decl1.self // expected-error {{'SameType<X>.Decl4<Y>.Decl1' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<X>.Decl4<Y>.Decl2.self // expected-error {{'SameType<X>.Decl4<Y>.Decl2' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<X>.Decl4<Y>.Decl3.self // expected-error {{'SameType<X>.Decl4<Y>.Decl3' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<X>.Decl4<Y>.Decl4<X>.self // expected-error {{'SameType<X>.Decl4<Y>.Decl4' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<X>.Decl4<Y>.Decl5<X>.self // expected-error {{'SameType<X>.Decl4<Y>.Decl5' requires the types 'Y' and 'X' be equivalent}}

let _ = SameType<Y>.Decl4<X>.TypeAlias1.self // expected-error {{'SameType<Y>.Decl4' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl4<X>.TypeAlias2.self // expected-error {{'SameType<Y>.Decl4' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl4<X>.TypeAlias3<X>.self // expected-error {{'SameType<Y>.Decl4' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl4<X>.Decl1.self // expected-error {{'SameType<Y>.Decl4' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl4<X>.Decl2.self // expected-error {{'SameType<Y>.Decl4' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl4<X>.Decl3.self // expected-error {{'SameType<Y>.Decl4' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl4<X>.Decl4<X>.self // expected-error {{'SameType<Y>.Decl4' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl4<X>.Decl5<X>.self // expected-error {{'SameType<Y>.Decl4' requires the types 'Y' and 'X' be equivalent}}

let _ = SameType<Y>.Decl4<Y>.TypeAlias1.self // expected-error {{'SameType<Y>.Decl4' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl4<Y>.TypeAlias2.self // expected-error {{'SameType<Y>.Decl4' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl4<Y>.TypeAlias3<X>.self // expected-error {{'SameType<Y>.Decl4' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl4<Y>.Decl1.self // expected-error {{'SameType<Y>.Decl4' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl4<Y>.Decl2.self // expected-error {{'SameType<Y>.Decl4' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl4<Y>.Decl3.self // expected-error {{'SameType<Y>.Decl4' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl4<Y>.Decl4<X>.self // expected-error {{'SameType<Y>.Decl4' requires the types 'Y' and 'X' be equivalent}}
let _ = SameType<Y>.Decl4<Y>.Decl5<X>.self // expected-error {{'SameType<Y>.Decl4' requires the types 'Y' and 'X' be equivalent}}

// Finally, extra complicated:
extension Conforms.Decl4 where U: AssociatedType, U.T: P {
    typealias TypeAlias1 = T
    typealias TypeAlias2 = Y
    typealias TypeAlias3<V> = (T, U, V)

    struct Decl1 {}
    enum Decl2 {}
    class Decl3 {}
    struct Decl4<V> {}
    enum Decl5<V: P> {}
}

let _ = Conforms<X>.Decl4<Z1>.TypeAlias1.self
let _ = Conforms<X>.Decl4<Z1>.TypeAlias2.self
let _ = Conforms<X>.Decl4<Z1>.TypeAlias3<X>.self
let _ = Conforms<X>.Decl4<Z1>.Decl1.self
let _ = Conforms<X>.Decl4<Z1>.Decl2.self
let _ = Conforms<X>.Decl4<Z1>.Decl3.self
let _ = Conforms<X>.Decl4<Z1>.Decl4<X>.self
let _ = Conforms<X>.Decl4<Z1>.Decl5<X>.self

// Two different forms of badness, corresponding to the two requirements:

let _ = Conforms<X>.Decl4<Y>.TypeAlias1.self
// expected-error@-1 {{type 'Y' does not conform to protocol 'AssociatedType'}}

let _ = Conforms<X>.Decl4<Y>.TypeAlias2.self
// expected-error@-1 {{type 'Y' does not conform to protocol 'AssociatedType'}}

let _ = Conforms<X>.Decl4<Y>.TypeAlias3<X>.self // expected-error {{type 'Y' does not conform to protocol 'AssociatedType'}}
let _ = Conforms<X>.Decl4<Y>.Decl1.self
// expected-error@-1 {{type 'Y' does not conform to protocol 'AssociatedType'}}

let _ = Conforms<X>.Decl4<Y>.Decl2.self
// expected-error@-1 {{type 'Y' does not conform to protocol 'AssociatedType'}}

let _ = Conforms<X>.Decl4<Y>.Decl3.self
// expected-error@-1 {{type 'Y' does not conform to protocol 'AssociatedType'}}

let _ = Conforms<X>.Decl4<Y>.Decl4<X>.self // expected-error {{type 'Y' does not conform to protocol 'AssociatedType'}}
let _ = Conforms<X>.Decl4<Y>.Decl5<X>.self // expected-error {{type 'Y' does not conform to protocol 'AssociatedType'}}

let _ = Conforms<X>.Decl4<Z2>.TypeAlias1.self // expected-error {{type 'Z2.T' (aka 'Y') does not conform to protocol 'P'}}
let _ = Conforms<X>.Decl4<Z2>.TypeAlias2.self // expected-error {{type 'Z2.T' (aka 'Y') does not conform to protocol 'P'}}
let _ = Conforms<X>.Decl4<Z2>.TypeAlias3<X>.self // expected-error {{type 'Z2.T' (aka 'Y') does not conform to protocol 'P'}}
let _ = Conforms<X>.Decl4<Z2>.Decl1.self // expected-error {{type 'Z2.T' (aka 'Y') does not conform to protocol 'P'}}
let _ = Conforms<X>.Decl4<Z2>.Decl2.self // expected-error {{type 'Z2.T' (aka 'Y') does not conform to protocol 'P'}}
let _ = Conforms<X>.Decl4<Z2>.Decl3.self // expected-error {{type 'Z2.T' (aka 'Y') does not conform to protocol 'P'}}
let _ = Conforms<X>.Decl4<Z2>.Decl4<X>.self // expected-error {{type 'Z2.T' (aka 'Y') does not conform to protocol 'P'}}
let _ = Conforms<X>.Decl4<Z2>.Decl5<X>.self // expected-error {{type 'Z2.T' (aka 'Y') does not conform to protocol 'P'}}
