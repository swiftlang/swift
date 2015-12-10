// RUN: %target-parse-verify-swift

// Dictionary types.
class Base {
    func f0(d: Set<String>) { }
    func f1(d: Set<Set<Int>>) { }
}

class Derived : Base {
    override func f0(d: Set<String>) { }
    override func f1(d: Set<Set<Int>>) { }
}

// Dictionary types in generic specializations.
struct X<T> { }

func testGenericSpec() {
    _ = X<Set<Int>>()
}

// Dictionary types for construction.
func constructDictionary(n: Int) {
    var dict = [Int : String](minimumCapacity: n)
    dict[5] = "hello"
}


struct NotHashable { }

var nh1 : Set<NotHashable> // expected-error{{'NotHashable' does not conform to protocol 'Hashable'}}
