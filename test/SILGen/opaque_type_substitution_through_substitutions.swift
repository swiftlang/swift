// RUN: %target-swift-emit-silgen -disable-availability-checking -verify %s

// rdar://problem/65683913

@_silgen_name("foo") func foo() -> Int

func createSomeOpaqueObject() -> some CustomStringConvertible {
    foo()
}

struct TypeWitness<R> {
    init(witness _: R) { }

    var type: R.Type { R.self }
    func getType() -> R.Type { R.self }
}

let w = TypeWitness(witness: createSomeOpaqueObject())
print(w.getType())
