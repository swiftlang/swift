// RUN: %target-swift-frontend -emit-ir -verify %s 

protocol BaseProtocol {
    static func run()
}

protocol DerivedProtocol: BaseProtocol {
}

struct Foo: DerivedProtocol {
    static func run() { }
}

@inline(never)
func test() {
    let t: DerivedProtocol.Type = Foo.self
    t.run()
}
