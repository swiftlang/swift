// {"kind":"emit-silgen","signature":"swift::Lowering::SILGenModule::useConformance(swift::ProtocolConformanceRef)"}
// RUN: not --crash %target-swift-frontend -emit-ir %s
// rdar://problem/65571199
// UNSUPPORTED: asan

// https://github.com/apple/swift/issues/51259

protocol TreeProtocol {

    typealias NodeProtocol = _TreeNodeProtocol
    associatedtype Node : NodeProtocol where Node.Tree == Self
    associatedtype NValuesTraversedBreadthFirst : Sequence = FooVals<Self> where NValuesTraversedBreadthFirst.Iterator.Element == Node.Val
    
    var root: Node? { get }
    
}

protocol _TreeNodeProtocol {

    associatedtype Tree : TreeProtocol where Tree.Node == Self
    associatedtype Val

    var value: Val { get }
    var children: [Tree.Node] { get }

}

struct Foo<V> : TreeProtocol {

    struct Node : _TreeNodeProtocol {
        typealias Tree = Foo
        typealias Val = V

        var value: Val {
            fatalError()
        }
        var children: [Tree.Node] {
            fatalError()
        }
    }
    
    var root: Foo<V>.Node? {
        fatalError()
    }

}

struct FooVals<F : TreeProtocol> : Sequence {

    struct Iterator : IteratorProtocol {
        
        typealias Element = F.Node.Val
        
        mutating func next() -> F.Node.Val? {
            fatalError()
        }
    }
    
    func makeIterator() -> FooVals<F>.Iterator {
        fatalError()
    }

}
