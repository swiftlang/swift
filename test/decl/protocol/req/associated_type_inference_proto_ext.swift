// RUN: %target-typecheck-verify-swift

struct Pair<A, B> {
    var a: A
    var b: B
}

extension Pair: Codable where A: Codable, B: Codable {}

extension Pair: Collection where A == B {
// expected-error@-1 {{conditional conformance of type 'Pair<A, B>' to protocol 'Collection' does not imply conformance to inherited protocol 'Sequence'}}
// expected-note@-2 {{did you mean to explicitly state the conformance like 'extension Pair: Sequence where ...'?}}
    typealias Element = A

    var startIndex: Int { return 0 }
    var endIndex: Int   { return 2 }

    subscript(index: Int) -> Element {
        switch index {
        case 0: return self.a
        case 1: return self.b
        default: fatalError("Index out of bounds.")
        }
    }
    
    func index(after i: Int) -> Int {
        precondition(i < endIndex, "Can't advance beyond endIndex")
        return i + 1
    }
}
