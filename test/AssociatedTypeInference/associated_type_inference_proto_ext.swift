// RUN: %target-typecheck-verify-swift

func assertEqualTypes<T>(_: T.Type, _: T.Type) {}

struct Pair<A, B> {
    var a: A
    var b: B
}

extension Pair: Codable where A: Codable, B: Codable {}

extension Pair: Collection where A == B {
// expected-error@-1 {{conditional conformance of type 'Pair<A, B>' to protocol 'Collection' does not imply conformance to inherited protocol 'Sequence'}}
// expected-note@-2 {{did you mean to explicitly state the conformance with the same bounds using 'where A == B'?}}
// expected-note@-3 {{did you mean to explicitly state the conformance with different bounds?}}
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

// https://github.com/apple/swift/issues/52024
protocol VectorBase : RandomAccessCollection where Index == Int, Element == Elle {
  associatedtype Elle

  var data: Array<Elle> { get set }
  init(data: Array<Elle>)
}

protocol Vector: VectorBase {}
extension Vector where Elle: FloatingPoint {
  // RandomAccessCollection
  var indices: Range<Int> { get {} }
  var startIndex: Int { get {} }
  var endIndex: Int { get {} }

  // MutableCollection
  subscript(i: Index) -> Elle {
    get {} set {}
  }
}

do {
  struct VectorF: Vector {
    var data: Array<Float>
    init(data: Array<Float>) {}
  }

  assertEqualTypes(VectorF.Elle.self, Float.self)
}
