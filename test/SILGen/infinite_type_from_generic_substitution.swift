// RUN: %target-swift-emit-sil -sil-verify-all %s -enable-sil-opaque-values -o /dev/null
// RUN: %target-swift-emit-sil -sil-verify-all %s -o /dev/null

protocol P {
    associatedtype A
}

struct S<T: P> {
    var s: T.A
}

struct SR: P {
    typealias A = S<SR>
}

struct SU<T: P> {
    var x: S<T>
}

func foo(x: SU<SR>) -> SU<SR> { return x }
func bar(x: S<SR>) -> S<SR> { return x }
func bas(x: S<SR>) -> S<SR> { return x.s }

enum E<T: P> {
    case recursive(T.A)
    case blah
}

struct ER: P {
    typealias A = E<ER>
}

enum EU<T: P> {
    case x(E<T>)
}

func zim(x: EU<ER>) -> EU<ER> { return x }
func zang(x: E<ER>) -> E<ER> { return x }


struct CCC {
  var members: [Member]
  var isInverted: Bool
}
enum Member {
  case atom(Int)
  case custom(CCC)
  indirect case intersection(CCC, CCC)
}
indirect enum Node {
  case customCharacterClass(CCC)
  case leaf
}
func f(_ n: Node) -> Int {
  switch n {
  case .customCharacterClass: return 1
  case .leaf: return 0
  }
}
