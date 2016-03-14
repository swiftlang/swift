// RUN: not %target-swift-frontend %s -parse

// Issue found by https://github.com/zneak (zneak)

protocol A { typealias B }
class C : A { typealias B = Int }

func crash<D: C>() -> Bool {
  let a: D.B? = nil
}
