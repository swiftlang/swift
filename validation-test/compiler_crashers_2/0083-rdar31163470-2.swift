// RUN: not --crash %target-swift-frontend -primary-file %s -emit-ir
// REQUIRES: asserts

protocol C {
  associatedtype I
}

protocol PST {
  associatedtype LT : C
}

protocol SL {
  associatedtype S : PST
}

struct PEN<_S : PST> : SL {
  typealias S = _S
  let l: _S.LT.I
}

struct PE<N : SL> {
  let n: N
  static func c<S>(_: PE<N>) where N == PEN<S> {}
}
