// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -primary-file %s -emit-ir

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
  // CHECK-LABEL: .c@
  // CHECK-NEXT: Generic signature: <N, S where N == PEN<S>, S : PST>
  static func c<S>(_: PE<N>) where N == PEN<S> {}
}
