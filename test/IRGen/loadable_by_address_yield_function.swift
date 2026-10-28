// RUN: %target-swift-frontend -Onone -emit-ir -sil-verify-all %s -Xllvm -sil-print-after=loadable-address -module-name main 2>&1 | %FileCheck %s

// https://github.com/swiftlang/swift/issues/89548
// LoadableByAddress rewrote the type of a yielded function value whose
// parameter is large, then inserted a `load` from the yielded value as if the
// yield had become indirect. The yield is still a direct value, so IRGen
// crashed emitting the `load`.

public struct Big { var a = 0, b = 0, c = 0, d = 0, e = 0 }

struct Box {
  private var _storage: (Big) -> Bool = { _ in true }
  var cb: (Big) -> Bool {
    _read { yield _storage }
  }
}

// CHECK-LABEL: sil hidden [transparent] @$s4main3BoxV2cbySbAA3BigVcvg : $@convention(method) (@guaranteed Box) -> @owned @callee_guaranteed (@in_guaranteed Big) -> Bool {
// CHECK:         ([[FN:%.*]], [[TOKEN:%.*]]) = begin_apply {{%.*}}(%0) : $@yield_once @convention(method) (@guaranteed Box) -> @yields @guaranteed @callee_guaranteed (@in_guaranteed Big) -> Bool
// CHECK-NOT:     load
// CHECK:         end_apply [[TOKEN]]
// CHECK:         return [[FN]]
// CHECK:       } // end sil function '$s4main3BoxV2cbySbAA3BigVcvg'
