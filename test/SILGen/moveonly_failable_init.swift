// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -module-name test %s | %FileCheck %s --enable-var-scope

struct MoveWithDeinit: ~Copyable {
  deinit { }
}

struct MyType: ~Copyable {
  var handle: MoveWithDeinit

  // CHECK-LABEL: sil hidden @$s4test6MyTypeV6handleACSgAA14MoveWithDeinitVSg_tcfC : $@convention(method) (@owned Optional<MoveWithDeinit>, @thin MyType.Type) -> @owned Optional<MyType>
  // CHECK: bb0(%0 : $Optional<MoveWithDeinit>, %1 : $@thin MyType.Type):
  init?(handle: consuming MoveWithDeinit?) {
    // CHECK: switch_enum [[SUBJECT:%.*]] : $Optional<MoveWithDeinit>, case #Optional.some!enumelt: bb2, case #Optional.none!enumelt: bb1
    guard let handle = consume handle else {
      // CHECK: bb1:
      // CHECK: [[NONE:%.*]] = enum $Optional<MyType>, #Optional.none!enumelt
      // CHECK: br bb3([[NONE]] : $Optional<MyType>)
      return nil
    }

    // CHECK: bb2([[WRAPPED:%.*]] : $MoveWithDeinit):
    // CHECK: br bb3
    self.handle = handle
  }
}

func test() -> MyType? {
  return MyType(handle: MoveWithDeinit())
}
