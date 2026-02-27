// RUN: %target-swift-emit-silgen -module-name main -enable-library-evolution %s | %FileCheck %s
// RUN: %target-swift-emit-ir -module-name main -enable-library-evolution %s > /dev/null

public struct R {}

// Make sure we correctly hande resilience for init accessors applied within
// the implicit memberwise initializer.
struct S {
  var x: Int

  private var _y: R?
  private var y: R? {
    @storageRestrictions(initializes: _y)
    init { _y = newValue }
    get { _y }
  }

  private var _z: (Int, R, Int)
  private var z: (Int, R, Int) = (0, R(), 0) {
    @storageRestrictions(initializes: _z)
    init { _z = newValue }
    get { _z }
  }
}
_ = S.init

// CHECK-LABEL: sil hidden [ossa] @$s4main1SV1xACSi_tcfC : $@convention(method) (Int, @thin S.Type) -> @out S
// CHECK: function_ref {{.*}} : $@convention(thin) (@in Optional<R>, @thin S.Type) -> @out Optional<R>
// CHECK: function_ref {{.*}} : $@convention(thin) (Int, @in R, Int, @thin S.Type) -> @out (Int, R, Int)
