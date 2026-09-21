// RUN: %target-swift-emit-silgen -module-name main %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -module-name main %s > /dev/null

struct Wrapper<T> {}

extension Wrapper where T == Any {
  struct Inner {
    var _flag: Bool
    var flag: Bool {
      @storageRestrictions(initializes: _flag)
      init { _flag = newValue }
      get { _flag }
    }
  }
}
_ = Wrapper<Any>.Inner.init

// CHECK-LABEL: sil hidden [ossa] @$s4main7WrapperVAAypRszlE5InnerV4flagAEyyp_GSb_tcfC : $@convention(method) (Bool, @thin Wrapper<Any>.Inner.Type) -> Wrapper<Any>.Inner
// CHECK: [[REF:%.*]] = function_ref @$s4main7WrapperVAAypRszlE5InnerV4flagSbvi : $@convention(thin) (Bool, @thin Wrapper<Any>.Inner.Type) -> @out Bool
// CHECK: apply [[REF]]({{.*}}) : $@convention(thin) (Bool, @thin Wrapper<Any>.Inner.Type) -> @out Bool

extension Wrapper where T == Int {
  struct InnerInt {
    var _flag: Bool
    var flag: Bool {
      @storageRestrictions(initializes: _flag)
      init { _flag = newValue }
      get { _flag }
    }
  }
}
_ = Wrapper<Int>.InnerInt.init

// CHECK-LABEL: sil hidden [ossa] @$s4main7WrapperVAASiRszlE8InnerIntV4flagAEySi_GSb_tcfC : $@convention(method) (Bool, @thin Wrapper<Int>.InnerInt.Type) -> Wrapper<Int>.InnerInt
// CHECK: [[INT_REF:%.*]] = function_ref @$s4main7WrapperVAASiRszlE8InnerIntV4flagSbvi : $@convention(thin) (Bool, @thin Wrapper<Int>.InnerInt.Type) -> @out Bool
// CHECK: apply [[INT_REF]]({{.*}}) : $@convention(thin) (Bool, @thin Wrapper<Int>.InnerInt.Type) -> @out Bool

// A genuinely generic extension keeps substitutions on the init-accessor apply.
struct Box<T> {}

extension Box {
  struct Inner {
    var _value: T
    var value: T {
      @storageRestrictions(initializes: _value)
      init { _value = newValue }
      get { _value }
    }
  }
}
_ = Box<Int>.Inner.init

// CHECK-LABEL: sil hidden [ossa] @$s4main3BoxV5InnerV5valueAEyx_Gx_tcfC : $@convention(method) <T> (@in T, @thin Box<T>.Inner.Type) -> @out Box<T>.Inner
// CHECK: [[GENERIC_REF:%.*]] = function_ref @$s4main3BoxV5InnerV5valuexvi : $@convention(thin) <τ_0_0> (@in τ_0_0, @thin Box<τ_0_0>.Inner.Type) -> @out τ_0_0
// CHECK: apply [[GENERIC_REF]]<T>({{.*}}) : $@convention(thin) <τ_0_0> (@in τ_0_0, @thin Box<τ_0_0>.Inner.Type) -> @out τ_0_0
