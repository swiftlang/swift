// RUN: %target-swift-frontend %s -emit-silgen -verify-sil-ownership | %FileCheck %s
// FIXME: switch to %target-swift-emit-silgen once we have syntax tree support

@propertyDelegate
struct Wrapper<T> {
  var value: T
}

@propertyDelegate
struct WrapperWithInitialValue<T> {
  var value: T

  init(initialValue: T) {
    self.value = initialValue
  }
}

struct HasMemberwiseInit<T> {
  var x: Bool by Wrapper
  var y: T by WrapperWithInitialValue
}

func forceHasMemberwiseInit() {
  _ = HasMemberwiseInit(x: Wrapper(value: true), y: 17)
}

// CHECK-LABEL: sil hidden [ossa] @$s18property_delegates17HasMemberwiseInitV1x1yACyxGAA7WrapperVySbG_xtcfC : $@convention(method) <T> (Wrapper<Bool>, @in T, @thin HasMemberwiseInit<T>.Type) -> @out HasMemberwiseInit<T>
// CHECK: [[X_STORAGE:%.*]] = struct_element_addr %0 : $*HasMemberwiseInit<T>, #HasMemberwiseInit.$x
// CHECK: store %1 to [trivial] [[X_STORAGE]] : $*Wrapper<Bool> 

// CHECK: [[Y_STORAGE:%.*]] = struct_element_addr %0 : $*HasMemberwiseInit<T>, #HasMemberwiseInit.$y
// CHECK: [[METATYPE:%.*]] = metatype $@thin WrapperWithInitialValue<T>.Type
// CHECK: [[INIT:%.*]] = function_ref @$s18property_delegates23WrapperWithInitialValueV07initialF0ACyxGx_tcfC : $@convention(method) <τ_0_0> (@in τ_0_0, @thin WrapperWithInitialV
// CHECK: apply [[INIT]]<T>([[Y_STORAGE]], %2, [[METATYPE]])
