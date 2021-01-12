// RUN: %target-swift-emit-silgen %s -enable-objc-interop | %FileCheck %s

protocol ClassProtocol: AnyObject {}

class BaseClass {}

func makeGenericClosureWithUnknownClass<T>(t: T) where T : ClassProtocol {
  _ = { [unowned t] in _ = t }
}

// CHECK-LABEL: sil private [ossa] @$s4main34makeGenericClosureWithUnknownClass1tyx_tAA0G8ProtocolRzlFyycfU_ : $@convention(thin) <T where T : ClassProtocol> (@guaranteed <τ_0_0 where τ_0_0 : ClassProtocol> { var @sil_unowned τ_0_0 } <T>) -> () {

func makeGenericClosureWithNativeClass1<T>(t: T) where T : BaseClass {
  _ = { [unowned t] in _ = t }
}

// CHECK-LABEL: sil private [ossa] @$s4main34makeGenericClosureWithNativeClass11tyx_tAA9BaseClassCRbzlFyycfU_ : $@convention(thin) <T where T : BaseClass> (@guaranteed @sil_unowned T) -> () {

func makeGenericClosureWithNativeClass2<T>(t: T) where T : ClassProtocol, T : BaseClass {
  _ = { [unowned t] in _ = t }
}

// CHECK-LABEL: sil private [ossa] @$s4main34makeGenericClosureWithNativeClass21tyx_tAA9BaseClassCRbzAA0I8ProtocolRzlFyycfU_ : $@convention(thin) <T where T : BaseClass, T : ClassProtocol> (@guaranteed @sil_unowned T) -> () {