// RUN: %target-swift-frontend %s -emit-silgen | %FileCheck %s
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

// CHECK-LABEL: sil private [ossa] @$s18property_delegates9HasNestedV1yACyxGSayxG_
// CHECK: [[METATYPE:%.*]] = metatype $@thin HasNested<T>.PrivateDelegate<Array<T>>.Type
// CHECK: [[STACK_SLOT:%.*]] = alloc_stack $HasNested<T>.PrivateDelegate<Array<T>>
// CHECK: [[ARRAY_STACK_SLOT:%.*]] = alloc_stack $Array<T>
// CHECK: store %0 to [init] [[ARRAY_STACK_SLOT]] : $*Array<T>
// CHECK: [[INIT:%.*]] = function_ref @$s18property_delegates9HasNestedV15PrivateDelegate{{.*}}initialValue
// CHECK: [[DELEGATE_INSTANCE:%.*]] = apply [[INIT]]<T, [T]>([[STACK_SLOT]], [[ARRAY_STACK_SLOT]], [[METATYPE]])
// CHECK: [[DELEGATE_VALUE:%.*]] = load [take] [[STACK_SLOT]] : $*HasNested<T>.PrivateDelegate<Array<T>>
// CHECK: struct $HasNested<T> ([[DELEGATE_VALUE]] : $HasNested<T>.PrivateDelegate<Array<T>>)
struct HasNested<T> {
  @propertyDelegate
  private struct PrivateDelegate<U> {
    var value: U
    init(initialValue: U) {
      self.value = initialValue
    }
  }

  private var y: [T] by PrivateDelegate = []

  static func blah(y: [T]) -> HasNested<T> {
    return HasNested<T>(y: y)
  }
}

// FIXME: For now, we are only checking that we don't crash.
struct HasDefaultInit {
  var x by Wrapper(value: true)
  var y by WrapperWithInitialValue = 25

  static func defaultInit() -> HasDefaultInit {
    return HasDefaultInit()
  }

  static func memberwiseInit(x: Bool, y: Int) -> HasDefaultInit {
    return HasDefaultInit(x: Wrapper(value: x), y: y)
  }
}

struct DelegateWithAccessors {
  var x: Int by Wrapper {
    // CHECK-LABEL: sil hidden [ossa] @$s18property_delegates21DelegateWithAccessorsV1xSivg
    // CHECK-NOT: return
    // CHECK: integer_literal $Builtin.IntLiteral, 42
    return 42

    // Synthesized setter
    // CHECK-LABEL: sil hidden [transparent] [ossa] @$s18property_delegates21DelegateWithAccessorsV1xSivs : $@convention(method) (Int, @inout DelegateWithAccessors) -> ()
    // CHECK-NOT: return
    // CHECK: struct_element_addr {{%.*}} : $*DelegateWithAccessors, #DelegateWithAccessors.$x
  }
  
  var y: Int by WrapperWithInitialValue {
    // Synthesized getter
    // CHECK-LABEL: sil hidden [transparent] [ossa] @$s18property_delegates21DelegateWithAccessorsV1ySivg : $@convention(method) (DelegateWithAccessors) -> Int
    // CHECK-NOT: return
    // CHECK: struct_extract %0 : $DelegateWithAccessors, #DelegateWithAccessors.$y

    // CHECK-LABEL: sil hidden [ossa] @$s18property_delegates21DelegateWithAccessorsV1ySivs : $@convention(method) (Int, @inout DelegateWithAccessors) -> ()
    // CHECK-NOT: struct_extract
    // CHECK: return
    set { }
  }

  mutating func test() {
    x = y
  }
}

func consumeOldValue(_: Int) { }
func consumeNewValue(_: Int) { }

struct DelegateWithDidSetWillSet {
  // CHECK-LABEL: sil hidden [ossa] @$s18property_delegates022DelegateWithDidSetW
  // CHECK: function_ref @$s18property_delegates022DelegateWithDidSetWillF0V1xSivw
  // CHECK: struct_element_addr {{%.*}} : $*DelegateWithDidSetWillSet, #DelegateWithDidSetWillSet.$x
  // CHECK-NEXT: struct_element_addr {{%.*}} : $*Wrapper<Int>, #Wrapper.value
  // CHECK-NEXT: assign %0 to {{%.*}} : $*Int
  // CHECK: function_ref @$s18property_delegates022DelegateWithDidSetWillF0V1xSivW
  var x: Int by Wrapper {
    didSet {
      consumeNewValue(oldValue)
    }

    willSet {
      consumeOldValue(newValue)
    }
  }

  mutating func test(x: Int) {
    self.x = x
  }
}
