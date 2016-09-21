// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

class Foo {
  init() {}
  init(_ x: Foo) {}
  init(_ x: Int) {}
}

class Bar: Foo {
  // CHECK-LABEL: sil hidden @_TFC22super_init_refcounting3Barc
  // CHECK:         [[SELF_VAR:%.*]] = alloc_box $Bar
  // CHECK:         [[PB:%.*]] = project_box [[SELF_VAR]]
  // CHECK:         [[SELF_MUI:%.*]] =  mark_uninitialized [derivedself] [[PB]]
  // CHECK:         [[ORIG_SELF:%.*]] = load [[SELF_MUI]]
  // CHECK-NOT:     strong_retain [[ORIG_SELF]]
  // CHECK:         [[ORIG_SELF_UP:%.*]] = upcast [[ORIG_SELF]]
  // CHECK-NOT:     strong_retain [[ORIG_SELF_UP]]
  // CHECK:         [[SUPER_INIT:%[0-9]+]] = function_ref @_TFC22super_init_refcounting3FoocfT_S0_ : $@convention(method) (@owned Foo) -> @owned Foo
  // CHECK:         [[NEW_SELF:%.*]] = apply [[SUPER_INIT]]([[ORIG_SELF_UP]])
  // CHECK:         [[NEW_SELF_DOWN:%.*]] = unchecked_ref_cast [[NEW_SELF]]
  // CHECK:         store [[NEW_SELF_DOWN]] to [[SELF_MUI]]
  override init() {
    super.init()
  }
}

extension Foo {
  // CHECK-LABEL: sil hidden @_TFC22super_init_refcounting3Fooc
  // CHECK:         [[SELF_VAR:%.*]] = alloc_box $Foo
  // CHECK:         [[PB:%.*]] = project_box [[SELF_VAR]]
  // CHECK:         [[SELF_MUI:%.*]] =  mark_uninitialized [delegatingself] [[PB]]
  // CHECK:         [[ORIG_SELF:%.*]] = load [[SELF_MUI]]
  // CHECK-NOT:     strong_retain [[ORIG_SELF]]
  // CHECK:         [[SUPER_INIT:%.*]] = class_method
  // CHECK:         [[NEW_SELF:%.*]] = apply [[SUPER_INIT]]([[ORIG_SELF]])
  // CHECK:         store [[NEW_SELF]] to [[SELF_MUI]]
  convenience init(x: Int) {
    self.init()
  }
}

class Zim: Foo {
  var foo = Foo()
  // CHECK-LABEL: sil hidden @_TFC22super_init_refcounting3Zimc
  // CHECK-NOT:     strong_retain
  // CHECK-NOT:     strong_release
  // CHECK:         function_ref @_TFC22super_init_refcounting3FoocfT_S0_ : $@convention(method) (@owned Foo) -> @owned Foo
}

class Zang: Foo {
  var foo: Foo

  override init() {
    foo = Foo()
    super.init()
  }
  // CHECK-LABEL: sil hidden @_TFC22super_init_refcounting4Zangc
  // CHECK-NOT:     strong_retain
  // CHECK-NOT:     strong_release
  // CHECK:         function_ref @_TFC22super_init_refcounting3FoocfT_S0_ : $@convention(method) (@owned Foo) -> @owned Foo
}

class Bad: Foo {
  // Invalid code, but it's not diagnosed till DI. We at least shouldn't
  // crash on it.
  override init() {
    super.init(self)
  }
}

class Good: Foo {
  let x: Int

  // CHECK-LABEL: sil hidden @_TFC22super_init_refcounting4Goodc
  // CHECK:         [[SELF_BOX:%.*]] = alloc_box $Good
  // CHECK:         [[PB:%.*]] = project_box [[SELF_BOX]]
  // CHECK:         [[SELF:%.*]] = mark_uninitialized [derivedself] [[PB]]
  // CHECK:         store %0 to [[SELF]]
  // CHECK:         [[SELF_OBJ:%.*]] = load [[SELF]]
  // CHECK:         [[X_ADDR:%.*]] = ref_element_addr [[SELF_OBJ]] : $Good, #Good.x
  // CHECK:         assign {{.*}} to [[X_ADDR]] : $*Int
  // CHECK:         [[SELF_OBJ:%.*]] = load [[SELF]] : $*Good
  // CHECK:         [[SUPER_OBJ:%.*]] = upcast [[SELF_OBJ]] : $Good to $Foo
  // CHECK:         [[SUPER_INIT:%.*]] = function_ref @_TFC22super_init_refcounting3FoocfSiS0_ : $@convention(method) (Int, @owned Foo) -> @owned Foo
  // CHECK:         [[SELF_OBJ:%.*]] = load [[SELF]]
  // CHECK:         [[X_ADDR:%.*]] = ref_element_addr [[SELF_OBJ]] : $Good, #Good.x
  // CHECK:         [[X:%.*]] = load [[X_ADDR]] : $*Int
  // CHECK:         apply [[SUPER_INIT]]([[X]], [[SUPER_OBJ]])
  override init() {
    x = 10
    super.init(x)
  }
}
