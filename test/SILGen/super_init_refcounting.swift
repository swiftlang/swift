// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

class Foo {
  init() {}
  init(_ x: Foo) {}
  init(_ x: Int) {}
}

class Bar: Foo {
  // CHECK-LABEL: sil hidden @_T022super_init_refcounting3BarC{{[_0-9a-zA-Z]*}}fc
  // CHECK:         [[SELF_VAR:%.*]] = alloc_box ${ var Bar }
  // CHECK:         [[PB:%.*]] = project_box [[SELF_VAR]]
  // CHECK:         [[SELF_MUI:%.*]] =  mark_uninitialized [derivedself] [[PB]]
  // CHECK:         [[ORIG_SELF:%.*]] = load_borrow [[SELF_MUI]]
  // CHECK-NOT:     copy_value [[ORIG_SELF]]
  // CHECK:         [[ORIG_SELF_UP:%.*]] = upcast [[ORIG_SELF]]
  // CHECK-NOT:     copy_value [[ORIG_SELF_UP]]
  // CHECK:         [[SUPER_INIT:%[0-9]+]] = function_ref @_T022super_init_refcounting3FooCACycfc : $@convention(method) (@owned Foo) -> @owned Foo
  // CHECK:         [[NEW_SELF:%.*]] = apply [[SUPER_INIT]]([[ORIG_SELF_UP]])
  // CHECK:         [[NEW_SELF_DOWN:%.*]] = unchecked_ref_cast [[NEW_SELF]]
  // CHECK:         store [[NEW_SELF_DOWN]] to [init] [[SELF_MUI]]
  override init() {
    super.init()
  }
}

extension Foo {
  // CHECK-LABEL: sil hidden @_T022super_init_refcounting3FooC{{[_0-9a-zA-Z]*}}fc
  // CHECK:         [[SELF_VAR:%.*]] = alloc_box ${ var Foo }
  // CHECK:         [[PB:%.*]] = project_box [[SELF_VAR]]
  // CHECK:         [[SELF_MUI:%.*]] =  mark_uninitialized [delegatingself] [[PB]]
  // CHECK:         [[ORIG_SELF:%.*]] = load_borrow [[SELF_MUI]]
  // CHECK-NOT:     copy_value [[ORIG_SELF]]
  // CHECK:         [[SUPER_INIT:%.*]] = class_method
  // CHECK:         [[NEW_SELF:%.*]] = apply [[SUPER_INIT]]([[ORIG_SELF]])
  // CHECK:         store [[NEW_SELF]] to [init] [[SELF_MUI]]
  convenience init(x: Int) {
    self.init()
  }
}

class Zim: Foo {
  var foo = Foo()
  // CHECK-LABEL: sil hidden @_T022super_init_refcounting3ZimC{{[_0-9a-zA-Z]*}}fc
  // CHECK-NOT:     copy_value
  // CHECK-NOT:     destroy_value
  // CHECK:         function_ref @_T022super_init_refcounting3FooCACycfc : $@convention(method) (@owned Foo) -> @owned Foo
}

class Zang: Foo {
  var foo: Foo

  override init() {
    foo = Foo()
    super.init()
  }
  // CHECK-LABEL: sil hidden @_T022super_init_refcounting4ZangC{{[_0-9a-zA-Z]*}}fc
  // CHECK-NOT:     copy_value
  // CHECK-NOT:     destroy_value
  // CHECK:         function_ref @_T022super_init_refcounting3FooCACycfc : $@convention(method) (@owned Foo) -> @owned Foo
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

  // CHECK-LABEL: sil hidden @_T022super_init_refcounting4GoodC{{[_0-9a-zA-Z]*}}fc
  // CHECK:         [[SELF_BOX:%.*]] = alloc_box ${ var Good }
  // CHECK:         [[PB:%.*]] = project_box [[SELF_BOX]]
  // CHECK:         [[SELF:%.*]] = mark_uninitialized [derivedself] [[PB]]
  // CHECK:         store %0 to [init] [[SELF]]
  // CHECK:         [[SELF_OBJ:%.*]] = load_borrow [[SELF]]
  // CHECK:         [[X_ADDR:%.*]] = ref_element_addr [[SELF_OBJ]] : $Good, #Good.x
  // CHECK:         assign {{.*}} to [[X_ADDR]] : $*Int
  // CHECK:         [[SELF_OBJ:%.*]] = load_borrow [[SELF]] : $*Good
  // CHECK:         [[SUPER_OBJ:%.*]] = upcast [[SELF_OBJ]] : $Good to $Foo
  // CHECK:         [[SUPER_INIT:%.*]] = function_ref @_T022super_init_refcounting3FooCACSicfc : $@convention(method) (Int, @owned Foo) -> @owned Foo
  // CHECK:         [[SELF_OBJ:%.*]] = load_borrow [[SELF]]
  // CHECK:         [[X_ADDR:%.*]] = ref_element_addr [[SELF_OBJ]] : $Good, #Good.x
  // CHECK:         [[X:%.*]] = load [trivial] [[X_ADDR]] : $*Int
  // CHECK:         apply [[SUPER_INIT]]([[X]], [[SUPER_OBJ]])
  override init() {
    x = 10
    super.init(x)
  }
}
