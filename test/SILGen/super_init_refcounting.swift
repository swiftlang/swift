// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

class Foo {
  init() {}
  init(_ x: Foo) {}
  init(_ x: Int) {}
}

class Bar: Foo {
  // CHECK-LABEL: sil hidden @$S22super_init_refcounting3BarC{{[_0-9a-zA-Z]*}}fc
  // CHECK: bb0([[INPUT_SELF:%.*]] : $Bar):
  // CHECK:         [[SELF_BOX:%.*]] = alloc_box ${ var Bar }
  // CHECK:         [[MARKED_SELF_BOX:%.*]] =  mark_uninitialized [derivedself] [[SELF_BOX]]
  // CHECK:         [[PB_SELF_BOX:%.*]] = project_box [[MARKED_SELF_BOX]]
  // CHECK:         store [[INPUT_SELF]] to [init] [[PB_SELF_BOX]]
  // CHECK:         [[ORIG_SELF:%.*]] = load [take] [[PB_SELF_BOX]]
  // CHECK-NOT:     copy_value [[ORIG_SELF]]
  // CHECK:         [[ORIG_SELF_UP:%.*]] = upcast [[ORIG_SELF]]
  // CHECK-NOT:     copy_value [[ORIG_SELF_UP]]
  // CHECK:         [[SUPER_INIT:%[0-9]+]] = function_ref @$S22super_init_refcounting3FooCACycfc : $@convention(method) (@owned Foo) -> @owned Foo
  // CHECK:         [[NEW_SELF:%.*]] = apply [[SUPER_INIT]]([[ORIG_SELF_UP]])
  // CHECK:         [[NEW_SELF_DOWN:%.*]] = unchecked_ref_cast [[NEW_SELF]]
  // CHECK:         store [[NEW_SELF_DOWN]] to [init] [[PB_SELF_BOX]]
  override init() {
    super.init()
  }
}

extension Foo {
  // CHECK-LABEL: sil hidden @$S22super_init_refcounting3FooC{{[_0-9a-zA-Z]*}}fc
  // CHECK:         [[SELF_BOX:%.*]] = alloc_box ${ var Foo }
  // CHECK:         [[MARKED_SELF_BOX:%.*]] =  mark_uninitialized [delegatingself] [[SELF_BOX]]
  // CHECK:         [[PB_SELF_BOX:%.*]] = project_box [[MARKED_SELF_BOX]]
  // CHECK:         [[ORIG_SELF:%.*]] = load [take] [[PB_SELF_BOX]]
  // CHECK-NOT:     copy_value [[ORIG_SELF]]
  // CHECK:         [[SUPER_INIT:%.*]] = class_method
  // CHECK:         [[NEW_SELF:%.*]] = apply [[SUPER_INIT]]([[ORIG_SELF]])
  // CHECK:         store [[NEW_SELF]] to [init] [[PB_SELF_BOX]]
  convenience init(x: Int) {
    self.init()
  }
}

class Zim: Foo {
  var foo = Foo()
  // CHECK-LABEL: sil hidden @$S22super_init_refcounting3ZimC{{[_0-9a-zA-Z]*}}fc
  // CHECK-NOT:     copy_value
  // CHECK-NOT:     destroy_value
  // CHECK:         function_ref @$S22super_init_refcounting3FooCACycfc : $@convention(method) (@owned Foo) -> @owned Foo
}

class Zang: Foo {
  var foo: Foo

  override init() {
    foo = Foo()
    super.init()
  }
  // CHECK-LABEL: sil hidden @$S22super_init_refcounting4ZangC{{[_0-9a-zA-Z]*}}fc
  // CHECK-NOT:     copy_value
  // CHECK-NOT:     destroy_value
  // CHECK:         function_ref @$S22super_init_refcounting3FooCACycfc : $@convention(method) (@owned Foo) -> @owned Foo
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

  // CHECK-LABEL: sil hidden @$S22super_init_refcounting4GoodC{{[_0-9a-zA-Z]*}}fc
  // CHECK:         [[SELF_BOX:%.*]] = alloc_box ${ var Good }
  // CHECK:         [[MARKED_SELF_BOX:%.*]] = mark_uninitialized [derivedself] [[SELF_BOX]]
  // CHECK:         [[PB_SELF_BOX:%.*]] = project_box [[MARKED_SELF_BOX]]
  // CHECK:         store %0 to [init] [[PB_SELF_BOX]]
  // CHECK:         [[SELF_OBJ:%.*]] = load_borrow [[PB_SELF_BOX]]
  // CHECK:         [[X_ADDR:%.*]] = ref_element_addr [[SELF_OBJ]] : $Good, #Good.x
  // CHECK:         assign {{.*}} to [[X_ADDR]] : $*Int
  // CHECK:         [[SELF_OBJ:%.*]] = load [take] [[PB_SELF_BOX]] : $*Good
  // CHECK:         [[SUPER_OBJ:%.*]] = upcast [[SELF_OBJ]] : $Good to $Foo
  // CHECK:         [[BORROWED_SUPER:%.*]] = begin_borrow [[SUPER_OBJ]]
  // CHECK:         [[DOWNCAST_BORROWED_SUPER:%.*]] = unchecked_ref_cast [[BORROWED_SUPER]] : $Foo to $Good
  // CHECK:         [[X_ADDR:%.*]] = ref_element_addr [[DOWNCAST_BORROWED_SUPER]] : $Good, #Good.x
  // CHECK:         [[X:%.*]] = load [trivial] [[X_ADDR]] : $*Int
  // CHECK:         end_borrow [[BORROWED_SUPER]] from [[SUPER_OBJ]]
  // CHECK:         [[SUPER_INIT:%.*]] = function_ref @$S22super_init_refcounting3FooCyACSicfc : $@convention(method) (Int, @owned Foo) -> @owned Foo
  // CHECK:         apply [[SUPER_INIT]]([[X]], [[SUPER_OBJ]])
  override init() {
    x = 10
    super.init(x)
  }
}
