// RUN: %target-swift-frontend -emit-sil %s | %FileCheck %s

// CHECK-LABEL: sil hidden [noinline] @_TFC10super_init3FooCfSiS0_ : $@convention(method) (Int, @thick Foo.Type) -> @owned Foo
// CHECK-NOT:     class_method
// CHECK-NOT:     super_method
// CHECK:         [[SUPER_INIT:%.*]] = function_ref @_TFC10super_init3FoocfSiS0_
// CHECK:         [[NEW_SELF:%.*]] = apply [[SUPER_INIT]]

// CHECK-LABEL: sil hidden [noinline] @_TFC10super_init3Barc
// CHECK-NOT:     super_method [[ORIG_SELF]] : $Bar, #Foo.init!initializer.1
// CHECK:         function_ref @_TFC10super_init3FoocfT_S0_

class Foo {
  @inline(never)
  init() {}
  @inline(never)
  init(_ x: Foo) {}
  @inline(never)
  init(_ x: Int) {}
}

class Bar: Foo {
  @inline(never)
  override init() {
    super.init()
  }
}

extension Foo {
  @inline(never)
  convenience init(x: Int) {
    self.init()
  }
}

class Zim: Foo {
  var foo = Foo()
  // CHECK-LABEL: sil hidden @_TFC10super_init3Zimc
  // CHECK-NOT:     super_method {{%[0-9]+}} : $Zim, #Foo.init!initializer.1
  // CHECK:         function_ref @_TFC10super_init3FooCfT_S0_
  // CHECK:         function_ref @_TFC10super_init3FoocfT_S0_
}

class Zang: Foo {
  var foo: Foo

  @inline(never)
  override init() {
    foo = Foo()
    super.init()
  }
  // CHECK-LABEL: sil hidden [noinline] @_TFC10super_init4ZangcfT_S0_
  // CHECK-NOT:         super_method {{%[0-9]+}} : $Zang, #Foo.init!initializer.1
  // CHECK:             function_ref @_TFC10super_init3FooCfT_S0_
  // CHECK:             function_ref @_TFC10super_init3FoocfT_S0_
}

class Bad: Foo {
  // Invalid code, but it's not diagnosed till DI. We at least shouldn't
  // crash on it.
  @inline(never)
  override init() {
    super.init(self)
  }
}

class Good: Foo {
  let x: Int

  // CHECK-LABEL: sil hidden [noinline] @_TFC10super_init4GoodcfT_S0_
  // CHECK-NOT:     super_method {{%[0-9]+}} : $Good, #Foo.init!initializer.1
  // CHECK:         [[SUPER_INIT:%.*]] = function_ref @_TFC10super_init3FoocfSiS0_
  // CHECK:         apply [[SUPER_INIT]]
  @inline(never)
  override init() {
    x = 10
    super.init(x)
  }
}
