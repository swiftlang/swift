// RUN: %target-swift-emit-silgen -enable-sil-ownership %s | %FileCheck %s

// Test for compilation order independence
class C : B {
  // foo inherited from B
  override func bar() {}
  // bas inherited from A
  override func qux() {}

  // zim inherited from B
  override func zang() {}

  required init(int i: Int) { }

  func flopsy() {}
  func mopsy() {}
}
// CHECK: sil_vtable C {
// CHECK:   #A.foo!1: {{.*}} : @$S7vtables1BC3foo{{[_0-9a-zA-Z]*}}F
// CHECK:   #A.bar!1: {{.*}} : @$S7vtables1CC3bar{{[_0-9a-zA-Z]*}}F
// CHECK:   #A.bas!1: {{.*}} : @$S7vtables1AC3bas{{[_0-9a-zA-Z]*}}F
// CHECK:   #A.qux!1: {{.*}} : @$S7vtables1CC3qux{{[_0-9a-zA-Z]*}}F
// CHECK:   #B.init!allocator.1: {{.*}} : @$S7vtables1CC{{[_0-9a-zA-Z]*}}fC
// CHECK:   #B.init!initializer.1: {{.*}} : @$S7vtables1CC{{[_0-9a-zA-Z]*}}fc
// CHECK:   #B.zim!1: {{.*}} : @$S7vtables1BC3zim{{[_0-9a-zA-Z]*}}F
// CHECK:   #B.zang!1: {{.*}} : @$S7vtables1CC4zang{{[_0-9a-zA-Z]*}}F
// CHECK:   #C.flopsy!1: {{.*}} : @$S7vtables1CC6flopsy{{[_0-9a-zA-Z]*}}F
// CHECK:   #C.mopsy!1: {{.*}} : @$S7vtables1CC5mopsy{{[_0-9a-zA-Z]*}}F
// CHECK: }

class A {
  func foo() {}
  func bar() {}
  func bas() {}
  func qux() {}
}

// CHECK: sil_vtable A {
// CHECK:   #A.foo!1: {{.*}} : @$S7vtables1AC3foo{{[_0-9a-zA-Z]*}}F
// CHECK:   #A.bar!1: {{.*}} : @$S7vtables1AC3bar{{[_0-9a-zA-Z]*}}F
// CHECK:   #A.bas!1: {{.*}} : @$S7vtables1AC3bas{{[_0-9a-zA-Z]*}}F
// CHECK:   #A.qux!1: {{.*}} : @$S7vtables1AC3qux{{[_0-9a-zA-Z]*}}F
// CHECK:   #A.init!initializer.1: {{.*}} : @$S7vtables1AC{{[_0-9a-zA-Z]*}}fc
// CHECK: }

class B : A {
  required init(int i: Int) { }

  override func foo() {}
  // bar inherited from A
  // bas inherited from A
  override func qux() {}

  func zim() {}
  func zang() {}
}

// CHECK: sil_vtable B {
// CHECK:   #A.foo!1: {{.*}} : @$S7vtables1BC3foo{{[_0-9a-zA-Z]*}}F
// CHECK:   #A.bar!1: {{.*}} : @$S7vtables1AC3bar{{[_0-9a-zA-Z]*}}F
// CHECK:   #A.bas!1: {{.*}} : @$S7vtables1AC3bas{{[_0-9a-zA-Z]*}}F
// CHECK:   #A.qux!1: {{.*}} : @$S7vtables1BC3qux{{[_0-9a-zA-Z]*}}F
// CHECK:   #B.init!allocator.1: {{.*}} : @$S7vtables1BC{{[_0-9a-zA-Z]*}}fC
// CHECK:   #B.init!initializer.1: {{.*}} : @$S7vtables1BC{{[_0-9a-zA-Z]*}}fc
// CHECK:   #B.zim!1: {{.*}} : @$S7vtables1BC3zim{{[_0-9a-zA-Z]*}}F
// CHECK:   #B.zang!1: {{.*}} : @$S7vtables1BC4zang{{[_0-9a-zA-Z]*}}F
// CHECK: }

// CHECK: sil_vtable RequiredInitDerived {
// CHECK-NEXT: #SimpleInitBase.init!initializer.1: {{.*}} : @$S7vtables19RequiredInitDerivedC{{[_0-9a-zA-Z]*}}fc
// CHECK-NEXT: #RequiredInitDerived.init!allocator.1: {{.*}} : @$S7vtables19RequiredInitDerivedC
// CHECK-NEXT: #RequiredInitDerived.deinit!deallocator.1: @$S7vtables19RequiredInitDerivedCfD
// CHECK-NEXT: }

class SimpleInitBase { }

class RequiredInitDerived : SimpleInitBase {
  required override init() { }
}

class Observed {
  var x: Int = 0 {
    didSet {
    }
    willSet {
    }
  }
}

// rdar://problem/21298214
class BaseWithDefaults {
   func a(_ object: AnyObject? = nil) {}
}

class DerivedWithoutDefaults : BaseWithDefaults {
   override func a(_ object: AnyObject?) { 
     super.a(object)   
   }
}


// CHECK-LABEL: sil_vtable Observed {
// CHECK-NOT:     #Observed.x!didSet
// CHECK-NOT:     #Observed.x!willSet
// CHECK:         #Observed.x!getter
// CHECK:         #Observed.x!setter

// CHECK-LABEL: sil_vtable DerivedWithoutDefaults {
// CHECK:         #BaseWithDefaults.a!1: {{.*}} : @$S7vtables22DerivedWithoutDefaultsC1a{{[_0-9a-zA-Z]*}}F



// Escape identifiers that represent special names

class SubscriptAsFunction {
  func `subscript`() {}
}

// CHECK-LABEL: sil_vtable SubscriptAsFunction {
// CHECK-NOT:     #SubscriptAsFunction.subscript
// CHECK:         #SubscriptAsFunction.`subscript`!1


class DeinitAsFunction {
  func `deinit`() {}
}

// CHECK-LABEL: sil_vtable DeinitAsFunction {
// CHECK:         #DeinitAsFunction.`deinit`!1
// CHECK:         #DeinitAsFunction.deinit!deallocator
