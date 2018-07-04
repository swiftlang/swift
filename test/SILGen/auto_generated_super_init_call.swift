// RUN: %target-swift-emit-silgen -Xllvm -sil-print-debuginfo -enable-sil-ownership %s | %FileCheck %s

// Test that we emit a call to super.init at the end of the initializer, when none has been previously added.

class Parent { 
  init() {}
}

class SomeDerivedClass : Parent {
  var y: Int
  func foo() {}

  override init() {
    y = 42
// CHECK-LABEL: sil hidden @$S30auto_generated_super_init_call16SomeDerivedClassC{{[_0-9a-zA-Z]*}}fc : $@convention(method) (@owned SomeDerivedClass) -> @owned SomeDerivedClass
// CHECK: integer_literal $Builtin.Int2048, 42
// CHECK: [[SELFLOAD:%[0-9]+]] = load [take] [[SELF:%[0-9]+]] : $*SomeDerivedClass
// CHECK-NEXT: [[PARENT:%[0-9]+]] = upcast [[SELFLOAD]] : $SomeDerivedClass to $Parent
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[INITCALL1:%[0-9]+]] = function_ref @$S30auto_generated_super_init_call6ParentCACycfc : $@convention(method) (@owned Parent) -> @owned Parent
// CHECK-NEXT: [[RES1:%[0-9]+]] = apply [[INITCALL1]]([[PARENT]])
// CHECK-NEXT: [[DOWNCAST:%[0-9]+]] = unchecked_ref_cast [[RES1]] : $Parent to $SomeDerivedClass
// CHECK-NEXT: store [[DOWNCAST]] to [init] [[SELF]] : $*SomeDerivedClass 
  }
  
  init(x: Int) {
    y = x
// CHECK-LABEL: sil hidden @$S30auto_generated_super_init_call16SomeDerivedClassC{{[_0-9a-zA-Z]*}}fc : $@convention(method) (Int, @owned SomeDerivedClass) -> @owned SomeDerivedClass
// CHECK: function_ref @$S30auto_generated_super_init_call6ParentCACycfc : $@convention(method) (@owned Parent) -> @owned Parent
  }

  init(b: Bool) {
    if b {
      y = 0
      return
    } else {
      y = 10
    }
    return
// Check that we are emitting the super.init expr into the epilog block.
    
// CHECK-LABEL: sil hidden @$S30auto_generated_super_init_call16SomeDerivedClassC{{[_0-9a-zA-Z]*}}fc : $@convention(method) (Bool, @owned SomeDerivedClass) -> @owned SomeDerivedClass    
// CHECK: bb4:
// SEMANTIC ARC TODO: Another case of needing a mutable load_borrow.
// CHECK-NEXT: [[SELFLOAD:%[0-9]+]] = load [take] [[SELF:%[0-9]+]] : $*SomeDerivedClass
// CHECK-NEXT: [[SELFLOAD_PARENT_CAST:%.*]] = upcast [[SELFLOAD]]
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[PARENT_INIT:%.*]] = function_ref @$S30auto_generated_super_init_call6ParentCACycfc : $@convention(method) (@owned Parent) -> @owned Parent,
// CHECK-NEXT: [[PARENT:%.*]] = apply [[PARENT_INIT]]([[SELFLOAD_PARENT_CAST]])
// CHECK-NEXT: [[SELFAGAIN:%.*]] = unchecked_ref_cast [[PARENT]]
// CHECK-NEXT: store [[SELFAGAIN]] to [init] [[SELF]]
// CHECK-NEXT: [[SELFLOAD:%.*]] = load [copy] [[SELF]]
// CHECK-NEXT: destroy_value
// CHECK-NEXT: return [[SELFLOAD]]
  }
// CHECK: } // end sil function '$S30auto_generated_super_init_call16SomeDerivedClassC{{[_0-9a-zA-Z]*}}fc'

  // One init has a call to super init. Make sure we don't insert more than one.
  init(b: Bool, i: Int) {
    if (b) {
      y = i
    } else {
      y = 0
    }
      
    super.init()
// CHECK-LABEL: sil hidden @$S30auto_generated_super_init_call16SomeDerivedClassC{{[_0-9a-zA-Z]*}}fc : $@convention(method) (Bool, Int, @owned SomeDerivedClass) -> @owned SomeDerivedClass    
// CHECK: function_ref @$S30auto_generated_super_init_call6ParentCACycfc : $@convention(method) (@owned Parent) -> @owned Parent
// CHECK: return
  }
}

// Check that we do call super.init.
class HasNoIVars : Parent {
  override init() {
// CHECK-LABEL: sil hidden @$S30auto_generated_super_init_call10HasNoIVarsC{{[_0-9a-zA-Z]*}}fc : $@convention(method) (@owned HasNoIVars) -> @owned HasNoIVars
// CHECK: function_ref @$S30auto_generated_super_init_call6ParentCACycfc : $@convention(method) (@owned Parent) -> @owned Parent
  }
}

// Check that we don't call super.init.
class ParentLess {
  var y: Int
  init() {
    y = 0
  }
}

class Grandparent {
  init() {}
}
// This should have auto-generated default initializer.
class ParentWithNoExplicitInit : Grandparent {
}
// Check that we add a call to super.init.
class ChildOfParentWithNoExplicitInit : ParentWithNoExplicitInit {
  var y: Int
  override init() {
    y = 10
// CHECK-LABEL: sil hidden @$S30auto_generated_super_init_call31ChildOfParentWithNoExplicitInitC{{[_0-9a-zA-Z]*}}fc
// CHECK: function_ref @$S30auto_generated_super_init_call24ParentWithNoExplicitInitCACycfc : $@convention(method) (@owned ParentWithNoExplicitInit) -> @owned ParentWithNoExplicitInit
  }
}

// This should have auto-generated default initializer.
class ParentWithNoExplicitInit2 : Grandparent {
  var i: Int = 0
}
// Check that we add a call to super.init.
class ChildOfParentWithNoExplicitInit2 : ParentWithNoExplicitInit2 {
  var y: Int
  override init() {
    y = 10
// CHECK-LABEL: sil hidden @$S30auto_generated_super_init_call32ChildOfParentWithNoExplicitInit2C{{[_0-9a-zA-Z]*}}fc
// CHECK: function_ref @$S30auto_generated_super_init_call25ParentWithNoExplicitInit2CACycfc : $@convention(method) (@owned ParentWithNoExplicitInit2) -> @owned ParentWithNoExplicitInit2
  }
}

// Do not insert the call nor warn - the user should call init(5).
class ParentWithNoDefaultInit {
  var i: Int
  init(x: Int) {
    i = x
  }
}
class ChildOfParentWithNoDefaultInit : ParentWithNoDefaultInit {
  var y: Int
  init() {
// CHECK-LABEL: sil hidden @$S30auto_generated_super_init_call30ChildOfParentWithNoDefaultInitC{{[_0-9a-zA-Z]*}}fc : $@convention(method) (@owned ChildOfParentWithNoDefaultInit) -> @owned ChildOfParentWithNoDefaultInit
// CHECK: bb0
// CHECK-NOT: apply
// CHECK: return
  }
}

// <https://bugs.swift.org/browse/SR-5974> - auto-generated super.init()
// delegation to a throwing or failing initializer
class FailingParent {
  init?() {}
}

class ThrowingParent {
  init() throws {}
}

class FailingThrowingParent {
  init?() throws {}
}

class FailingChild : FailingParent {
  override init?() {}
}

class ThrowingChild : ThrowingParent {
  override init() throws {}
}

class FailingThrowingChild : FailingThrowingParent {
  override init?() throws {}
}
