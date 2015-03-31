// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

func foo() -> String? { return "" }
func bar() -> String? { return "" }

func a(x: String) {}
func b(x: String) {}
func c(x: String) {}

// CHECK-LABEL: sil hidden @_TF16if_while_binding10if_no_else
func if_no_else() {
  // CHECK:   [[FOO:%.*]] = function_ref @_TF16if_while_binding3fooFT_GSqSS_
  // CHECK:   [[OPT_RES:%.*]] = apply [[FOO]]()
  // CHECK:   switch_enum [[OPT_RES]] : $Optional<String>, case #Optional.Some!enumelt.1: [[YES:bb[0-9]+]], default [[CONT:bb[0-9]+]]
  if let x? = foo() {
  // CHECK: [[YES]]([[VAL:%[0-9]+]] : $String):
  // CHECK:   [[A:%.*]] = function_ref @_TF16if_while_binding
  // CHECK:   retain_value [[VAL]]
  // CHECK:   apply [[A]]([[VAL]])
  // CHECK:   release_value [[VAL]]
  // CHECK:   br [[CONT]]
    a(x)
  }
  // CHECK: [[CONT]]:
  // CHECK-NEXT:   tuple ()
}

// CHECK-LABEL: sil hidden @_TF16if_while_binding13if_else_chainFT_T_ : $@thin () -> () {
func if_else_chain() {
  // CHECK:   [[FOO:%.*]] = function_ref @_TF16if_while_binding3foo
  // CHECK:   [[OPT_RES:%.*]] = apply [[FOO]]()
  // CHECK:   switch_enum [[OPT_RES]] : $Optional<String>, case #Optional.Some!enumelt.1: [[YESX:bb[0-9]+]], default [[NOX:bb[0-9]+]]
  if let x? = foo() {
  // CHECK: [[YESX]]([[VAL:%[0-9]+]] : $String):
  // CHECK:   debug_value [[VAL]] : $String  // let x
  // CHECK:   [[A:%.*]] = function_ref @_TF16if_while_binding
  // CHECK:   retain_value [[VAL]]
  // CHECK:   apply [[A]]([[VAL]])
  // CHECK:   release_value [[VAL]]
  // CHECK:   br [[CONT_X:bb[0-9]+]]
    a(x)
  // CHECK: [[NOX]]:
  // CHECK:   switch_enum {{.*}} : $Optional<String>, case #Optional.Some!enumelt.1: [[YESY:bb[0-9]+]], default [[ELSE:bb[0-9]+]]
  } else if var y? = bar() {
  // CHECK: [[YESY]]([[VAL:%[0-9]+]] : $String):
  // CHECK:   alloc_box $String   // var y
  // CHECK:   br [[CONT_Y:bb[0-9]+]]
    b(y)
  } else {
    // CHECK: [[ELSE]]:
    // CHECK: function_ref if_while_binding.c
    c("")
    // CHECK:   br [[CONT_Y]]
  }

  // CHECK: [[CONT_Y]]:
  //   br [[CONT_X]]
  // CHECK: [[CONT_X]]:
}

// CHECK-LABEL: sil hidden @_TF16if_while_binding10while_loopFT_T_ : $@thin () -> () {
func while_loop() {
  // CHECK:   br [[LOOP_ENTRY:bb[0-9]+]]
  // CHECK: [[LOOP_ENTRY]]:
  
  // CHECK:   switch_enum {{.*}} : $Optional<String>, case #Optional.Some!enumelt.1: [[LOOP_BODY:bb[0-9]+]], default [[LOOP_EXIT:bb[0-9]+]]
  while let x? = foo() {
  // CHECK: [[LOOP_BODY]]([[X:%[0-9]+]] : $String):
  // CHECK:   switch_enum {{.*}} : $Optional<String>, case #Optional.Some!enumelt.1: [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
    if let y? = bar() {
  // CHECK: [[YES]]([[Y:%[0-9]+]] : $String):
      a(y)
      break
      // CHECK: release_value [[Y]]
      // CHECK: release_value [[X]]
      // CHECK:     br [[LOOP_EXIT]]
    }
  // CHECK: [[NO]]:
  // CHECK:   release_value [[X]]
  // CHECK:   br [[LOOP_ENTRY]]
  }
  // CHECK: [[LOOP_EXIT]]:
  // CHECK-NEXT:   tuple ()
  // CHECK-NEXT:   return
}

// Don't leak alloc_stacks for address-only conditional bindings in 'while'.
// <rdar://problem/16202294>
// CHECK-LABEL: sil hidden @_TF16if_while_binding18while_loop_generic
// CHECK:         br [[COND:bb[0-9]+]]
// CHECK:       [[COND]]:
// CHECK:         [[OPTBUF:%[0-9]+]] = alloc_stack $Optional<T>
// CHECK:         switch_enum_addr {{.*}}, case #Optional.Some!enumelt.1: [[LOOPBODY:bb.*]], default [[OUT:bb[0-9]+]]
// CHECK:       [[LOOPBODY]]:
// CHECK:         [[ENUMVAL:%.*]] = unchecked_take_enum_data_addr
// CHECK:         [[X:%.*]] = alloc_stack $T
// CHECK:         copy_addr [take] [[ENUMVAL]] to [initialization] %8#1
// CHECK:         destroy_addr [[X]]
// CHECK:         dealloc_stack [[X]]
// CHECK:         br [[COND]]
// CHECK:       [[OUT]]:
// CHECK:         dealloc_stack [[OPTBUF]]#0
// CHECK:         br [[DONE:bb[0-9]+]]
// CHECK:       [[DONE]]:
// CHECK:         strong_release %0
func while_loop_generic<T>(source: () -> T?) {
  while let x? = source() {
  }
}

// <rdar://problem/19382942> Improve 'if let' to avoid optional pyramid of doom
// CHECK-LABEL: sil hidden @_TF16if_while_binding16while_loop_multiFT_T_
func while_loop_multi() {
  // CHECK:   br [[LOOP_ENTRY:bb[0-9]+]]
  // CHECK: [[LOOP_ENTRY]]:
  // CHECK:         switch_enum {{.*}}, case #Optional.Some!enumelt.1: [[CHECKBUF2:bb.*]], default [[LOOP_EXIT0:bb[0-9]+]]

  // CHECK: [[CHECKBUF2]]([[A:%[0-9]+]] : $String):
  // CHECK:   debug_value [[A]] : $String  // let a

  // CHECK:   switch_enum {{.*}}, case #Optional.Some!enumelt.1: [[LOOP_BODY:bb.*]], default [[LOOP_EXIT2a:bb[0-9]+]]

  // CHECK: [[LOOP_BODY]]([[B:%[0-9]+]] : $String):
  while let a? = foo(), b? = bar() {
    // CHECK:   debug_value [[B]] : $String  // let b
    // CHECK:   debug_value [[A]] : $String  // let c
    // CHECK:   release_value [[B]]
    // CHECK:   release_value [[A]]
    // CHECK:   br [[LOOP_ENTRY]]
    let c = a
  }
  // CHECK: [[LOOP_EXIT2a]]:
  // CHECK: release_value [[A]]
  // CHECK: br [[LOOP_EXIT0]]

  // CHECK: [[LOOP_EXIT0]]:
  // CHECK-NEXT:   tuple ()
  // CHECK-NEXT:   return
}

// CHECK-LABEL: sil hidden @_TF16if_while_binding8if_multiFT_T_
func if_multi() {
  // CHECK:   switch_enum {{.*}}, case #Optional.Some!enumelt.1: [[CHECKBUF2:bb.*]], default [[IF_DONE:bb[0-9]+]]

  // CHECK: [[CHECKBUF2]]([[A:%[0-9]+]] : $String):
  // CHECK:   debug_value [[A]] : $String  // let a
  // CHECK:   switch_enum {{.*}}, case #Optional.Some!enumelt.1: [[IF_BODY:bb.*]], default [[IF_EXIT1a:bb[0-9]+]]

  // CHECK: [[IF_BODY]]([[B:%[0-9]+]] : $String):
  if let a? = foo(), var b? = bar() {
    // CHECK:   alloc_box $String // var b
    // CHECK:   debug_value {{.*}} : $String  // let c
    // CHECK:   strong_release
    // CHECK:   release_value [[A]]
    // CHECK:   br [[IF_DONE]]
    let c = a
  }
  // CHECK: [[IF_EXIT1a]]:
  // CHECK:   release_value [[A]]
  // CHECK:   br [[IF_DONE]]
  
  // CHECK: [[IF_DONE]]:
  // CHECK-NEXT:   tuple ()
  // CHECK-NEXT:   return
}

// CHECK-LABEL: sil hidden @_TF16if_while_binding13if_multi_elseFT_T_
func if_multi_else() {
  // CHECK:   switch_enum {{.*}}, case #Optional.Some!enumelt.1: [[CHECKBUF2:bb.*]], default [[ELSE:bb[0-9]+]]
  // CHECK: [[CHECKBUF2]]([[A:%[0-9]+]] : $String):
  // CHECK:   debug_value [[A]] : $String  // let a
  // CHECK:   switch_enum {{.*}}, case #Optional.Some!enumelt.1: [[IF_BODY:bb.*]], default [[IF_EXIT1a:bb[0-9]+]]
  
  // CHECK: [[IF_BODY]]([[B:%[0-9]+]] : $String):
  if let a? = foo(), var b? = bar() {
    // CHECK:   alloc_box $String // var b
    // CHECK:   debug_value {{.*}} : $String  // let c
    // CHECK:   strong_release
    // CHECK:   release_value [[A]]
    // CHECK:   br [[IF_DONE:bb[0-9]+]]
    let c = a
  } else {
    let d = 0
    // CHECK: [[IF_EXIT1a]]:
    // CHECK:   release_value [[A]]
    // CHECK:   br [[ELSE]]
    // CHECK: [[ELSE]]:
    // CHECK:   debug_value {{.*}} : $Int  // let d
    // CHECK:   br [[IF_DONE]]
 }
  // CHECK: [[IF_DONE]]:
  // CHECK-NEXT:   tuple ()
  // CHECK-NEXT:   return
}

// CHECK-LABEL: sil hidden @_TF16if_while_binding14if_multi_whereFT_T_
func if_multi_where() {
  // CHECK:   switch_enum {{.*}}, case #Optional.Some!enumelt.1: [[CHECKBUF2:bb.*]], default [[ELSE:bb[0-9]+]]
  // CHECK: [[CHECKBUF2]]([[A:%[0-9]+]] : $String):
  // CHECK:   debug_value [[A]] : $String  // let a
  // CHECK:   switch_enum {{.*}}, case #Optional.Some!enumelt.1: [[CHECK_WHERE:bb.*]], default [[IF_EXIT1a:bb[0-9]+]]

  // CHECK: [[CHECK_WHERE]]([[B:%[0-9]+]] : $String):
  // CHECK:   [[BBOX:%[0-9]+]] = alloc_box $String // var b
  // CHECK:   function_ref Swift.Bool._getBuiltinLogicValue (Swift.Bool)() -> Builtin.Int1
  // CHECK:   cond_br {{.*}}, [[IF_BODY:bb[0-9]+]], [[IF_EXIT3:bb[0-9]+]]
  // CHECK: [[IF_EXIT3]]:
  // CHECK:   strong_release [[BBOX]]#0
  // CHECK:   release_value [[A]]
  // CHECK:   br [[IF_DONE:bb[0-9]+]]
  if let a? = foo(), var b? = bar() where a == b {
    // CHECK: [[IF_BODY]]:
    // CHECK:   debug_value [[CVAL:%[0-9]+]] : $String  // let c
    // CHECK:   strong_release [[BBOX]]#0
    // CHECK:   release_value [[A]]
    // CHECK:   br [[IF_DONE]]
    let c = a
  }
  // CHECK: [[IF_EXIT1a]]:
  // CHECK:   release_value [[A]]
  // CHECK:   br [[IF_DONE]]
  // CHECK: [[IF_DONE]]:
  // CHECK-NEXT:   tuple ()
  // CHECK-NEXT:   return
}


// <rdar://problem/19797158> Swift 1.2's "if" has 2 behaviours. They could be unified.
// CHECK-LABEL: sil hidden @_TF16if_while_binding18if_leading_booleanFSiT_
func if_leading_boolean(a : Int) {
  // Test the boolean condition.
  
  // CHECK: debug_value %0 : $Int  // let a
  // CHECK: [[EQRESULT:%[0-9]+]] = apply {{.*}}(%0, %0) : $@thin (Int, Int) -> Bool

  // CHECK-NEXT: [[EQRESULTI1:%[0-9]+]] = apply %2([[EQRESULT]]) : $@cc(method) @thin (Bool) -> Builtin.Int1
  // CHECK-NEXT: cond_br [[EQRESULTI1]], [[CHECKFOO:bb[0-9]+]], [[IFDONE:bb[0-9]+]]

  // Call Foo and test for the optional being present.
// CHECK: [[CHECKFOO]]:
  // CHECK: [[OPTRESULT:%[0-9]+]] = apply {{.*}}() : $@thin () -> @owned Optional<String>
  
  // CHECK:   switch_enum [[OPTRESULT]] : $Optional<String>, case #Optional.Some!enumelt.1: [[SUCCESS:bb.*]], default [[IF_DONE:bb[0-9]+]]

// CHECK: [[SUCCESS]]([[B:%[0-9]+]] : $String):
  // CHECK-NEXT:   debug_value [[B:%[0-9]+]] : $String  // let b
  // CHECK-NEXT:   debug_value [[B:%[0-9]+]] : $String  // let c
  // CHECK-NEXT:   release_value [[B]]
  // CHECK-NEXT:   br [[IFDONE]]
  if a == a, let b? = foo() {
    let c = b
  }
  // CHECK: [[IFDONE]]:
  // CHECK-NEXT: tuple ()

}


/// <rdar://problem/20364869> Assertion failure when using 'as' pattern in 'if let'
class BaseClass {}
class DerivedClass : BaseClass {}

// CHECK-LABEL: sil hidden @_TF16if_while_binding20testAsPatternInIfLetFGSqCS_9BaseClass_T_
func testAsPatternInIfLet(a : BaseClass?) {
  // CHECK-NEXT: bb0(%0 : $Optional<BaseClass>):
  // CHECK-NEXT:   debug_value %0 : $Optional<BaseClass>  // let a
  // CHECK-NEXT:   retain_value %0 : $Optional<BaseClass>
  // CHECK-NEXT:   switch_enum %0 : $Optional<BaseClass>, case #Optional.Some!enumelt.1: bb1,
  // CHECK:      bb1(%4 : $BaseClass):
  // CHECK-NEXT:   checked_cast_br %4 : $BaseClass to $DerivedClass, bb2,

  // CHECK:      bb2(%6 : $DerivedClass):
  // CHECK-NEXT:   debug_value %6 : $DerivedClass
  // CHECK-NEXT:   strong_release %6 : $DerivedClass
  if let b as DerivedClass = a {

  }
}


