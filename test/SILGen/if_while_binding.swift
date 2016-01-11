// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

func foo() -> String? { return "" }
func bar() -> String? { return "" }

func a(x: String) {}
func b(x: String) {}
func c(x: String) {}

func marker_1() {}
func marker_2() {}
func marker_3() {}


// CHECK-LABEL: sil hidden @_TF16if_while_binding10if_no_else
func if_no_else() {
  // CHECK:   [[FOO:%.*]] = function_ref @_TF16if_while_binding3fooFT_GSqSS_
  // CHECK:   [[OPT_RES:%.*]] = apply [[FOO]]()
  // CHECK:   switch_enum [[OPT_RES]] : $Optional<String>, case #Optional.Some!enumelt.1: [[YES:bb[0-9]+]], default [[CONT:bb[0-9]+]]
  if let x = foo() {
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

// CHECK-LABEL: sil hidden @_TF16if_while_binding13if_else_chainFT_T_ : $@convention(thin) () -> () {
func if_else_chain() {
  // CHECK:   [[FOO:%.*]] = function_ref @_TF16if_while_binding3foo
  // CHECK:   [[XVAL:%.*]] = apply [[FOO]]()
  // CHECK:   switch_enum [[XVAL]] : $Optional<String>, case #Optional.Some!enumelt.1: [[YESX:bb[0-9]+]], default [[NOX:bb[0-9]+]]
  if let x = foo() {
  // CHECK: [[YESX]]([[XVAL:%[0-9]+]] : $String):
  // CHECK:   debug_value [[XVAL]] : $String, let, name "x"
  // CHECK:   [[A:%.*]] = function_ref @_TF16if_while_binding1aFSST_
  // CHECK:   retain_value [[XVAL]]
  // CHECK:   apply [[A]]([[XVAL]])
  // CHECK:   release_value [[XVAL]]
  // CHECK:   br [[CONT_X:bb[0-9]+]]
    a(x)
  // CHECK: [[NOX]]:
  // CHECK:   [[BAR:%.*]] = function_ref  @_TF16if_while_binding3barFT_GSqSS_
  // CHECK:   [[YVAL:%[0-9]+]] = apply [[BAR]]()
  // CHECK:   switch_enum [[YVAL]] : $Optional<String>, case #Optional.Some!enumelt.1: [[YESY:bb[0-9]+]], default [[NOY:bb[0-9]+]]
  } else if let y = bar() {
  // CHECK: [[YESY]]([[YVAL:%[0-9]+]] : $String):
  // CHECK:   debug_value [[YVAL]] : $String
  // CHECK:   [[B:%[0-9]+]] = function_ref @_TF16if_while_binding1bFSST_
  // CHECK:   retain_value [[YVAL]]
  // CHECK:   [[BRET:%[0-9]+]] = apply [[B]]([[YVAL]])
  // CHECK:   release_value [[YVAL]] : $String
  // CHECK:   br [[CONT_Y:bb[0-9]+]]
    b(y)
  } else {
  // CHECK: [[NOY]]:
    // CHECK: [[C:%[0-9]+]] = function_ref @_TF16if_while_binding1cFSST_
    c("")
    // CHECK:   br [[CONT_Y:bb[0-9]+]]
  }

  // CHECK: [[CONT_Y]]:
  //   br [[CONT_X]]
  // CHECK: [[CONT_X]]:
  // CHECK:  tuple ()
  // CHECK:  return
}

// CHECK-LABEL: sil hidden @_TF16if_while_binding10while_loopFT_T_ : $@convention(thin) () -> () {
func while_loop() {
  // CHECK:   br [[LOOP_ENTRY:bb[0-9]+]]
  // CHECK: [[LOOP_ENTRY]]:
  
  // CHECK:   switch_enum {{.*}} : $Optional<String>, case #Optional.Some!enumelt.1: [[LOOP_BODY:bb[0-9]+]], default [[LOOP_EXIT:bb[0-9]+]]
  while let x = foo() {
  // CHECK: [[LOOP_BODY]]([[X:%[0-9]+]] : $String):
  // CHECK:   switch_enum {{.*}} : $Optional<String>, case #Optional.Some!enumelt.1: [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
    if let y = bar() {
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
// CHECK:         [[X:%.*]] = alloc_stack $T, let, name "x"
// CHECK:         [[OPTBUF:%[0-9]+]] = alloc_stack $Optional<T>
// CHECK:         switch_enum_addr {{.*}}, case #Optional.Some!enumelt.1: [[LOOPBODY:bb.*]], default [[OUT:bb[0-9]+]]
// CHECK:       [[OUT]]:
// CHECK:         dealloc_stack [[OPTBUF]]
// CHECK:         dealloc_stack [[X]]
// CHECK:         br [[DONE:bb[0-9]+]]
// CHECK:       [[LOOPBODY]]:
// CHECK:         [[ENUMVAL:%.*]] = unchecked_take_enum_data_addr
// CHECK:         copy_addr [take] [[ENUMVAL]] to [initialization] [[X]]
// CHECK:         destroy_addr [[X]]
// CHECK:         dealloc_stack [[X]]
// CHECK:         br [[COND]]
// CHECK:       [[DONE]]:
// CHECK:         strong_release %0
func while_loop_generic<T>(source: () -> T?) {
  while let x = source() {
  }
}

// <rdar://problem/19382942> Improve 'if let' to avoid optional pyramid of doom
// CHECK-LABEL: sil hidden @_TF16if_while_binding16while_loop_multiFT_T_
func while_loop_multi() {
  // CHECK:   br [[LOOP_ENTRY:bb[0-9]+]]
  // CHECK: [[LOOP_ENTRY]]:
  // CHECK:         switch_enum {{.*}}, case #Optional.Some!enumelt.1: [[CHECKBUF2:bb.*]], default [[LOOP_EXIT0:bb[0-9]+]]

  // CHECK: [[CHECKBUF2]]([[A:%[0-9]+]] : $String):
  // CHECK:   debug_value [[A]] : $String, let, name "a"

  // CHECK:   switch_enum {{.*}}, case #Optional.Some!enumelt.1: [[LOOP_BODY:bb.*]], default [[LOOP_EXIT2a:bb[0-9]+]]

  // CHECK: [[LOOP_EXIT2a]]:
  // CHECK: release_value [[A]]
  // CHECK: br [[LOOP_EXIT0]]

  // CHECK: [[LOOP_BODY]]([[B:%[0-9]+]] : $String):
  while let a = foo(), b = bar() {
    // CHECK:   debug_value [[B]] : $String, let, name "b"
    // CHECK:   debug_value [[A]] : $String, let, name "c"
    // CHECK:   release_value [[B]]
    // CHECK:   release_value [[A]]
    // CHECK:   br [[LOOP_ENTRY]]
    let c = a
  }
  // CHECK: [[LOOP_EXIT0]]:
  // CHECK-NEXT:   tuple ()
  // CHECK-NEXT:   return
}

// CHECK-LABEL: sil hidden @_TF16if_while_binding8if_multiFT_T_
func if_multi() {
  // CHECK: [[FOO:%[0-9]+]] = function_ref @_TF16if_while_binding3fooFT_GSqSS_
  // CHECK: [[AVAL:%[0-9]+]] = apply [[FOO]]()
  // CHECK: switch_enum [[AVAL]] : $Optional<String>, case #Optional.Some!enumelt.1: [[YESA:bb[0-9]+]], default [[DONE:bb[0-9]+]]

  // CHECK: [[YESA]]([[AVAL:%[0-9]+]] : $String):
  // CHECK:   debug_value [[AVAL]]
  // CHECK:   [[BAR:%[0-9]+]] = function_ref @_TF16if_while_binding3barFT_GSqSS_
  // CHECK:   [[BVAL:%[0-9]+]] = apply [[BAR]]()
  // CHECK:   switch_enum [[BVAL]] : $Optional<String>, case #Optional.Some!enumelt.1: [[YESB:bb[0-9]+]], default [[NOB:bb[0-9]+]]

  // CHECK: [[NOB]]:
  // CHECK:   release_value [[AVAL]]
  // CHECK:   br [[DONE]]
  if let a = foo(), let b = bar() {
    // CHECK: [[YESB]]([[BVAL:%[0-9]+]] : $String):
    // CHECK:   debug_value [[BVAL]]
    // CHECK:   debug_value [[AVAL]]
    // CHECK:   release_value [[BVAL]]
    // CHECK:   release_value [[AVAL]]
    // CHECK:   br [[DONE]]
    let c = a
  }
  // CHECK: [[DONE]]:
  // CHECK-NEXT:   tuple ()
  // CHECK-NEXT:   return
}

// CHECK-LABEL: sil hidden @_TF16if_while_binding13if_multi_elseFT_T_
func if_multi_else() {
  // CHECK: [[FOO:%[0-9]+]] = function_ref @_TF16if_while_binding3fooFT_GSqSS_
  // CHECK: [[AVAL:%[0-9]+]] = apply [[FOO]]()
  // CHECK: switch_enum [[AVAL]] : $Optional<String>, case #Optional.Some!enumelt.1: [[YESA:bb[0-9]+]], default [[ELSE:bb[0-9]+]]

  // CHECK: [[YESA]]([[AVAL:%[0-9]+]] : $String):
  // CHECK:   debug_value [[AVAL]]
  // CHECK:   [[BAR:%[0-9]+]] = function_ref @_TF16if_while_binding3barFT_GSqSS_
  // CHECK:   [[BVAL:%[0-9]+]] = apply [[BAR]]()
  // C/HECK:   switch_enum [[BVAL]] : $Optional<String>, case #Optional.Some!enumelt.1: [[YESB:bb[0-9]+]], default [NOB:bb[0-9]+]]

  // CHECK: [[NOB]]:
  // CHECK:   release_value [[AVAL]]
  // C/HECK:   br [[ELSE]]

  // CHECK: [[YESB]]([[BVAL:%[0-9]+]] : $String):
  // CHECK:   debug_value [[BVAL]]
  // CHECK:   debug_value [[AVAL]]
  // CHECK:   release_value [[BVAL]]
  // CHECK:   release_value [[AVAL]]
  // CHECK:   br [[DONE:bb[0-9]+]]

  // CHECK: [[ELSE]]:
  // Integer literal construction stuff ...
  // CHECK:   br [[DONE]]
  if let a = foo(), let b = bar() {
    let c = a
  } else {
    let d = 0
 }
  // CHECK: [[DONE]]:
  // CHECK-NEXT:   tuple ()
  // CHECK-NEXT:   return
}

// CHECK-LABEL: sil hidden @_TF16if_while_binding14if_multi_whereFT_T_
func if_multi_where() {
  // CHECK: [[FOO:%[0-9]+]] = function_ref @_TF16if_while_binding3foo
  // CHECK: [[AVAL:%[0-9]+]] = apply [[FOO]]()
  // CHECK: switch_enum {{.*}}, case #Optional.Some!enumelt.1: [[YESA:bb.*]], default [[DONE:bb[0-9]+]]

  // CHECK: [[YESA]]([[AVAL:%[0-9]+]] : $String):
  // CHECK:   debug_value [[AVAL]]
  // CHECK:   [[BAR:%[0-9]+]] = function_ref @_TF16if_while_binding3bar
  // CHECK:   [[BVAL:%[0-9]+]] = apply [[BAR]]()
  // CHECK:   switch_enum [[BVAL]] : $Optional<String>, case #Optional.Some!enumelt.1: [[YESB:bb[0-9]+]], default [[NOB:bb[0-9]+]]

  // CHECK: [[NOB]]:
  // CHECK:   release_value [[AVAL]]
  // CHECK:   br [[DONE]]

  // CHECK: [[YESB]]([[BVAL:%[0-9]+]] : $String):
  // CHECK: debug_value [[BVAL]]
  // Check where condition ...
  // CHECK: cond_br %{{[0-9]+}}, [[WHEREYES:bb[0-9]+]], [[WHERENO:bb[0-9]+]]

  // CHECK: [[WHERENO]]:
  // CHECK:   release_value [[BVAL]]
  // CHECK:   release_value [[AVAL]]
  // CHECK:   br [[DONE]]

  if let a = foo(), let b = bar() where a == b {
    // CHECK: [[WHEREYES]]:
    // CHECK:   debug_value [[AVAL]]
    // CHECK:   release_value [[BVAL]]
    // CHECK:   release_value [[AVAL]]
    // CHECK:   br [[DONE]]
    let c = a
  }
  // CHECK: [[DONE]]:
  // CHECK-NEXT:   tuple ()
  // CHECK-NEXT:   return
}


// <rdar://problem/19797158> Swift 1.2's "if" has 2 behaviours. They could be unified.
// CHECK-LABEL: sil hidden @_TF16if_while_binding18if_leading_booleanFSiT_
func if_leading_boolean(a : Int) {
  // Test the boolean condition.
  
  // CHECK: debug_value %0 : $Int, let, name "a"
  // CHECK: [[EQRESULT:%[0-9]+]] = apply {{.*}}(%0, %0) : $@convention(thin) (Int, Int) -> Bool

  // CHECK-NEXT: [[EQRESULTI1:%[0-9]+]] = apply %2([[EQRESULT]]) : $@convention(method) (Bool) -> Builtin.Int1
  // CHECK-NEXT: cond_br [[EQRESULTI1]], [[CHECKFOO:bb[0-9]+]], [[IFDONE:bb[0-9]+]]

  // Call Foo and test for the optional being present.
// CHECK: [[CHECKFOO]]:
  // CHECK: [[OPTRESULT:%[0-9]+]] = apply {{.*}}() : $@convention(thin) () -> @owned Optional<String>
  
  // CHECK:   switch_enum [[OPTRESULT]] : $Optional<String>, case #Optional.Some!enumelt.1: [[SUCCESS:bb.*]], default [[IF_DONE:bb[0-9]+]]

// CHECK: [[SUCCESS]]([[B:%[0-9]+]] : $String):
  // CHECK-NEXT:   debug_value [[B:%[0-9]+]] : $String, let, name "b"
  // CHECK-NEXT:   debug_value [[B:%[0-9]+]] : $String, let, name "c"
  // CHECK-NEXT:   release_value [[B]]
  // CHECK-NEXT:   br [[IFDONE]]
  if a == a, let b = foo() {
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
  // CHECK: bb0(%0 : $Optional<BaseClass>):
  // CHECK-NEXT:   debug_value %0 : $Optional<BaseClass>, let, name "a"
  // CHECK-NEXT:   retain_value %0 : $Optional<BaseClass>
  // CHECK-NEXT:   switch_enum %0 : $Optional<BaseClass>, case #Optional.Some!enumelt.1: [[OPTPRESENTBB:bb[0-9]+]], default [[NILBB:bb[0-9]+]]
  
  // CHECK:      [[OPTPRESENTBB]](%4 : $BaseClass):
  // CHECK-NEXT:   checked_cast_br %4 : $BaseClass to $DerivedClass, [[ISDERIVEDBB:bb[0-9]+]], [[ISBASEBB:bb[0-9]+]]

  // CHECK:      [[ISDERIVEDBB]](%6 : $DerivedClass):
  // CHECK:    enum $Optional<DerivedClass>, #Optional.Some!enumelt.1, %6 : $DerivedClass
  // CHECK:    br [[MERGE:bb[0-9]+]](

  // CHECK: [[ISBASEBB]]:
  // CHECK:    strong_release %4 : $BaseClass
  // CHECK: = enum $Optional<DerivedClass>, #Optional.None!enumelt
  // CHECK: br [[MERGE]](

  // CHECK: [[MERGE]]([[OPTVAL:%[0-9]+]] : $Optional<DerivedClass>):
  // CHECK:    switch_enum [[OPTVAL]] : $Optional<DerivedClass>, case #Optional.Some!enumelt.1: [[ISDERIVEDBB:bb[0-9]+]], default [[NILBB:bb[0-9]+]]

  // CHECK:      [[ISDERIVEDBB]]([[DERIVEDVAL:%[0-9]+]] : $DerivedClass):
  // CHECK-NEXT:   debug_value [[DERIVEDVAL]] : $DerivedClass
  // CHECK-NEXT:   strong_release [[DERIVEDVAL]] : $DerivedClass
  // CHECK-NEXT:   br [[NILBB]]
   
  
  // CHECK:      [[NILBB]]:
  // CHECK-NEXT:   release_value %0 : $Optional<BaseClass>
  // CHECK-NEXT:   tuple ()
  // CHECK-NEXT:   return
  if case let b as DerivedClass = a {

  }
}

// <rdar://problem/22312114> if case crashes swift - bools not supported in let/else yet
// CHECK-LABEL: sil hidden @_TF16if_while_binding12testCaseBoolFGSqSb_T_
func testCaseBool(value : Bool?) {
  // CHECK: bb0(%0 : $Optional<Bool>):
  // CHECK: switch_enum %0 : $Optional<Bool>, case #Optional.Some!enumelt.1: bb1, default bb3
  // CHECK: bb1(%3 : $Bool):
  // CHECK: [[ISTRUE:%[0-9]+]] = struct_extract %3 : $Bool, #Bool._value
  // CHECK: cond_br [[ISTRUE]], bb2, bb3
  // CHECK: bb2:
  // CHECK: function_ref @_TF16if_while_binding8marker_1FT_T_
  // CHECK: br bb3                                          // id: %8
  if case true? = value {
    marker_1()
  }

  // CHECK:   bb3:                                              // Preds: bb0 bb1 bb2
  // CHECK:   switch_enum %0 : $Optional<Bool>, case #Optional.Some!enumelt.1: bb4, default bb6

  // CHECK:   bb4(
  // CHECK:   [[ISTRUE:%[0-9]+]] = struct_extract %10 : $Bool, #Bool._value   // user: %12
  // CHECK:   cond_br [[ISTRUE]], bb6, bb5

  // CHECK: bb5:
  // CHECK: function_ref @_TF16if_while_binding8marker_2FT_T_
  // CHECK: br bb6                                          // id: %15

  // CHECK: bb6:                                              // Preds: bb3 bb4 bb5
  if case false? = value {
    marker_2()
  }
}
