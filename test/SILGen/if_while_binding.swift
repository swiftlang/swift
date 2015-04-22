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

// CHECK-LABEL: sil hidden @_TF16if_while_binding13if_else_chainFT_T_ : $@convention(thin) () -> () {
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

// CHECK-LABEL: sil hidden @_TF16if_while_binding10while_loopFT_T_ : $@convention(thin) () -> () {
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
  // CHECK: [[EQRESULT:%[0-9]+]] = apply {{.*}}(%0, %0) : $@convention(thin) (Int, Int) -> Bool

  // CHECK-NEXT: [[EQRESULTI1:%[0-9]+]] = apply %2([[EQRESULT]]) : $@convention(method) (Bool) -> Builtin.Int1
  // CHECK-NEXT: cond_br [[EQRESULTI1]], [[CHECKFOO:bb[0-9]+]], [[IFDONE:bb[0-9]+]]

  // Call Foo and test for the optional being present.
// CHECK: [[CHECKFOO]]:
  // CHECK: [[OPTRESULT:%[0-9]+]] = apply {{.*}}() : $@convention(thin) () -> @owned Optional<String>
  
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
  // CHECK-NEXT:   switch_enum %0 : $Optional<BaseClass>, case #Optional.Some!enumelt.1: [[OPTPRESENTBB:bb[0-9]+]], default [[NILBB:bb[0-9]+]]
  
  // CHECK:      [[OPTPRESENTBB]](%4 : $BaseClass):
  // CHECK-NEXT:   checked_cast_br %4 : $BaseClass to $DerivedClass, [[ISDERIVEDBB:bb[0-9]+]], [[ISBASEBB:bb[0-9]+]]

  // CHECK:      [[ISDERIVEDBB]](%6 : $DerivedClass):
  // CHECK-NEXT:   debug_value %6 : $DerivedClass
  // CHECK-NEXT:   strong_release %4 : $BaseClass
  // CHECK-NEXT:   br [[NILBB]]
  
  // CHECK: [[ISBASEBB]]:
  // CHECK-NEXT: br [[ISBASEBBCLEANUP:bb[0-9]+]]
  
  // CHECK: [[ISBASEBBCLEANUP]]:
  // CHECK-NEXT: strong_release %4 : $BaseClass
  // CHECK-NEXT: br [[NILBB]]
  
  // CHECK:      [[NILBB]]:
  // CHECK-NEXT:   release_value %0 : $Optional<BaseClass>
  // CHECK-NEXT:   tuple ()
  // CHECK-NEXT:   return
  if let b as DerivedClass = a {

  }
}

// CHECK-LABEL: sil hidden @_TF16if_while_binding22testLetElseExprPatternFSiT_

func testLetElseExprPattern(a : Int) {
  marker_1()
  // CHECK: [[M1:%[0-9]+]] = function_ref @_TF16if_while_binding8marker_1FT_T_ : $@convention(thin) () -> ()
  // CHECK-NEXT: apply [[M1]]() : $@convention(thin) () -> ()

  // CHECK: function_ref static Swift.~= infix <A : Swift.Equatable>(A, A) -> Swift.Bool
  // CHECK: cond_br {{.*}}, bb1, bb2
  let 4 = a else { marker_2(); return }

  // Fall through case comes first.

  // CHECK: bb1:
  // CHECK: [[M3:%[0-9]+]] = function_ref @_TF16if_while_binding8marker_3FT_T_ : $@convention(thin) () -> ()
  // CHECK-NEXT: apply [[M3]]() : $@convention(thin) () -> ()
  // CHECK-NEXT: br bb3
  marker_3()

  // CHECK: bb2:
  // CHECK: [[M2:%[0-9]+]] = function_ref @_TF16if_while_binding8marker_2FT_T_ : $@convention(thin) () -> ()
  // CHECK-NEXT: apply [[M2]]() : $@convention(thin) () -> ()
  // CHECK-NEXT: br bb3

  // CHECK: bb3:
  // CHECK-NEXT: tuple ()
  // CHECK-NEXT: return
}


@noreturn
func abort() { abort() }

// CHECK-LABEL: sil hidden @_TF16if_while_binding20testLetElseOptional1FGSqSi_Si
// CHECK-NEXT: bb0(%0 : $Optional<Int>):
// CHECK-NEXT:   debug_value %0 : $Optional<Int>  // let a
// CHECK-NEXT:   switch_enum %0 : $Optional<Int>, case #Optional.Some!enumelt.1: bb1, default bb2
func testLetElseOptional1(a : Int?) -> Int {

// CHECK: bb1(%3 : $Int):
// CHECK-NEXT:   debug_value %3 : $Int  // let t
// CHECK-NEXT:   return %3 : $Int
  let t? = a else { abort() }

// CHECK:  bb2:
// CHECK-NEXT:    // function_ref if_while_binding.abort () -> ()
// CHECK-NEXT:    %6 = function_ref @_TF16if_while_binding5abortFT_T_
// CHECK-NEXT:    %7 = apply %6() : $@convention(thin) @noreturn () -> ()
// CHECK-NEXT:    unreachable
return t
}

// CHECK-LABEL: sil hidden @_TF16if_while_binding20testLetElseOptional2FGSqSS_SS
// CHECK-NEXT: bb0(%0 : $Optional<String>):
// CHECK-NEXT:   debug_value %0 : $Optional<String>  // let a
// CHECK-NEXT:   retain_value %0 : $Optional<String>
// CHECK-NEXT:   switch_enum %0 : $Optional<String>, case #Optional.Some!enumelt.1: bb1, default bb2
func testLetElseOptional2(a : String?) -> String {
  let t? = a else { abort() }

// CHECK:  bb1(%4 : $String):
// CHECK-NEXT:   debug_value %4 : $String  // let t
// CHECK-NEXT:   release_value %0 : $Optional<String>
// CHECK-NEXT:   return %4 : $String

// CHECK:        bb2:
// CHECK-NEXT:   // function_ref if_while_binding.abort () -> ()
// CHECK-NEXT:   %8 = function_ref @_TF16if_while_binding5abortFT_T_
// CHECK-NEXT:   %9 = apply %8()
// CHECK-NEXT:   unreachable
  return t
}

enum MyOpt<T> {
  case None, Some(T)
}

// CHECK-LABEL: sil hidden @_TF16if_while_binding28testAddressOnlyEnumInLetElseU__FGOS_5MyOptQ__Q_ 
// CHECK-NEXT: bb0(%0 : $*T, %1 : $*MyOpt<T>):
// CHECK-NEXT: debug_value_addr %1 : $*MyOpt<T>  // let a
// CHECK-NEXT: %3 = alloc_stack $T  // let t
// CHECK-NEXT: %4 = alloc_stack $MyOpt<T>
// CHECK-NEXT: copy_addr %1 to [initialization] %4#1 : $*MyOpt<T>
// CHECK-NEXT: switch_enum_addr %4#1 : $*MyOpt<T>, case #MyOpt.Some!enumelt.1: bb2, default bb1
func testAddressOnlyEnumInLetElse<T>(a : MyOpt<T>) -> T {
// CHECK:  bb1:
// CHECK-NEXT:   dealloc_stack %4#0
// CHECK-NEXT:   dealloc_stack %3#0
// CHECK-NEXT:   br bb3
  let t? = a else { abort() }

// CHECK:    bb2:
// CHECK-NEXT:     %10 = unchecked_take_enum_data_addr %4#1 : $*MyOpt<T>, #MyOpt.Some!enumelt.1
// CHECK-NEXT:     copy_addr [take] %10 to [initialization] %3#1 : $*T
// CHECK-NEXT:     dealloc_stack %4#0
// CHECK-NEXT:     copy_addr [take] %3#1 to [initialization] %0 : $*T
// CHECK-NEXT:     dealloc_stack %3#0
// CHECK-NEXT:     destroy_addr %1 : $*MyOpt<T>
// CHECK-NEXT:     tuple ()
// CHECK-NEXT:     return 
  
// CHECK:    bb3:
// CHECK-NEXT:     // function_ref if_while_binding.abort () -> ()
// CHECK-NEXT:     %18 = function_ref @_TF16if_while_binding5abortFT_T_
// CHECK-NEXT:     %19 = apply %18() : $@convention(thin) @noreturn () -> ()
// CHECK-NEXT:     unreachable

  return t
}



// CHECK-LABEL: sil hidden @_TF16if_while_binding19testCleanupEmissionU__FQ_T_
// <rdar://problem/20563234> let-else problem: cleanups for bound patterns shouldn't be run in the else block
protocol MyProtocol {}
func testCleanupEmission<T>(x: T) {
  // SILGen shouldn't crash/verify abort on this example.
  let x2? = x as? MyProtocol else { return }
}


// CHECK-LABEL: sil hidden @_TF16if_while_binding15test_is_patternFCS_9BaseClassT_
func test_is_pattern(y : BaseClass) {
  // checked_cast_br %0 : $BaseClass to $DerivedClass
  let is DerivedClass = y else { marker_1(); return }
  
  marker_2()
}

// CHECK-LABEL: sil hidden @_TF16if_while_binding15test_as_patternFCS_9BaseClassCS_12DerivedClass
func test_as_pattern(y : BaseClass) -> DerivedClass {
  // checked_cast_br %0 : $BaseClass to $DerivedClass
  let result as DerivedClass = y else {  }
  // CHECK: bb{{.*}}({{.*}} : $DerivedClass):

  
  // CHECK: bb{{.*}}([[PTR:%[0-9]+]] : $DerivedClass):
  // CHECK-NEXT: debug_value [[PTR]] : $DerivedClass  // let result
  // CHECK-NEXT: strong_release %0 : $BaseClass
  // CHECK-NEXT: return [[PTR]] : $DerivedClass
  return result
}

// CHECK-LABEL: sil hidden @_TF16if_while_binding21testEnumReabstractionFP_T_
func testEnumReabstraction(x: Any) {
  let f? = x as? () -> () else {}
  // CHECK: function_ref reabstraction thunk helper from @callee_owned (@in ()) -> (@out ()) to @callee_owned () -> (@unowned ())
  f()
}

// CHECK-LABEL: sil hidden @_TF16if_while_binding22let_else_tuple_bindingFGSqTSiSi__Si
func let_else_tuple_binding(a : (Int, Int)?) -> Int {

// CHECK-NEXT: bb0(%0 : $Optional<(Int, Int)>):
// CHECK-NEXT:   debug_value %0 : $Optional<(Int, Int)>  // let a
// CHECK-NEXT:   switch_enum %0 : $Optional<(Int, Int)>, case #Optional.Some!enumelt.1: bb1, default bb2

  let (x, y)? = a else { }
  return x

// CHECK: bb1(%3 : $(Int, Int)):
// CHECK-NEXT:   %4 = tuple_extract %3 : $(Int, Int), 0
// CHECK-NEXT:   debug_value %4 : $Int  // let x
// CHECK-NEXT:   %6 = tuple_extract %3 : $(Int, Int), 1
// CHECK-NEXT:   debug_value %6 : $Int  // let y
// CHECK-NEXT:   return %4 : $Int
}



