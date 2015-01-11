// RUN: %swift -emit-silgen %s | FileCheck %s

func foo() -> String? { return "" }
func bar() -> String? { return "" }

func a(x: String) {}
func b(x: String) {}
func c(x: String) {}

// CHECK-LABEL: sil hidden @_TF16if_while_binding10if_no_else
func if_no_else() {
  // CHECK:   [[OPT_BUF:%.*]] = alloc_stack $Optional<String>
  // CHECK:   [[FOO:%.*]] = function_ref @_TF16if_while_binding3fooFT_GSqSS_
  // CHECK:   [[OPT_RES:%.*]] = apply [[FOO]]()
  // CHECK:   store [[OPT_RES]] to [[OPT_BUF]]#1
  // CHECK:   [[HAS_VALUE:%.*]] = select_enum_addr [[OPT_BUF]]#1
  // CHECK:   cond_br [[HAS_VALUE]], [[YES:bb[0-9]+]], [[NO:bb[0-9]+]]
  // CHECK: [[NO]]:
  // CHECK:   destroy_addr [[OPT_BUF]]
  // CHECK:   br [[CONT:bb[0-9]+]]
  if let x = foo() {
  // CHECK: [[YES]]:
  // CHECK:   [[VAL_BUF:%.*]] = unchecked_take_enum_data_addr [[OPT_BUF]]
  // CHECK:   [[VAL:%.*]] = load [[VAL_BUF]]
  // CHECK:   [[A:%.*]] = function_ref @_TF16if_while_binding
  // CHECK:   retain_value [[VAL]]
  // CHECK:   apply [[A]]([[VAL]])
  // CHECK:   release_value [[VAL]]
  // CHECK:   br [[CONT]]
    a(x)
  }
  // CHECK: [[CONT]]:
  // CHECK:   dealloc_stack [[OPT_BUF]]#0
}

// CHECK-LABEL: sil hidden @_TF16if_while_binding13if_else_chainFT_T_ : $@thin () -> () {
func if_else_chain() {
  // CHECK:   [[OPT_BUF:%.*]] = alloc_stack $Optional<String>
  // CHECK:   [[FOO:%.*]] = function_ref @_TF16if_while_binding3foo
  // CHECK:   [[OPT_RES:%.*]] = apply [[FOO]]()
  // CHECK:   store [[OPT_RES]] to [[OPT_BUF]]#1
  // CHECK:   [[HAS_VALUE:%.*]] = select_enum_addr [[OPT_BUF]]#1
  // CHECK:   cond_br [[HAS_VALUE]], [[YESX:bb[0-9]+]], [[NOX:bb[0-9]+]]
  if let x = foo() {
  // CHECK: [[YESX]]:
  // CHECK:   [[VAL_BUF:%.*]] = unchecked_take_enum_data_addr [[OPT_BUF]]
  // CHECK:   [[VAL:%.*]] = load [[VAL_BUF]]
  // CHECK:   [[A:%.*]] = function_ref @_TF16if_while_binding
  // CHECK:   retain_value [[VAL]]
  // CHECK:   apply [[A]]([[VAL]])
  // CHECK:   release_value [[VAL]]
  // CHECK:   br [[CONT_X:bb[0-9]+]]
    a(x)
  // CHECK: [[NOX]]:
  // CHECK:   destroy_addr [[OPT_BUF]]
  // CHECK:   [[OPT_BUF_2:%.*]] = alloc_stack $Optional<String>
  // CHECK:   cond_br {{%.*}}, [[YESY:bb[0-9]+]], [[NOY:bb[0-9]+]]
  } else if var y = bar() {
  // CHECK: [[YESY]]:
  // CHECK:   alloc_box $String
  // CHECK:   br [[CONT_Y:bb[0-9]+]]
    b(y)
  } else {
  // CHECK: [[NOY]]:
  // CHECK:   destroy_addr [[OPT_BUF_2]]
  // CHECK:   br [[CONT_Y]]
    c("")
  }

  // CHECK: [[CONT_Y]]:
  // CHECK:   dealloc_stack [[OPT_BUF_2]]#0

  // CHECK: [[CONT_X]]:
  // CHECK:   dealloc_stack [[OPT_BUF]]#0
}

// CHECK-LABEL: sil hidden @_TF16if_while_binding10while_loopFT_T_ : $@thin () -> () {
func while_loop() {
  // CHECK:   [[OPT_BUF:%.*]] = alloc_stack $Optional<String>
  // CHECK:   br [[LOOP_ENTRY:bb[0-9]+]]
  // CHECK: [[LOOP_ENTRY]]:
  // CHECK:   [[HAS_VALUE:%.*]] = select_enum_addr [[OPT_BUF]]#1
  // CHECK:   cond_br [[HAS_VALUE]], [[LOOP_BODY:bb[0-9]+]], [[LOOP_EXIT1:bb[0-9]+]]
  // CHECK: [[LOOP_EXIT1]]:
  // CHECK:   destroy_addr [[OPT_BUF]]
  // CHECK:   br [[LOOP_EXIT:bb[0-9]+]]

  while let x = foo() {
  // CHECK: [[LOOP_BODY]]:
  // CHECK:   cond_br {{%.*}}, [[YES:bb[0-9]+]], [[NO:bb[0-9]+]]
    if let y = bar() {
  // CHECK: [[YES]]:
  // CHECK-NOT: destroy_addr [[OPT_BUF]]
  // CHECK:     br [[LOOP_END:bb[0-9]+]]
      a(y)
      break
    }
  // CHECK: [[NO]]:
  // CHECK:   br [[LOOP_ENTRY]]
  }
  // CHECK: [[LOOP_EXIT]]:
  // CHECK:   dealloc_stack [[OPT_BUF]]
}

// Don't leak alloc_stacks for address-only conditional bindings in 'while'.
// <rdar://problem/16202294>
// CHECK-LABEL: sil hidden @_TF16if_while_binding18while_loop_generic
// CHECK:         br [[COND:bb[0-9]+]]
// CHECK:       [[COND]]:
// CHECK:         cond_br {{.*}}, [[LOOP:bb.*]], bb{{.*}}
// CHECK:       [[LOOP]]:
// CHECK:         [[X:%.*]] = alloc_stack $T
// CHECK:         destroy_addr [[X]]
// CHECK:         dealloc_stack [[X]]
// CHECK:         br [[COND]]
func while_loop_generic<T>(source: () -> T?) {
  while let x = source() {
  }
}

// <rdar://problem/19382942> Improve 'if let' to avoid optional pyramid of doom
// CHECK-LABEL: sil hidden @_TF16if_while_binding16while_loop_multiFT_T_
func while_loop_multi() {
  // CHECK:   [[OPT_BUF1:%.*]] = alloc_stack $Optional<String>
  // CHECK:   [[OPT_BUF2:%.*]] = alloc_stack $Optional<String>
  // CHECK:   br [[LOOP_ENTRY:bb[0-9]+]]
  // CHECK: [[LOOP_ENTRY]]:
  // CHECK:   [[HAS_VALUE1:%.*]] = select_enum_addr [[OPT_BUF1]]#1
  // CHECK:   cond_br [[HAS_VALUE1]], [[CHECKBUF2:bb[0-9]+]], [[LOOP_EXIT1:bb[0-9]+]]
  // CHECK: [[LOOP_EXIT1]]:
  // CHECK:   destroy_addr [[OPT_BUF1]]#1
  // CHECK:   br [[LOOP_EXIT0:bb[0-9]+]]

  // CHECK: [[CHECKBUF2]]:
  // CHECK:   [[VAL_BUF1:%.*]] = unchecked_take_enum_data_addr [[OPT_BUF1]]#1
  // CHECK:   debug_value {{.*}} : $String  // let a
  // CHECK:   [[HAS_VALUE2:%.*]] = select_enum_addr [[OPT_BUF2]]#1
  // CHECK:   cond_br [[HAS_VALUE2]], [[LOOP_BODY:bb[0-9]+]], [[LOOP_EXIT2:bb[0-9]+]]
  // CHECK: [[LOOP_EXIT2]]:
  // CHECK:   destroy_addr [[OPT_BUF2]]#1
  // CHECK:   br [[LOOP_EXIT2a:bb[0-9]+]]
  // CHECK: [[LOOP_BODY]]:
  while let a = foo(), b = bar() {
    // CHECK:   [[VAL_BUF2:%.*]] = unchecked_take_enum_data_addr [[OPT_BUF2]]#1
    // CHECK:   debug_value {{.*}} : $String  // let b
    // CHECK:   debug_value {{.*}} : $String  // let c
    // CHECK:   br [[LOOP_ENTRY]]
    let c = a
  }
  // CHECK: [[LOOP_EXIT2a]]:
  // CHECK: release_value

  // CHECK: [[LOOP_EXIT0]]:
  // CHECK:   dealloc_stack [[OPT_BUF2]]#0
  // CHECK:   dealloc_stack [[OPT_BUF1]]#0
}

// CHECK-LABEL: sil hidden @_TF16if_while_binding8if_multiFT_T_
func if_multi() {
  // CHECK:   [[OPT_BUF1:%.*]] = alloc_stack $Optional<String>
  // CHECK:   [[OPT_BUF2:%.*]] = alloc_stack $Optional<String>
  // CHECK:   [[HAS_VALUE1:%.*]] = select_enum_addr [[OPT_BUF1]]#1
  // CHECK:   cond_br [[HAS_VALUE1]], [[CHECKBUF2:bb[0-9]+]], [[IF_EXIT1:bb[0-9]+]]
  // CHECK: [[IF_EXIT1]]:
  // CHECK:   destroy_addr [[OPT_BUF1]]#1
  // CHECK:   br [[IF_DONE:bb[0-9]+]]

  // CHECK: [[CHECKBUF2]]:
  // CHECK:   [[VAL_BUF1:%.*]] = unchecked_take_enum_data_addr [[OPT_BUF1]]#1
  // CHECK:   debug_value {{.*}} : $String  // let a
  // CHECK:   [[HAS_VALUE2:%.*]] = select_enum_addr [[OPT_BUF2]]#1
  // CHECK:   cond_br [[HAS_VALUE2]], [[IF_BODY:bb[0-9]+]], [[IF_EXIT2:bb[0-9]+]]

  // CHECK: [[IF_EXIT2]]:
  // CHECK:   destroy_addr [[OPT_BUF2]]#1
  // CHECK:   br [[IF_EXIT1a:bb[0-9]+]]

  // CHECK: [[IF_BODY]]:
  if let a = foo(), var b = bar() {
    // CHECK:   alloc_box $String // var b
    // CHECK:   [[VAL_BUF2:%.*]] = unchecked_take_enum_data_addr [[OPT_BUF2]]#1
    // CHECK:   debug_value {{.*}} : $String  // let c
    // CHECK:   br [[IF_DONE]]
    let c = a
  }
  // CHECK: [[IF_EXIT1a]]:
  // CHECK:   release_value 
  // CHECK:   br [[IF_DONE]]
  
  // CHECK: [[IF_DONE]]:
  // CHECK:   dealloc_stack [[OPT_BUF2]]#0
  // CHECK:   dealloc_stack [[OPT_BUF1]]#0
}

// CHECK-LABEL: sil hidden @_TF16if_while_binding13if_multi_elseFT_T_
func if_multi_else() {
  // CHECK:   [[OPT_BUF1:%.*]] = alloc_stack $Optional<String>
  // CHECK:   [[OPT_BUF2:%.*]] = alloc_stack $Optional<String>
  // CHECK:   [[HAS_VALUE1:%.*]] = select_enum_addr [[OPT_BUF1]]#1
  // CHECK:   cond_br [[HAS_VALUE1]], [[CHECKBUF2:bb[0-9]+]], [[IF_EXIT1:bb[0-9]+]]
  // CHECK: [[IF_EXIT1]]:
  // CHECK:   destroy_addr [[OPT_BUF1]]#1
  // CHECK:   br [[ELSE:bb[0-9]+]]
  // CHECK: [[CHECKBUF2]]:
  // CHECK:   [[VAL_BUF1:%.*]] = unchecked_take_enum_data_addr [[OPT_BUF1]]#1
  // CHECK:   debug_value {{.*}} : $String  // let a
  // CHECK:   [[HAS_VALUE2:%.*]] = select_enum_addr [[OPT_BUF2]]#1
  // CHECK:   cond_br [[HAS_VALUE2]], [[IF_BODY:bb[0-9]+]], [[IF_EXIT2:bb[0-9]+]]
  // CHECK: [[IF_EXIT2]]:
  // CHECK:   destroy_addr [[OPT_BUF2]]#1
  // CHECK:   br [[IF_EXIT2a:bb[0-9]+]]
  // CHECK: [[IF_BODY]]:
  if let a = foo(), var b = bar() {
    // CHECK:   alloc_box $String // var b
    // CHECK:   [[VAL_BUF2:%.*]] = unchecked_take_enum_data_addr [[OPT_BUF2]]#1
    // CHECK:   debug_value {{.*}} : $String  // let c
    // CHECK:   br [[IF_DONE:bb[0-9]+]]
    let c = a
  } else {
    let d = 0
    // CHECK: [[IF_EXIT2a]]:
    // CHECK:   release_value
    // CHECK:   br [[ELSE]]
    // CHECK: [[ELSE]]:
    // CHECK:   debug_value {{.*}} : $Int  // let d
    // CHECK:   br [[IF_DONE]]
 }
  // CHECK: [[IF_DONE]]:
  // CHECK:   dealloc_stack [[OPT_BUF2]]#0
  // CHECK:   dealloc_stack [[OPT_BUF1]]#0
}

// CHECK-LABEL: sil hidden @_TF16if_while_binding14if_multi_whereFT_T_
func if_multi_where() {

  // CHECK:   [[OPT_BUF1:%.*]] = alloc_stack $Optional<String>
  // CHECK:   [[OPT_BUF2:%.*]] = alloc_stack $Optional<String>
  // CHECK:   [[HAS_VALUE1:%.*]] = select_enum_addr [[OPT_BUF1]]#1
  // CHECK:   cond_br [[HAS_VALUE1]], [[CHECKBUF2:bb[0-9]+]], [[IF_EXIT1:bb[0-9]+]]
  // CHECK: [[IF_EXIT1]]:
  // CHECK:   destroy_addr [[OPT_BUF1]]#1
  // CHECK:   br [[IF_DONE:bb[0-9]+]]
  // CHECK: [[CHECKBUF2]]:
  // CHECK:   [[VAL_BUF1:%.*]] = unchecked_take_enum_data_addr [[OPT_BUF1]]#1
  // CHECK:   debug_value [[AVAL:%[0-9]+]] : $String  // let a
  // CHECK:   [[HAS_VALUE2:%.*]] = select_enum_addr [[OPT_BUF2]]#1
  // CHECK:   cond_br [[HAS_VALUE2]], [[CHECK_WHERE:bb[0-9]+]], [[IF_EXIT2:bb[0-9]+]]
  // CHECK: [[IF_EXIT2]]:
  // CHECK:   destroy_addr [[OPT_BUF2]]#1
  // CHECK:   br [[IF_EXIT2a:bb[0-9]+]]
  // CHECK: [[CHECK_WHERE]]:
  // CHECK:   [[BBOX:%[0-9]+]] = alloc_box $String // var b
  // CHECK:   [[VAL_BUF2:%.*]] = unchecked_take_enum_data_addr [[OPT_BUF2]]#1
  // CHECK:   function_ref Swift.Bool._getBuiltinLogicValue (Swift.Bool)() -> Builtin.Int1
  // CHECK:   cond_br {{.*}}, [[IF_BODY:bb[0-9]+]], [[IF_EXIT3:bb[0-9]+]]
  if let a = foo(), var b = bar() where a == b {
    // CHECK: [[IF_BODY]]:
    // CHECK:   debug_value [[CVAL:%[0-9]+]] : $String  // let c
    // CHECK:   strong_release [[BBOX]]#0
    // CHECK:   release_value [[AVAL]]
    // CHECK:   br [[IF_DONE]]
    let c = a
  }
  // CHECK: [[IF_EXIT3]]:
  // CHECK:   strong_release [[BBOX]]#0
  // CHECK:   br [[IF_EXIT2a]]
  // CHECK: [[IF_EXIT2a]]:
  // CHECK:   release_value [[AVAL]]
  // CHECK:   br [[IF_DONE]]
  // CHECK: [[IF_DONE]]:
  // CHECK:   dealloc_stack [[OPT_BUF2]]#0
  // CHECK:   dealloc_stack [[OPT_BUF1]]#0
  // CHECK:   return
}
