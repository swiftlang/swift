// RUN: %swift -emit-silgen %s | FileCheck %s

func foo() -> String? { return "" }
func bar() -> String? { return "" }

func a(x: String) {}
func b(x: String) {}
func c(x: String) {}

// CHECK-LABEL: sil @_TF16if_while_binding10if_no_elseFT_T_
func if_no_else() {
  // CHECK:   [[OPT_BUF:%.*]] = alloc_stack $Optional<String>
  // CHECK:   [[FOO:%.*]] = function_ref @_TF16if_while_binding3fooFT_GSqSS_
  // CHECK:   [[OPT_RES:%.*]] = apply [[FOO]]()
  // CHECK:   store [[OPT_RES]] to [[OPT_BUF]]#1
  // CHECK:   [[HAS_VALUE_FN:%.*]] = function_ref @_TFSs22_doesOptionalHaveValueU__FT1vRGSqQ___Bi1_
  // CHECK:   [[HAS_VALUE:%.*]] = apply [transparent] [[HAS_VALUE_FN]]<T = String>([[OPT_BUF]]#1)
  // CHECK:   cond_br [[HAS_VALUE]], [[YES:bb[0-9]+]], [[NO:bb[0-9]+]]
  if let x = foo() {
  // CHECK: [[YES]]:
  // CHECK:   [[GET_OPT_VAL:%.*]] = function_ref @_TFSs17_getOptionalValueU__FT1vGSqQ___Q_
  // CHECK:   apply [transparent] [[GET_OPT_VAL]]<T = String>([[VAL_BUF:%.*]]#1, [[OPT_BUF]]#1)
  // CHECK:   [[VAL:%.*]] = load [[VAL_BUF]]#1
  // CHECK:   [[A:%.*]] = function_ref @_TF16if_while_binding1aFT1xSS_T_
  // CHECK:   [[VAL_COPY:%.*]] = copy_value %10
  // CHECK:   apply [[A]]([[VAL_COPY]])
  // CHECK:   destroy_value [[VAL]]
  // CHECK:   br [[CONT:bb[0-9]+]]
    a(x)
  }
  // CHECK: [[NO]]:
  // CHECK:   destroy_addr [[OPT_BUF]]
  // CHECK:   br [[CONT]]
}

// CHECK-LABEL: sil @_TF16if_while_binding13if_else_chainFT_T_ : $@thin () -> () {
func if_else_chain() {
  // CHECK:   [[OPT_BUF:%.*]] = alloc_stack $Optional<String>
  // CHECK:   [[FOO:%.*]] = function_ref @_TF16if_while_binding3fooFT_GSqSS_
  // CHECK:   [[OPT_RES:%.*]] = apply [[FOO]]()
  // CHECK:   store [[OPT_RES]] to [[OPT_BUF]]#1
  // CHECK:   [[HAS_VALUE_FN:%.*]] = function_ref @_TFSs22_doesOptionalHaveValueU__FT1vRGSqQ___Bi1_
  // CHECK:   [[HAS_VALUE:%.*]] = apply [transparent] [[HAS_VALUE_FN]]<T = String>([[OPT_BUF]]#1)
  // CHECK:   cond_br [[HAS_VALUE]], [[YES:bb[0-9]+]], [[NO:bb[0-9]+]]
  if let x = foo() {
  // CHECK: [[YES]]:
  // CHECK:   [[GET_OPT_VAL:%.*]] = function_ref @_TFSs17_getOptionalValueU__FT1vGSqQ___Q_
  // CHECK:   apply [transparent] [[GET_OPT_VAL]]<T = String>([[VAL_BUF:%.*]]#1, [[OPT_BUF]]#1)
  // CHECK:   [[VAL:%.*]] = load [[VAL_BUF]]#1
  // CHECK:   [[A:%.*]] = function_ref @_TF16if_while_binding1aFT1xSS_T_
  // CHECK:   [[VAL_COPY:%.*]] = copy_value %10
  // CHECK:   apply [[A]]([[VAL_COPY]])
  // CHECK:   destroy_value [[VAL]]
  // CHECK:   br [[CONT_A:bb[0-9]+]]
    a(x)
  // CHECK: [[NO]]:
  // CHECK:   destroy_addr [[OPT_BUF]]
  // CHECK:   [[OPT_BUF_2:%.*]] = alloc_stack $Optional<String>
  // CHECK:   cond_br {{%.*}}, [[YES:bb[0-9]+]], [[NO:bb[0-9]+]]
  } else if var y = bar() {
  // CHECK: [[YES]]:
  // CHECK:   alloc_box $String
  // CHECK:   br [[CONT_B:bb[0-9]+]]
    b(y)
  } else {
  // CHECK: [[NO]]:
  // CHECK:   destroy_addr [[OPT_BUF_2]]
  // CHECK:   br [[CONT_B]]
    c("")
  }

  // CHECK: [[CONT_B]]:
  // CHECK:   dealloc_stack [[OPT_BUF_2]]#0

  // CHECK: [[CONT_A]]:
  // CHECK:   dealloc_stack [[OPT_BUF]]#0
}

// CHECK-LABEL: sil @_TF16if_while_binding10while_loopFT_T_ : $@thin () -> () {
func while_loop() {
  // CHECK:   [[OPT_BUF:%.*]] = alloc_stack $Optional<String>
  // CHECK:   br [[LOOP_ENTRY:bb[0-9]+]]
  // CHECK: [[LOOP_ENTRY]]:
  // CHECK:   [[HAS_VALUE_FN:%.*]] = function_ref @_TFSs22_doesOptionalHaveValueU__FT1vRGSqQ___Bi1_
  // CHECK:   [[HAS_VALUE:%.*]] = apply [transparent] [[HAS_VALUE_FN]]<T = String>([[OPT_BUF]]#1)
  // CHECK:   cond_br [[HAS_VALUE]], [[LOOP_BODY:bb[0-9]+]], [[LOOP_EXIT:bb[0-9]+]]
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
  // CHECK:   destroy_addr [[OPT_BUF]]
  // CHECK:   br [[LOOP_END]]
  // CHECK: [[LOOP_END]]:
  // CHECK:   dealloc_stack [[OPT_BUF]]
}
