// RUN: %target-swift-emit-silgen %s | %FileCheck %s

func foo() {}
func bar() {}
func bas() {}
func zim() {}

// CHECK-LABEL: sil{{.*}} @$s{{.*}}18testArgumentScopes
func testArgumentScopes(trivialArg: Int,
                        regularArg: String,
                        addrOnlyArg: Any,
                        tupleArg: (String, String),
                        addrOnlyTupleArg: (String, Any),
                        inoutTrivialArg: inout Int,
                        inoutRegularArg: inout String,
                        inoutAddrOnlyArg: inout Any,
                        inoutTupleArg: inout (String, String),
                        inoutAddrOnlyTupleArg: inout (String, Any),
                        borrowingTrivialArg: borrowing Int,
                        borrowingRegularArg: borrowing String,
                        borrowingAddrOnlyArg: borrowing Any,
                        borrowingTupleArg: borrowing (String, String),
                        borrowingAddrOnlyTupleArg: borrowing (String, Any),
                        consumingTrivialArg: consuming Int,
                        consumingRegularArg: consuming String,
                        consumingAddrOnlyArg: consuming Any,
                        consumingTupleArg: consuming (String, String),
                        consumingAddrOnlyTupleArg: consuming (String, Any)) {
// CHECK: bb0(
// CHECK-SAME: [[TRIVIAL_ARG:%[0-9]+]] : $Int,
// CHECK-SAME: [[REGULAR_ARG:%[0-9]+]] : @guaranteed $String,
// CHECK-SAME: [[ADDR_ONLY_ARG:%[0-9]+]] : $*Any,
// CHECK-SAME: [[TUPLE_ARG_0:%[0-9]+]] : @guaranteed $String,
// CHECK-SAME: [[TUPLE_ARG_1:%[0-9]+]] : @guaranteed $String,
// CHECK-SAME: [[ADDR_ONLY_TUPLE_ARG_0:%[0-9]+]] : @guaranteed $String,
// CHECK-SAME: [[ADDR_ONLY_TUPLE_ARG_1:%[0-9]+]] : $*Any,
// CHECK-SAME: [[INOUT_TRIVIAL_ARG:%[0-9]+]] : $*Int,
// CHECK-SAME: [[INOUT_REGULAR_ARG:%[0-9]+]] : $*String,
// CHECK-SAME: [[INOUT_ADDR_ONLY_ARG:%[0-9]+]] : $*Any,
// CHECK-SAME: [[INOUT_TUPLE_ARG:%[0-9]+]] : $*(String, String), 
// CHECK-SAME: [[INOUT_ADDR_ONLY_TUPLE_ARG:%[0-9]+]] : $*(String, Any),
// CHECK-SAME: [[BORROWING_TRIVIAL_ARG:%[0-9]+]] : @noImplicitCopy $Int, 
// CHECK-SAME: [[BORROWING_REGULAR_ARG:%[0-9]+]] : @noImplicitCopy @guaranteed $String, 
// CHECK-SAME: [[BORROWING_ADDR_ONLY_ARG:%[0-9]+]] : @noImplicitCopy $*Any,
// CHECK-SAME: [[BORROWING_TUPLE_ARG_0:%[0-9]+]] : @noImplicitCopy @guaranteed $String,
// CHECK-SAME: [[BORROWING_TUPLE_ARG_1:%[0-9]+]] : @noImplicitCopy @guaranteed $String,
// CHECK-SAME: [[BORROWING_ADDR_ONLY_TUPLE_ARG_0:%[0-9]+]] : @noImplicitCopy @guaranteed $String,
// CHECK-SAME: [[BORROWING_ADDR_ONLY_TUPLE_ARG_1:%[0-9]+]] : @noImplicitCopy $*Any, 
// CHECK-SAME: [[CONSUMING_TRIVIAL_ARG:%[0-9]+]] : @noImplicitCopy @_eagerMove $Int,
// CHECK-SAME: [[CONSUMING_REGULAR_ARG:%[0-9]+]] : @noImplicitCopy @_eagerMove @owned $String,
// CHECK-SAME: [[CONSUMING_ADDR_ONLY_ARG:%[0-9]+]] : @noImplicitCopy @_eagerMove $*Any,
// CHECK-SAME: [[CONSUMING_TUPLE_ARG_0:%[0-9]+]] : @noImplicitCopy @_eagerMove @owned $String,
// CHECK-SAME: [[CONSUMING_TUPLE_ARG_1:%[0-9]+]] : @noImplicitCopy @_eagerMove @owned $String,
// CHECK-SAME: [[CONSUMING_ADDR_ONLY_TUPLE_ARG_0:%[0-9]+]] : @noImplicitCopy @_eagerMove @owned $String,
// CHECK-SAME: [[CONSUMING_ADDR_ONLY_TUPLE_ARG_1:%[0-9]+]] : @noImplicitCopy @_eagerMove $*Any
// CHECK-SAME: ):

// -- tupleArg {
// CHECK: [[TUPLE_ARG_IMPLODE:%.*]] = tuple ([[TUPLE_ARG_0]], [[TUPLE_ARG_1]])

// -- addrOnlyTupleArg {
// CHECK: [[ADDR_ONLY_TUPLE_IMPLODE:%.*]] = alloc_stack {{.*}} $(String, Any)
// CHECK: [[ADDR_ONLY_TUPLE_IMPLODE_0:%.*]] = tuple_element_addr [[ADDR_ONLY_TUPLE_IMPLODE]], 0
// CHECK: [[ADDR_ONLY_TUPLE_IMPLODE_1:%.*]] = tuple_element_addr [[ADDR_ONLY_TUPLE_IMPLODE]], 1
// CHECK: [[ADDR_ONLY_TUPLE_0_COPY:%.*]] = copy_value [[ADDR_ONLY_TUPLE_ARG_0]]
// CHECK: store [[ADDR_ONLY_TUPLE_0_COPY]] to [init] [[ADDR_ONLY_TUPLE_IMPLODE_0]]
// CHECK: copy_addr [[ADDR_ONLY_TUPLE_ARG_1]] to [init] [[ADDR_ONLY_TUPLE_IMPLODE_1]]

// -- borrowingRegularArg {
// CHECK: [[BORROWING_REGULAR_ARG_WRAP:%.*]] = copyable_to_moveonlywrapper [guaranteed] [[BORROWING_REGULAR_ARG]]
// CHECK: [[BORROWING_REGULAR_ARG_COPY:%.*]] = copy_value [[BORROWING_REGULAR_ARG_WRAP]]
// CHECK: [[BORROWING_REGULAR_ARG_MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[BORROWING_REGULAR_ARG_COPY]]

// -- borrowingAddrOnlyArg {
// CHECK: [[BORROWING_ADDR_ONLY_ARG_WRAP:%.*]] = copyable_to_moveonlywrapper_addr [[BORROWING_ADDR_ONLY_ARG]]
// CHECK: [[BORROWING_ADDR_ONLY_ARG_MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[BORROWING_ADDR_ONLY_ARG_WRAP]]

// -- borrowingTupleArg {
// CHECK: [[BORROWING_TUPLE_ARG_IMPLODE:%.*]] = tuple ([[BORROWING_TUPLE_ARG_0]], [[BORROWING_TUPLE_ARG_1]])

// -- borrowingAddrOnlyTupleArg {
// CHECK: [[BORROWING_ADDR_ONLY_TUPLE_IMPLODE:%.*]] = alloc_stack {{.*}} $(String, Any)
// CHECK: [[BORROWING_ADDR_ONLY_TUPLE_IMPLODE_0:%.*]] = tuple_element_addr [[BORROWING_ADDR_ONLY_TUPLE_IMPLODE]], 0
// CHECK: [[BORROWING_ADDR_ONLY_TUPLE_IMPLODE_1:%.*]] = tuple_element_addr [[BORROWING_ADDR_ONLY_TUPLE_IMPLODE]], 1
// CHECK: [[BORROWING_ADDR_ONLY_TUPLE_0_COPY:%.*]] = copy_value [[BORROWING_ADDR_ONLY_TUPLE_ARG_0]]
// CHECK: store [[BORROWING_ADDR_ONLY_TUPLE_0_COPY]] to [init] [[BORROWING_ADDR_ONLY_TUPLE_IMPLODE_0]]
// CHECK: copy_addr [[BORROWING_ADDR_ONLY_TUPLE_ARG_1]] to [init] [[BORROWING_ADDR_ONLY_TUPLE_IMPLODE_1]]

// -- consumingTrivialArg {
// CHECK: [[CONSUMING_TRIVIAL_ARG_BOX:%.*]] = alloc_box ${ var @moveOnly Int }
// CHECK: [[CONSUMING_TRIVIAL_ARG_BOX_BORROW:%.*]] = begin_borrow [var_decl] [[CONSUMING_TRIVIAL_ARG_BOX]]
// CHECK: [[CONSUMING_TRIVIAL_ARG_BOX_PROJ:%.*]] = project_box [[CONSUMING_TRIVIAL_ARG_BOX_BORROW]]
// CHECK: [[CONSUMING_TRIVIAL_ARG_BOX_PROJ_MO:%.*]] = moveonlywrapper_to_copyable_addr [[CONSUMING_TRIVIAL_ARG_BOX_PROJ]]
// CHECK: store [[CONSUMING_TRIVIAL_ARG]] to [trivial] [[CONSUMING_TRIVIAL_ARG_BOX_PROJ_MO]]

// -- consumingRegularArg {
// CHECK: [[CONSUMING_REGULAR_ARG_BOX:%.*]] = alloc_box ${ var @moveOnly String }
// CHECK: [[CONSUMING_REGULAR_ARG_BOX_BORROW:%.*]] = begin_borrow [var_decl] [[CONSUMING_REGULAR_ARG_BOX]]
// CHECK: [[CONSUMING_REGULAR_ARG_BOX_PROJ:%.*]] = project_box [[CONSUMING_REGULAR_ARG_BOX_BORROW]]
// CHECK: [[CONSUMING_REGULAR_ARG_BOX_PROJ_MO:%.*]] = moveonlywrapper_to_copyable_addr [[CONSUMING_REGULAR_ARG_BOX_PROJ]]
// CHECK: store [[CONSUMING_REGULAR_ARG]] to [init] [[CONSUMING_REGULAR_ARG_BOX_PROJ_MO]]

// -- consumingAddrOnlyArg {
// CHECK: [[CONSUMING_ADDR_ONLY_ARG_BOX:%.*]] = alloc_box ${ var @moveOnly Any }
// CHECK: [[CONSUMING_ADDR_ONLY_ARG_BOX_BORROW:%.*]] = begin_borrow [var_decl] [[CONSUMING_ADDR_ONLY_ARG_BOX]]
// CHECK: [[CONSUMING_ADDR_ONLY_ARG_BOX_PROJ:%.*]] = project_box [[CONSUMING_ADDR_ONLY_ARG_BOX_BORROW]]
// CHECK: [[CONSUMING_ADDR_ONLY_ARG_BOX_PROJ_MO:%.*]] = moveonlywrapper_to_copyable_addr [[CONSUMING_ADDR_ONLY_ARG_BOX_PROJ]]
// CHECK: copy_addr [take] [[CONSUMING_ADDR_ONLY_ARG]] to [init] [[CONSUMING_ADDR_ONLY_ARG_BOX_PROJ_MO]]

// -- consumingTupleArg {
// CHECK: [[CONSUMING_TUPLE_ARG_IMPLODE:%.*]] = tuple ([[CONSUMING_TUPLE_ARG_0]], [[CONSUMING_TUPLE_ARG_1]])
// CHECK: [[CONSUMING_TUPLE_ARG_BOX:%.*]] = alloc_box ${ var @moveOnly (String, String) }
// CHECK: [[CONSUMING_TUPLE_ARG_BOX_BORROW:%.*]] = begin_borrow [var_decl] [[CONSUMING_TUPLE_ARG_BOX]]
// CHECK: [[CONSUMING_TUPLE_ARG_BOX_PROJ:%.*]] = project_box [[CONSUMING_TUPLE_ARG_BOX_BORROW]]
// CHECK: [[CONSUMING_TUPLE_ARG_BOX_PROJ_MO:%.*]] = moveonlywrapper_to_copyable_addr [[CONSUMING_TUPLE_ARG_BOX_PROJ]]
// CHECK: store [[CONSUMING_TUPLE_ARG_IMPLODE]] to [init] [[CONSUMING_TUPLE_ARG_BOX_PROJ_MO]]

// -- consumingAddrOnlyTupleArg {
// CHECK: [[CONSUMING_ADDR_ONLY_TUPLE_ARG_IMPLODE:%.*]] = alloc_stack $(String, Any)
// CHECK: [[CONSUMING_ADDR_ONLY_TUPLE_ARG_IMPLODE_0:%.*]] = tuple_element_addr [[CONSUMING_ADDR_ONLY_TUPLE_ARG_IMPLODE]], 0
// CHECK: [[CONSUMING_ADDR_ONLY_TUPLE_ARG_IMPLODE_1:%.*]] = tuple_element_addr [[CONSUMING_ADDR_ONLY_TUPLE_ARG_IMPLODE]], 1
// CHECK: store [[CONSUMING_ADDR_ONLY_TUPLE_ARG_0]] to [init] [[CONSUMING_ADDR_ONLY_TUPLE_ARG_IMPLODE_0]]
// CHECK: copy_addr [take] [[CONSUMING_ADDR_ONLY_TUPLE_ARG_1]] to [init] [[CONSUMING_ADDR_ONLY_TUPLE_ARG_IMPLODE_1]]
// CHECK: [[CONSUMING_ADDR_ONLY_TUPLE_ARG_BOX:%.*]] = alloc_box ${ var @moveOnly (String, Any) }
// CHECK: [[CONSUMING_ADDR_ONLY_TUPLE_ARG_BOX_BORROW:%.*]] = begin_borrow [var_decl] [[CONSUMING_ADDR_ONLY_TUPLE_ARG_BOX]]
// CHECK: [[CONSUMING_ADDR_ONLY_TUPLE_ARG_BOX_PROJ:%.*]] = project_box [[CONSUMING_ADDR_ONLY_TUPLE_ARG_BOX_BORROW]]
// CHECK: [[CONSUMING_ADDR_ONLY_TUPLE_ARG_BOX_PROJ_MO:%.*]] = moveonlywrapper_to_copyable_addr [[CONSUMING_ADDR_ONLY_TUPLE_ARG_BOX_PROJ]]
// CHECK: copy_addr [take] [[CONSUMING_ADDR_ONLY_TUPLE_ARG_IMPLODE]] to [init] [[CONSUMING_ADDR_ONLY_TUPLE_ARG_BOX_PROJ_MO]]

// -- consumingAddrOnlyTupleArg }
// CHECK: end_formal_scope [[CONSUMING_ADDR_ONLY_TUPLE_ARG_BOX_PROJ]]

// -- consumingTupleArg }
// CHECK: end_formal_scope [[CONSUMING_TUPLE_ARG_BOX_PROJ]]

// -- consumingAddrOnlyArg }
// CHECK: end_formal_scope [[CONSUMING_ADDR_ONLY_ARG_BOX_PROJ]]

// -- consumingRegularArg }
// CHECK: end_formal_scope [[CONSUMING_REGULAR_ARG_BOX_PROJ]]

// -- consumingTrivialArg }
// CHECK: end_formal_scope [[CONSUMING_TRIVIAL_ARG_BOX_PROJ]]

// -- borrowingAddrOnlyTupleArg }
// CHECK: end_formal_scope [[BORROWING_ADDR_ONLY_TUPLE_IMPLODE]]

// -- borrowingTupleArg }
// TODO: should be on marker rather than tuple?
// CHECK: end_formal_scope [[BORROWING_TUPLE_ARG_IMPLODE]]

// -- borrowingAddrOnlyArg }
// CHECK: end_formal_scope [[BORROWING_ADDR_ONLY_ARG_MARK]]

// -- borrowingRegularArg }
// TODO: should be on marker rather than orig?
// CHECK: end_formal_scope [[BORROWING_REGULAR_ARG]]

// -- borrowingTrivialArg }
// TODO: should be on marker rather than orig?
// CHECK: end_formal_scope [[BORROWING_TRIVIAL_ARG]]

// -- inoutAddrOnlyTupleArg }
// CHECK: end_formal_scope [[INOUT_ADDR_ONLY_TUPLE_ARG]]

// -- inoutTupleArg }
// CHECK: end_formal_scope [[INOUT_TUPLE_ARG]]

// -- inoutAddrOnlyArg }
// CHECK: end_formal_scope [[INOUT_ADDR_ONLY_ARG]]

// -- inoutRegularArg }
// CHECK: end_formal_scope [[INOUT_REGULAR_ARG]]

// -- inoutTrivialArg }
// CHECK: end_formal_scope [[INOUT_TRIVIAL_ARG]]

// -- addrOnlyTupleArg }
// CHECK: end_formal_scope [[ADDR_ONLY_TUPLE_IMPLODE]]

// -- tupleArg }
// CHECK: end_formal_scope [[TUPLE_ARG_IMPLODE]]

// -- addrOnlyArg }
// CHECK: end_formal_scope [[ADDR_ONLY_ARG]]

// -- regularArg }
// CHECK: end_formal_scope [[REGULAR_ARG]]

// -- trivialArg }
// CHECK: end_formal_scope [[TRIVIAL_ARG]]
}

// CHECK-LABEL: sil{{.*}} @$s{{.*}}13testLetScopes
func testLetScopes() {
  // CHECK: [[INT_INIT:%.*]] = function_ref @[[INT_INIT_FUNC:\$sSi.*C]] :
  // CHECK: [[ONE:%.*]] = apply [[INT_INIT]](
  // CHECK: [[TRIVIAL_LET:%.*]] = move_value [var_decl] [[ONE]]
  let trivialLet: Int = 1
  // CHECK: [[STRING_INIT:%.*]] = function_ref @$sSS{{.*}}C :
  // CHECK: [[STRING:%.*]] = apply [[STRING_INIT]](
  // CHECK: [[REGULAR_LET:%.*]] = move_value [var_decl] [[STRING]]
  let regularLet: String = "S"
  // CHECK: [[ADDR_ONLY_LET:%.*]] = alloc_stack [lexical] [var_decl] $Any
  let addrOnlyLet: Any = 1
  // CHECK: [[TUPLE_LET_IMPLODE:%.*]] = tuple
  // CHECK: [[TUPLE_LET:%.*]] = move_value [var_decl] [[TUPLE_LET_IMPLODE]]
  let tupleLet = (trivialLet, regularLet)
  // CHECK: [[ADDR_ONLY_TUPLE_LET:%.*]] = alloc_stack [lexical] [var_decl] $(String, Any)
  let addrOnlyTupleLet = (regularLet, addrOnlyLet)
  do {
    // CHECK: [[INT_INIT:%.*]] = function_ref @[[INT_INIT_FUNC]] :
    // CHECK: [[TWO:%.*]] = apply [[INT_INIT]](
    // CHECK: [[NESTED_LET:%.*]] = move_value [var_decl] [[TWO]]
    let nestedLet = 2
    // CHECK: [[FOO:%.*]] = function_ref @$s{{.*}}3foo
    // CHECK: apply [[FOO]]()
    foo()
    // CHECK: end_formal_scope [[NESTED_LET]]
  }
  // CHECK: [[BAR:%.*]] = function_ref @$s{{.*}}3bar
  // CHECK: apply [[BAR]]()
  bar()
  do {
    // Ensure that formal scope follows the variable name, not the
    // initialization
    // CHECK: [[DEFERRED_LET:%.*]] = alloc_stack [var_decl] $Int, let, name "deferredLet"
    // CHECK: [[DEFERRED_LET_MARK:%.*]] = mark_uninitialized [var] [[DEFERRED_LET]]
    let deferredLet: Int
    // CHECK: [[FOO:%.*]] = function_ref @$s{{.*}}3foo
    // CHECK: apply [[FOO]]()
    foo()
    do {
      // CHECK: [[INT_INIT:%.*]] = function_ref @[[INT_INIT_FUNC]] :
      // CHECK: [[THREE:%.*]] = apply [[INT_INIT]](
      deferredLet = 3
    }
    // CHECK: [[BAR:%.*]] = function_ref @$s{{.*}}3bar
    // CHECK: apply [[BAR]]()
    bar()
    // CHECK: end_formal_scope [[DEFERRED_LET_MARK]]
  }
  // CHECK: [[BAS:%.*]] = function_ref @$s{{.*}}3bas
  // CHECK: apply [[BAS]]()
  bas()
  // CHECK: end_formal_scope [[ADDR_ONLY_TUPLE_LET]]
  // CHECK: end_formal_scope [[TUPLE_LET]]
  // CHECK: end_formal_scope [[ADDR_ONLY_LET]]
  // CHECK: end_formal_scope [[REGULAR_LET]]
  // CHECK: end_formal_scope [[TRIVIAL_LET]]
}

// CHECK-LABEL: sil{{.*}} @$s{{.*}}13testVarScopes
func testVarScopes() {
  // CHECK: [[TRIVIAL_VAR_BOX:%.*]] = alloc_box ${ var Int }
  // CHECK: [[TRIVIAL_VAR_BOX_B:%.*]] = begin_borrow [var_decl] [[TRIVIAL_VAR_BOX]]
  // CHECK: [[TRIVIAL_VAR:%.*]] = project_box [[TRIVIAL_VAR_BOX_B]]
  var trivialVar: Int = 1
  // CHECK: [[REGULAR_VAR_BOX:%.*]] = alloc_box ${ var String }
  // CHECK: [[REGULAR_VAR_BOX_B:%.*]] = begin_borrow [var_decl] [[REGULAR_VAR_BOX]]
  // CHECK: [[REGULAR_VAR:%.*]] = project_box [[REGULAR_VAR_BOX_B]]
  var regularVar: String = "1"
  // CHECK: [[ADDR_ONLY_VAR_BOX:%.*]] = alloc_box ${ var Any }
  // CHECK: [[ADDR_ONLY_VAR_BOX_B:%.*]] = begin_borrow [lexical] [var_decl] [[ADDR_ONLY_VAR_BOX]]
  // CHECK: [[ADDR_ONLY_VAR:%.*]] = project_box [[ADDR_ONLY_VAR_BOX_B]]
  var addrOnlyVar: Any = 1
  // CHECK: [[TUPLE_VAR_BOX:%.*]] = alloc_box ${ var (Int, String) }
  // CHECK: [[TUPLE_VAR_BOX_B:%.*]] = begin_borrow [var_decl] [[TUPLE_VAR_BOX]]
  // CHECK: [[TUPLE_VAR:%.*]] = project_box [[TUPLE_VAR_BOX_B]]
  var tupleAny = (trivialVar, regularVar)
  // CHECK: [[ADDR_ONLY_TUPLE_VAR_BOX:%.*]] = alloc_box ${ var (String, Any) }
  // CHECK: [[ADDR_ONLY_TUPLE_VAR_BOX_B:%.*]] = begin_borrow [lexical] [var_decl] [[ADDR_ONLY_TUPLE_VAR_BOX]]
  // CHECK: [[ADDR_ONLY_TUPLE_VAR:%.*]] = project_box [[ADDR_ONLY_TUPLE_VAR_BOX_B]]
  var addrOnlyTupleAny = (regularVar, addrOnlyVar)
  do {
    // CHECK: [[NESTED_VAR_BOX:%.*]] = alloc_box ${ var Int }
    // CHECK: [[NESTED_VAR_BOX_B:%.*]] = begin_borrow [var_decl] [[NESTED_VAR_BOX]]
    // CHECK: [[NESTED_VAR:%.*]] = project_box [[NESTED_VAR_BOX_B]]
    var nestedVar = 2
    // CHECK: [[FOO:%.*]] = function_ref @$s{{.*}}3foo
    // CHECK: apply [[FOO]]()
    foo()
    // CHECK: end_formal_scope [[NESTED_VAR]]
  }
  // CHECK: [[BAR:%.*]] = function_ref @$s{{.*}}3bar
  // CHECK: apply [[BAR]]()
  bar()
  do {
    // CHECK: [[DEFERRED_VAR_BOX:%.*]] = alloc_box ${ var Int }
    // CHECK: [[DEFERRED_VAR_BOX_M:%.*]] = mark_uninitialized [var] [[DEFERRED_VAR_BOX]]
    // CHECK: [[DEFERRED_VAR_BOX_B:%.*]] = begin_borrow [var_decl] [[DEFERRED_VAR_BOX_M]]
    // CHECK: [[DEFERRED_VAR:%.*]] = project_box [[DEFERRED_VAR_BOX_B]]
    var deferredVar: Int
    // CHECK: [[FOO:%.*]] = function_ref @$s{{.*}}3foo
    // CHECK: apply [[FOO]]()
    foo()
    do {
      deferredVar = 2
    }
    // CHECK: [[BAR:%.*]] = function_ref @$s{{.*}}3bar
    // CHECK: apply [[BAR]]()
    bar()
    // CHECK: end_formal_scope [[DEFERRED_VAR]]
  }
  // CHECK: [[BAS:%.*]] = function_ref @$s{{.*}}3bas
  // CHECK: apply [[BAS]]()
  bas()
  // CHECK: end_formal_scope [[ADDR_ONLY_TUPLE_VAR]]
  // CHECK: end_formal_scope [[TUPLE_VAR]]
  // CHECK: end_formal_scope [[ADDR_ONLY_VAR]]
  // CHECK: end_formal_scope [[REGULAR_VAR]]
  // CHECK: end_formal_scope [[TRIVIAL_VAR]]
}

// CHECK-LABEL: sil{{.*}} @$s{{.*}}15testGuardScopes
func testGuardScopes(trivial: Int?, regular: String?, ao: Any?) {
// CHECK: bb0(
// CHECK-SAME: [[TRIVIAL:%[0-9]+]] : $Optional<Int>,
// CHECK-SAME: [[REGULAR:%[0-9]+]] : @guaranteed $Optional<String>,
// CHECK-SAME: [[AO:%[0-9]+]] : $*Optional<Any>
// CHECK-SAME: ):

  // CHECK:   switch_enum [[TRIVIAL]], case #Optional.some!enumelt: [[DID_UNWRAP_TRIVIAL:bb[0-9]+]],
  // CHECK: [[DID_UNWRAP_TRIVIAL]]([[UNWRAPPED_TRIVIAL_ARG:%.*]] : $Int):
  // CHECK:   [[UNWRAPPED_TRIVIAL:%.*]] = move_value [var_decl] [[UNWRAPPED_TRIVIAL_ARG]]
  guard let unwrappedTrivial = trivial else { return }
  
  // CHECK:   [[REGULAR_C:%.*]] = copy_value [[REGULAR]]
  // CHECK:   switch_enum [[REGULAR_C]], case #Optional.some!enumelt: [[DID_UNWRAP_REGULAR:bb[0-9]+]],
  // CHECK: [[DID_UNWRAP_REGULAR]]([[UNWRAPPED_REGULAR_ARG:%.*]] : @owned $String):
  // CHECK:   [[UNWRAPPED_REGULAR:%.*]] = move_value [var_decl] [[UNWRAPPED_REGULAR_ARG]]
  guard let unwrappedRegular = regular else { return }

  do {
    // CHECK:   [[UNWRAPPED_AO:%.*]] = alloc_stack [lexical] [var_decl] $Any
    // CHECK:   [[AO_C:%.*]] = alloc_stack $Optional<Any>
    // CHECK:   copy_addr [[AO]] to [init] [[AO_C]]
    // CHECK:   switch_enum_addr [[AO_C]], case #Optional.some!enumelt: [[DID_UNWRAP_AO:bb[0-9]+]],
    // CHECK: [[DID_UNWRAP_AO]]:
    guard let unwrappedAO = ao else { return }
    // CHECK: [[FOO:%.*]] = function_ref @$s{{.*}}3foo
    // CHECK: apply [[FOO]]()
    foo()
    // CHECK: end_formal_scope [[UNWRAPPED_AO]]
  }
  // CHECK: [[BAR:%.*]] = function_ref @$s{{.*}}3bar
  // CHECK: apply [[BAR]]()
  bar()
  // CHECK: end_formal_scope [[UNWRAPPED_REGULAR]]
  // CHECK: end_formal_scope [[UNWRAPPED_TRIVIAL]]
}

enum SwitchBindingScopeTest {
  case trivial(Int)
  case regular(String)
  case ao(Any)
  case trivial2(Int)
  case regular2(String)
  case ao2(Any)
}

// CHECK-LABEL: sil{{.*}} @$s{{.*}}18testSwitchBindings
func testSwitchBindings(e: SwitchBindingScopeTest, c: Bool) {
  switch e {
  // CHECK: switch_enum_addr {{%[0-9]+}},
  // CHECK-SAME: case #SwitchBindingScopeTest.trivial!enumelt: [[CASE_TRIVIAL:bb[0-9]+]]
  // CHECK-SAME: case #SwitchBindingScopeTest.trivial2!enumelt: [[CASE_TRIVIAL2:bb[0-9]+]]
  
  // CHECK-DAG: [[CASE_TRIVIAL]]:
  // CHECK:   [[TRIVIAL_DATA:%.*]] = unchecked_inplace_enum_data_addr
  // CHECK:   [[TRIVIAL_VAL:%.*]] = load [trivial] [[TRIVIAL_DATA]]
  // CHECK:   [[TRIVIAL:%.*]] = move_value [var_decl] [[TRIVIAL_VAL]]
  // CHECK:   cond_br {{.*}}, [[CASE_TRIVIAL_GUARD_SUCCEED:bb[0-9]+]], [[CASE_TRIVIAL_GUARD_FAILED:bb[0-9]+]]
  case .trivial(let trivial) where c:
  // CHECK: [[CASE_TRIVIAL_GUARD_SUCCEED]]:
  // CHECK:   [[FOO:%.*]] = function_ref @$s{{.*}}3foo
  // CHECK:   apply [[FOO]]()
    foo()
  // CHECK:   end_formal_scope [[TRIVIAL]]
  
  // CHECK: [[CASE_TRIVIAL_GUARD_FAILED]]:
  // CHECK:   end_formal_scope [[TRIVIAL]]
  
  // CHECK:   [[TRIVIAL_FALLTHROUGH_1:%.*]] = move_value [var_decl] [[TRIVIAL_VAL]]
  // CHECK:   cond_br {{.*}}, [[CASE_TRIVIAL_FALLTHROUGH_GUARD_SUCCEED:bb[0-9]+]], [[CASE_TRIVIAL_FALLTHROUGH_GUARD_FAILED:bb[0-9]+]]
  case .trivial(let trivialFallthrough) where c:
  // CHECK: [[CASE_TRIVIAL_FALLTHROUGH_GUARD_SUCCEED]]:
  // CHECK:   [[BAR:%.*]] = function_ref @$s{{.*}}3bar
  // CHECK:   apply [[BAR]]()
    bar()
  // -- `fallthrough` is implemented by passing the binding value along to
  // a different variable with the same name. so this `let trivialFallthrough`
  // scope ends, while a new one begins in the next BB
  // CHECK:   end_formal_scope [[TRIVIAL_FALLTHROUGH_1]]
  // CHECK:   br [[TRIVIAL_FALLTHROUGH_JOIN:bb[0-9]+]]([[TRIVIAL_FALLTHROUGH_1]])
    fallthrough
  // CHECK: [[CASE_TRIVIAL_FALLTHROUGH_GUARD_FAILED]]:
  // CHECK:   end_formal_scope [[TRIVIAL_FALLTHROUGH_1]]
  // CHECK:   [[TRIVIAL_SHARED:%.*]] = move_value [var_decl] [[TRIVIAL_VAL]]
  // CHECK:   br [[TRIVIAL_SHARED_DEST:bb[0-9]+]]([[TRIVIAL_SHARED]])
  
  // CHECK-DAG: [[CASE_TRIVIAL2]]:
  // CHECK:   [[TRIVIAL2_DATA:%.*]] = unchecked_inplace_enum_data_addr
  // CHECK:   [[TRIVIAL2_VAL:%.*]] = load [trivial] [[TRIVIAL2_DATA]]
  // CHECK:   [[TRIVIAL2:%.*]] = move_value [var_decl] [[TRIVIAL2_VAL]]
  // CHECK:   cond_br {{.*}}, [[CASE_TRIVIAL2_GUARD_SUCCEED:bb[0-9]+]], [[CASE_TRIVIAL2_GUARD_FAILED:bb[0-9]+]]
  // CHECK: [[CASE_TRIVIAL2_GUARD_SUCCEED]]:
  // -- since the same case block is fallen into from above, we have to unify
  // the paths with a new variable binding with the same name. the binding we
  // set up for the guard has its scope ended, and the value gets rebound in 
  // the case body.
  // CHECK:   end_formal_scope [[TRIVIAL2]]
  // CHECK:   br [[TRIVIAL_FALLTHROUGH_JOIN]]([[TRIVIAL2]])
  
  // CHECK: [[TRIVIAL_FALLTHROUGH_JOIN]]([[TRIVIAL_FALLTHROUGH_2:%.*]] : $Int):
  case .trivial2(let trivialFallthrough) where c:
  // CHECK:   [[BAS:%.*]] = function_ref @$s{{.*}}3bas
  // CHECK:   apply [[BAS]]()
    bas()
  // CHECK:   end_formal_scope [[TRIVIAL_FALLTHROUGH_2]]
  
  // CHECK: [[CASE_TRIVIAL2_GUARD_FAILED]]:
  // CHECK:   end_formal_scope [[TRIVIAL2]]
  // CHECK:   [[TRIVIAL2_SHARED:%.*]] = move_value [var_decl] [[TRIVIAL2_VAL]]
  // CHECK:   br [[TRIVIAL_SHARED_DEST]]([[TRIVIAL2_SHARED]])
  
  // CHECK: [[TRIVIAL_SHARED_DEST]]([[TRIVIAL_SHARED_BIND:%.*]] : $Int):
  case .trivial(let trivialShared), .trivial2(let trivialShared):
  // -- the shared case block recreates its binding independent of the paths
  // it may have come in from
  // CHECK:   [[ZIM:%.*]] = function_ref @$s{{.*}}3zim
  // CHECK:   apply [[ZIM]]()
    zim()
  // CHECK:   end_formal_scope [[TRIVIAL_SHARED_BIND]]
  
  default:
    break
  }
}

class TestSpecialDeclScopesInClass {
  // CHECK-LABEL: sil {{.*}} @$s{{.*}}28TestSpecialDeclScopesInClassC{{.*}}c :
  init() {
  // -- scope for self
  // CHECK: bb0([[SELF:%0]] : @owned $TestSpecialDeclScopesInClass):
  // CHECK:   [[FOO:%.*]] = function_ref @$s{{.*}}3foo
  // CHECK:   apply [[FOO]]()
    foo()
  // CHECK:   end_formal_scope [[SELF]]
  }
  
  // CHECK-LABEL: sil {{.*}} @$s{{.*}}28TestSpecialDeclScopesInClassC10delegating{{.*}}C :
  convenience init(delegating: ()) {
  // CHECK: [[SELF_BOX:%.*]] = alloc_box ${ var TestSpecialDeclScopesInClass }
  // CHECK: [[SELF_MARK:%.*]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
  // CHECK: [[SELF_BOX_B:%.*]] = begin_borrow [lexical] [var_decl] [[SELF_MARK]]
  // CHECK: [[SELF:%.*]] = project_box [[SELF_BOX_B]]
  
  // CHECK: [[INIT:%.*]] = class_method
  // CHECK: apply [[INIT]](
    self.init()
  // CHECK: end_formal_scope [[SELF]]
  }
  
  // CHECK-LABEL: sil {{.*}} @$s{{.*}}28TestSpecialDeclScopesInClassC6method
  func method() {
  // -- scope for self
  // CHECK: bb0([[SELF:%0]] : @guaranteed $TestSpecialDeclScopesInClass):
  // CHECK:   [[FOO:%.*]] = function_ref @$s{{.*}}3foo
  // CHECK:   apply [[FOO]]()
    foo()
  // CHECK:   end_formal_scope [[SELF]]
  }
  
  // CHECK-LABEL: sil {{.*}} @$s{{.*}}28TestSpecialDeclScopesInClassC{{.*}}d :
  deinit {
  // -- scope for self
  // CHECK: bb0([[SELF:%0]] : @guaranteed $TestSpecialDeclScopesInClass):
  // CHECK:   [[FOO:%.*]] = function_ref @$s{{.*}}3foo
  // CHECK:   apply [[FOO]]()
    foo()
  // CHECK:   end_formal_scope [[SELF]]
  }
}

class TestSpecialDeclScopesInSubclass : TestSpecialDeclScopesInClass {
  // CHECK-LABEL: sil {{.*}} @$s{{.*}}31TestSpecialDeclScopesInSubclassC{{.*}}c :
  override init() {
  // CHECK: [[SELF_BOX:%.*]] = alloc_box ${ var TestSpecialDeclScopesInSubclass }
  // CHECK: [[SELF_MARK:%.*]] = mark_uninitialized [derivedself] [[SELF_BOX]]
  // CHECK: [[SELF_BOX_B:%.*]] = begin_borrow [lexical] [var_decl] [[SELF_MARK]]
  // CHECK: [[SELF:%.*]] = project_box [[SELF_BOX_B]]
  
  // CHECK: [[SUPER_INIT:%.*]] = function_ref @$s{{.*}}28TestSpecialDeclScopesInClassC{{.*}}c :
  // CHECK: apply [[SUPER_INIT]](
    super.init()
  
  // CHECK: end_formal_scope [[SELF]]
  }
}

struct TestSpecialDeclScopesInStruct: ~Copyable {
  // CHECK-LABEL: sil {{.*}} @$s{{.*}}29TestSpecialDeclScopesInStructV{{.*}}C :
  init() {
  // -- scope for self
  // CHECK: [[SELF_BOX:%.*]] = alloc_box ${ var TestSpecialDeclScopesInStruct }
  // CHECK: [[SELF_MARK:%.*]] = mark_uninitialized [rootself] [[SELF_BOX]]
  // CHECK: [[SELF_BOX_B:%.*]] = begin_borrow [lexical] [var_decl] [[SELF_MARK]]
  // CHECK: [[SELF:%.*]] = project_box [[SELF_BOX_B]]
  
  // CHECK:   [[FOO:%.*]] = function_ref @$s{{.*}}3foo
  // CHECK:   apply [[FOO]]()
    foo()
  // CHECK: end_formal_scope [[SELF]]
  }
  
  // CHECK-LABEL: sil {{.*}} @$s{{.*}}29TestSpecialDeclScopesInStructV{{.*}}D :
  deinit {
    // -- scope for self
    // CHECK: [[SELF:%.*]] = drop_deinit
    
    // CHECK:   [[FOO:%.*]] = function_ref @$s{{.*}}3foo
    // CHECK:   apply [[FOO]]()
    foo()
    // CHECK: end_formal_scope [[SELF]]
  }
}

func escapey(_: @escaping () -> ()) {}
func nonescapey(_: () -> ()) {}

func testCaptures() {
  let trivial = 1
  let normal = "String"
  let ao: Any = 1
  var trivialVar = 1
  var normalVar = "String"
  var aoVar: Any = 1
  
  // CHECK-LABEL: sil {{.*}} @$s{{.*}}12testCaptures{{.*}}fU_ :
  // CHECK: bb0(
  // CHECK-SAME:   [[TRIVIAL:%.*]] : @closureCapture $Int
  // CHECK-SAME:   [[NORMAL:%.*]] : @closureCapture @guaranteed $String
  // CHECK-SAME:   [[AO:%.*]] : @closureCapture $*Any
  // CHECK-SAME:   [[TRIVIAL_VAR_BOX:%.*]] : @closureCapture @guaranteed ${ var Int }
  // CHECK-SAME:   [[NORMAL_VAR_BOX:%.*]] : @closureCapture @guaranteed ${ var String }
  // CHECK-SAME:   [[AO_VAR_BOX:%.*]] : @closureCapture @guaranteed ${ var Any }
  escapey {
    // CHECK: [[TRIVIAL_VAR:%.*]] = project_box [[TRIVIAL_VAR_BOX]]
    // CHECK: [[NORMAL_VAR:%.*]] = project_box [[NORMAL_VAR_BOX]]
    // CHECK: [[AO_VAR:%.*]] = project_box [[AO_VAR_BOX]]
    _ = trivial
    _ = normal
    _ = ao
    _ = trivialVar
    _ = normalVar
    _ = aoVar
    // CHECK: end_formal_scope [[AO_VAR]]
    // CHECK: end_formal_scope [[NORMAL_VAR]]
    // CHECK: end_formal_scope [[TRIVIAL_VAR]]
    // CHECK: end_formal_scope [[AO]]
    // CHECK: end_formal_scope [[NORMAL]]
    // CHECK: end_formal_scope [[TRIVIAL]]
  }
  
  // CHECK-LABEL: sil {{.*}} @$s{{.*}}12testCaptures{{.*}}fU0_ :
  // CHECK: bb0(
  // CHECK-SAME:   [[TRIVIAL:%[0-9]+]] : @closureCapture $Int
  // CHECK-SAME:   [[NORMAL:%[0-9]+]] : @closureCapture @guaranteed $String
  // CHECK-SAME:   [[AO:%[0-9]+]] : @closureCapture $*Any
  // CHECK-SAME:   [[TRIVIAL_VAR:%[0-9]+]] : @closureCapture $*Int
  // CHECK-SAME:   [[NORMAL_VAR:%[0-9]+]] : @closureCapture $*String
  // CHECK-SAME:   [[AO_VAR:%[0-9]+]] : @closureCapture $*Any
  nonescapey {
    _ = trivial
    _ = normal
    _ = ao
    _ = trivialVar
    _ = normalVar
    _ = aoVar
    // CHECK: end_formal_scope [[AO_VAR]]
    // CHECK: end_formal_scope [[NORMAL_VAR]]
    // CHECK: end_formal_scope [[TRIVIAL_VAR]]
    // CHECK: end_formal_scope [[AO]]
    // CHECK: end_formal_scope [[NORMAL]]
    // CHECK: end_formal_scope [[TRIVIAL]]
  }
}
