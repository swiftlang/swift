// RUN: %target-swift-frontend -enable-sil-opaque-values -emit-sorted-sil -Xllvm -new-mangling-for-tests -Xllvm -sil-full-demangle -emit-silgen %s | %FileCheck %s

// UNSUPPORTED: resilient_stdlib

protocol Foo {
  func foo()
}

protocol P {
  var x : Int { get }
}

func s010_hasVarArg(_ args: Any...) {}

// Test that we still use addresses when dealing with array initialization
// ---
// CHECK-LABEL: sil @_T020opaque_values_silgen21s020_______callVarArgyyF : $@convention(thin) () -> () {
// CHECK: %[[APY:.*]] = apply %{{.*}}<Any>(%{{.*}}) : $@convention(thin) <τ_0_0> (Builtin.Word) -> (@owned Array<τ_0_0>, Builtin.RawPointer)
// CHECK: %[[BRW:.*]] = begin_borrow %[[APY]]
// CHECK: %[[TPL:.*]] = tuple_extract %[[BRW]] : $(Array<Any>, Builtin.RawPointer), 1
// CHECK: end_borrow %[[BRW]] from %[[APY]] : $(Array<Any>, Builtin.RawPointer), $(Array<Any>, Builtin.RawPointer)
// CHECK: destroy_value %[[APY]]
// CHECK: %[[PTR:.*]] = pointer_to_address %[[TPL]] : $Builtin.RawPointer to [strict] $*Any
// CHECK: init_existential_addr %[[PTR]] : $*Any, $Int
// CHECK: return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '_T020opaque_values_silgen21s020_______callVarArgyyF'
public func s020_______callVarArg() {
  s010_hasVarArg(3)
}

// Test emitSemanticStore.
// ---
// CHECK-LABEL: sil hidden @_T020opaque_values_silgen21s030______assigninoutyxz_xtlF : $@convention(thin) <T> (@inout T, @in T) -> () {
// CHECK: bb0([[ARG0:%.*]] : $*T, [[ARG1:%.*]] : $T):
// CHECK:   [[BORROWED_ARG1:%.*]] = begin_borrow [[ARG1]]
// CHECK:   [[CPY:%.*]] = copy_value [[BORROWED_ARG1]] : $T
// CHECK:   assign [[CPY]] to [[ARG0]] : $*T
// CHECK:   end_borrow [[BORROWED_ARG1]] from [[ARG1]]
// CHECK:   destroy_value [[ARG1]] : $T
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '_T020opaque_values_silgen21s030______assigninoutyxz_xtlF'
func s030______assigninout<T>(_ a: inout T, _ b: T) {
  a = b
}

// Test that we no longer use copy_addr or tuple_element_addr when copy by value is possible
// ---
// CHECK-LABEL: sil hidden @_T020opaque_values_silgen21s040___tupleReturnIntSiSi_xtlF : $@convention(thin) <T> (Int, @in T) -> Int {
// CHECK: bb0([[ARG0:%.*]] : $Int, [[ARG1:%.*]] : $T):
// CHECK:   [[TPL:%.*]] = tuple ([[ARG0]] : $Int, [[ARG1]] : $T)
// CHECK:   [[BORROWED_ARG1:%.*]] = begin_borrow [[TPL]] : $(Int, T)
// CHECK:   [[CPY:%.*]] = copy_value [[BORROWED_ARG1]] : $(Int, T)
// CHECK:   [[INT:%.*]] = tuple_extract [[CPY]] : $(Int, T), 0
// CHECK:   [[GEN:%.*]] = tuple_extract [[CPY]] : $(Int, T), 1
// CHECK:   destroy_value [[GEN]] : $T
// CHECK:   end_borrow [[BORROWED_ARG1]] from [[TPL]] : $(Int, T), $(Int, T)
// CHECK:   destroy_value [[TPL]] : $(Int, T)
// CHECK:   return [[INT]]
// CHECK-LABEL: } // end sil function '_T020opaque_values_silgen21s040___tupleReturnIntSiSi_xtlF'
func s040___tupleReturnInt<T>(_ x: (Int, T)) -> Int {
  let y = x.0
  return y
}

// Test returning an opaque tuple of tuples.
// ---
// CHECK-LABEL: sil hidden [noinline] @_T020opaque_values_silgen21s050______multiResultx_x_xttxlF : $@convention(thin) <T> (@in T) -> (@out T, @out T, @out T) {
// CHECK: bb0(%0 : $T):
// CHECK: %[[CP1:.*]] = copy_value %{{.*}} : $T
// CHECK: %[[CP2:.*]] = copy_value %{{.*}} : $T
// CHECK: %[[CP3:.*]] = copy_value %{{.*}} : $T
// CHECK: destroy_value %0 : $T
// CHECK: %[[TPL:.*]] = tuple (%[[CP1]] : $T, %[[CP2]] : $T, %[[CP3]] : $T)
// CHECK: return %[[TPL]] : $(T, T, T)
// CHECK-LABEL: } // end sil function '_T020opaque_values_silgen21s050______multiResultx_x_xttxlF'
@inline(never)
func s050______multiResult<T>(_ t: T) -> (T, (T, T)) {
  return (t, (t, t))
}

// Test returning an opaque tuple of tuples as a concrete tuple.
// ---
// CHECK-LABEL: sil @_T020opaque_values_silgen21s060__callMultiResultSi_Si_SittSi1i_tF : $@convention(thin) (Int) -> (Int, Int, Int) {
// CHECK: bb0(%0 : $Int):
// CHECK: %[[FN:.*]] = function_ref @_T020opaque_values_silgen21s050______multiResultx_x_xttxlF : $@convention(thin) <τ_0_0> (@in τ_0_0) -> (@out τ_0_0, @out τ_0_0, @out τ_0_0)
// CHECK: %[[TPL:.*]] = apply %[[FN]]<Int>(%0) : $@convention(thin) <τ_0_0> (@in τ_0_0) -> (@out τ_0_0, @out τ_0_0, @out τ_0_0)
// CHECK: %[[I1:.*]] = tuple_extract %[[TPL]] : $(Int, Int, Int), 0
// CHECK: %[[I2:.*]] = tuple_extract %[[TPL]] : $(Int, Int, Int), 1
// CHECK: %[[I3:.*]] = tuple_extract %[[TPL]] : $(Int, Int, Int), 2
// CHECK: %[[R:.*]] = tuple (%[[I1]] : $Int, %[[I2]] : $Int, %[[I3]] : $Int)
// CHECK: return %[[R]] : $(Int, Int, Int)
// CHECK-LABEL: } // end sil function '_T020opaque_values_silgen21s060__callMultiResultSi_Si_SittSi1i_tF'
public func s060__callMultiResult(i: Int) -> (Int, (Int, Int)) {
  return s050______multiResult(i)
}

// SILGen, prepareArchetypeCallee. Materialize a
// non-class-constrainted self from a class-constrained archetype.
// ---
// CHECK-LABEL: sil hidden @_T020opaque_values_silgen21s070__materializeSelfyx1t_ts9AnyObjectRzAA3FooRzlF : $@convention(thin) <T where T : AnyObject, T : Foo> (@owned T) -> () {
// CHECK: bb0([[ARG:%.*]] : $T):
// CHECK: [[WITNESS_METHOD:%.*]] = witness_method $T, #Foo.foo!1 : <Self where Self : Foo> (Self) -> () -> () : $@convention(witness_method) <τ_0_0 where τ_0_0 : Foo> (@in_guaranteed τ_0_0) -> ()
// CHECK: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK: apply [[WITNESS_METHOD]]<T>([[BORROWED_ARG]]) : $@convention(witness_method) <τ_0_0 where τ_0_0 : Foo> (@in_guaranteed τ_0_0) -> ()
// CHECK: end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK: destroy_value [[ARG]] : $T
// CHECK: return %{{[0-9]+}} : $()
// CHECK-LABEL: } // end sil function '_T020opaque_values_silgen21s070__materializeSelfyx1t_ts9AnyObjectRzAA3FooRzlF'
func s070__materializeSelf<T: Foo>(t: T) where T: AnyObject {
  t.foo()
}

// Test open existential with opaque values
// ---
// CHECK-LABEL: sil hidden @_T020opaque_values_silgen21s080______________barSiAA1P_p1p_tF : $@convention(thin) (@in P) -> Int {
// CHECK: bb0([[ARG:%.*]] : $P):
// CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:   [[OPENED_ARG:%.*]] = open_existential_opaque [[BORROWED_ARG]] : $P to $@opened
// CHECK:   [[WITNESS_FUNC:%.*]] = witness_method $@opened
// CHECK:   [[RESULT:%.*]] = apply [[WITNESS_FUNC]]<{{.*}}>([[OPENED_ARG]]) : $@convention(witness_method) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> Int
// CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:   destroy_value [[ARG]] : $P
// CHECK:   return [[RESULT]] : $Int
func s080______________bar(p: P) -> Int {
  return p.x
}

// Test OpaqueTypeLowering copyValue and destroyValue.
// ---
// CHECK-LABEL: sil hidden @_T020opaque_values_silgen21s090___________callerxxlF : $@convention(thin) <T> (@in T) -> @out T {
// CHECK: bb0([[ARG:%.*]] : $T):
// CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:   [[COPY:%.*]] = copy_value [[BORROWED_ARG]] : $T
// CHECK:   [[RESULT:%.*]] = apply {{%.*}}<T>([[COPY]]) : $@convention(thin) <τ_0_0> (@in τ_0_0) -> @out τ_0_0
// CHECK:   end_borrow [[BORROWED_ARG:%.*]] from [[ARG]]
// CHECK:   destroy_value [[ARG]] : $T
// CHECK:   return %{{.*}} : $T
// CHECK-LABEL: } // end sil function '_T020opaque_values_silgen21s090___________callerxxlF'
func s090___________caller<T>(_ t: T) -> T {
  return s090___________caller(t)
}

// Test a simple opaque parameter and return value.
// ---
// CHECK-LABEL: sil hidden @_T020opaque_values_silgen21s100_________identityxx1t_tlF : $@convention(thin) <T> (@in T) -> @out T {
// CHECK: bb0([[ARG:%.*]] : $T):
// CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[BORROWED_ARG]] : $T
// CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:   destroy_value [[ARG]] : $T
// CHECK:   return [[COPY_ARG]] : $T
// CHECK-LABEL: } // end sil function '_T020opaque_values_silgen21s100_________identityxx1t_tlF'
func s100_________identity<T>(t: T) -> T {
  return t
}

// Test a guaranteed opaque parameter.
// ---
// CHECK-LABEL: sil hidden [transparent] [thunk] @_T020opaque_values_silgen21s110___GuaranteedSelfVAA3FooAaaDP3fooyyFTW : $@convention(witness_method) (@in_guaranteed s110___GuaranteedSelf) -> () {
// CHECK: bb0(%0 : $s110___GuaranteedSelf):
// CHECK:   %[[F:.*]] = function_ref @_T020opaque_values_silgen21s110___GuaranteedSelfV3fooyyF : $@convention(method) (s110___GuaranteedSelf) -> ()
// CHECK:   apply %[[F]](%0) : $@convention(method) (s110___GuaranteedSelf) -> ()
// CHECK:   return
// CHECK-LABEL: } // end sil function '_T020opaque_values_silgen21s110___GuaranteedSelfVAA3FooAaaDP3fooyyFTW'
struct s110___GuaranteedSelf : Foo {
  func foo() {}
}

// Tests a corner case wherein we used to do a temporary and return a pointer to T instead of T
// ---
// CHECK-LABEL: sil hidden @_T020opaque_values_silgen21s120______returnValuexxlF : $@convention(thin) <T> (@in T) -> @out T {
// CHECK: bb0([[ARG:%.*]] : $T):
// CHECK:   [[BORROWED_ARG1:%.*]] = begin_borrow [[ARG]]
// CHECK:   [[COPY_ARG1:%.*]] = copy_value [[BORROWED_ARG1]] : $T
// CHECK:   end_borrow [[BORROWED_ARG1]] from [[ARG]]
// CHECK:   [[BORROWED_ARG2:%.*]] = begin_borrow [[COPY_ARG1]]
// CHECK:   [[COPY_ARG2:%.*]] = copy_value [[BORROWED_ARG2]] : $T
// CHECK:   end_borrow [[BORROWED_ARG2]] from [[COPY_ARG1]]
// CHECK:   return [[COPY_ARG2]] : $T
// CHECK-LABEL: } // end sil function '_T020opaque_values_silgen21s120______returnValuexxlF'
func s120______returnValue<T>(_ x: T) -> T {
  let y = x
  return y
}

// Tests Optional initialization by value
// ---
// CHECK-LABEL: sil hidden @_T020opaque_values_silgen21s130_____________wrapxSgxlF : $@convention(thin) <T> (@in T) -> @out Optional<T> {
// CHECK: bb0([[ARG:%.*]] : $T):
// CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[BORROWED_ARG]] : $T
// CHECK:   [[OPTIONAL_ARG:%.*]] = enum $Optional<T>, #Optional.some!enumelt.1, [[COPY_ARG]] : $T
// CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:   destroy_value [[ARG]] : $T
// CHECK:   return [[OPTIONAL_ARG]] : $Optional<T>
// CHECK-LABEL: } // end sil function '_T020opaque_values_silgen21s130_____________wrapxSgxlF'
func s130_____________wrap<T>(_ x: T) -> T? {
  return x
}

// Tests For-each statements
// ---
// CHECK-LABEL: sil hidden @_T020opaque_values_silgen21s140______forEachStmtyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:   [[PROJ_BOX_ARG:%.*]] = project_box %{{.*}} : ${ var IndexingIterator<CountableRange<Int>> }
// CHECK:   [[APPLY_ARG1:%.*]] = apply
// CHECK-NOT: alloc_stack $Int
// CHECK-NOT: store [[APPLY_ARG1]] to [trivial]
// CHECK-NOT: alloc_stack $CountableRange<Int>
// CHECK-NOT: dealloc_stack
// CHECK:   [[APPLY_ARG2:%.*]] = apply %{{.*}}<CountableRange<Int>>
// CHECK:   store [[APPLY_ARG2]] to [trivial] [[PROJ_BOX_ARG]]
// CHECK:   br bb1
// CHECK: bb1:
// CHECK-NOT: alloc_stack $Optional<Int>
// CHECK:   [[APPLY_ARG3:%.*]] = apply %{{.*}}<CountableRange<Int>>
// CHECK-NOT: dealloc_stack
// CHECK:   [[ENUM_ARG:%.*]] = select_enum [[APPLY_ARG3]] : $Optional<Int>
// CHECK:   cond_br [[ENUM_ARG]], bb3, bb2
// CHECK: bb2:
// CHECK:   return %{{.*}} : $()
// CHECK: bb3:
// CHECK:   unchecked_enum_data [[APPLY_ARG3]]
// CHECK:   br bb1
// CHECK-LABEL: } // end sil function '_T020opaque_values_silgen21s140______forEachStmtyyF'
func s140______forEachStmt() {
  for _ in 1..<42 {
  }
}

func s150___________anyArg(_: Any) {}

// Tests init of opaque existentials
// ---
// CHECK-LABEL: sil hidden @_T020opaque_values_silgen21s160_______callAnyArgyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:   [[INT_TYPE:%.*]] = metatype $@thin Int.Type
// CHECK:   [[INT_LIT:%.*]] = integer_literal $Builtin.Int2048, 42
// CHECK:   [[INT_ARG:%.*]] = apply %{{.*}}([[INT_LIT]], [[INT_TYPE]]) : $@convention(method) (Builtin.Int2048, @thin Int.Type) -> Int
// CHECK:   [[INIT_OPAQUE:%.*]] = init_existential_opaque [[INT_ARG]] : $Int, $Int, $Any
// CHECK:   apply %{{.*}}([[INIT_OPAQUE]]) : $@convention(thin) (@in Any) -> ()
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '_T020opaque_values_silgen21s160_______callAnyArgyyF'
func s160_______callAnyArg() {
  s150___________anyArg(42)
}

// Tests unconditional_checked_cast for opaque values
// ---
// CHECK-LABEL: sil hidden @_T020opaque_values_silgen21s170____force_convertxylF : $@convention(thin) <T> () -> @out T {
// CHECK: bb0:
// CHECK-NOT: alloc_stack
// CHECK:   [[INT_TYPE:%.*]] = metatype $@thin Int.Type
// CHECK:   [[INT_LIT:%.*]] = integer_literal $Builtin.Int2048, 42
// CHECK:   [[INT_ARG:%.*]] = apply %{{.*}}([[INT_LIT]], [[INT_TYPE]]) : $@convention(method) (Builtin.Int2048, @thin Int.Type) -> Int
// CHECK:   [[INT_CAST:%.*]] = unconditional_checked_cast_opaque [[INT_ARG]] : $Int to $T
// CHECK:   [[CAST_BORROW:%.*]] = begin_borrow [[INT_CAST]] : $T
// CHECK:   [[RETURN_VAL:%.*]] = copy_value [[CAST_BORROW]] : $T
// CHECK:   end_borrow [[CAST_BORROW]] from [[INT_CAST]] : $T, $T
// CHECK:   destroy_value [[INT_CAST]] : $T
// CHECK:   return [[RETURN_VAL]] : $T
// CHECK-LABEL: } // end sil function '_T020opaque_values_silgen21s170____force_convertxylF'
func s170____force_convert<T>() -> T {
  let x : T = 42 as! T
  return x
}
