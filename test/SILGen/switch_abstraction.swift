
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name switch_abstraction -parse-stdlib %s | %FileCheck %s

struct A {}

enum Optionable<T> {
  case Summn(T)
  case Nuttn
}

// CHECK-LABEL: sil hidden [ossa] @$s18switch_abstraction18enum_reabstraction1x1ayAA10OptionableOyAA1AVAHcG_AHtF : $@convention(thin) (@guaranteed Optionable<(A) -> A>, A) -> ()
// CHECK: bb0([[ARG:%.*]] : @guaranteed $Optionable<(A) -> A>,
// CHECK: switch_enum [[ARG]] : $Optionable<(A) -> A>, case #Optionable.Summn!enumelt: [[DEST:bb[0-9]+]]
//
// CHECK: [[DEST]]([[ARG:%.*]] : 
// CHECK:   [[ORIG:%.*]] = copy_value [[ARG]]
// CHECK:   [[CONV:%.*]] = convert_function [[ORIG]]
// CHECK:   [[REABSTRACT:%.*]] = function_ref @$s{{.*}}TR :
// CHECK:   [[SUBST:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACT]]([[CONV]])
func enum_reabstraction(x: Optionable<(A) -> A>, a: A) {
  switch x {
  case .Summn(var f):
    f(a)
  case .Nuttn:
    ()
  }
}

enum Wacky<A, B> {
  case Foo(A)
  case Bar((B) -> A)
}

// CHECK-LABEL: sil hidden [ossa] @$s18switch_abstraction45enum_addr_only_to_loadable_with_reabstraction{{[_0-9a-zA-Z]*}}F : $@convention(thin) <T> (@in_guaranteed Wacky<T, A>, A) -> @out T {
// CHECK: switch_enum_addr [[ENUM:%.*]] : $*Wacky<T, A>, {{.*}} case #Wacky.Bar!enumelt: [[DEST:bb[0-9]+]]
// CHECK: [[DEST]]:
// CHECK:   [[ORIG_ADDR:%.*]] = unchecked_take_enum_data_addr [[ENUM]] : $*Wacky<T, A>, #Wacky.Bar
// CHECK:   [[ORIG:%.*]] = load [take] [[ORIG_ADDR]]
// CHECK:   [[CONV:%.*]] = convert_function [[ORIG]]
// CHECK:   [[REABSTRACT:%.*]] = function_ref @$s{{.*}}TR :
// CHECK:   [[SUBST:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACT]]<T>([[CONV]])
func enum_addr_only_to_loadable_with_reabstraction<T>(x: Wacky<T, A>, a: A)
  -> T
{
  switch x {
  case .Foo(var b):
    return b
  case .Bar(var f):
    return f(a)
  }
}

func hello() {}
func goodbye(_: Any) {}

// CHECK-LABEL: sil hidden [ossa] @$s18switch_abstraction34requires_address_and_reabstractionyyF : $@convention(thin) () -> () {
// CHECK: [[FN:%.*]] = function_ref @$s18switch_abstraction5helloyyF : $@convention(thin) () -> ()
// CHECK: [[THICK:%.*]] = thin_to_thick_function [[FN]]
// CHECK: [[BOX:%.*]] = alloc_stack
// CHECK: [[THUNK:%.*]] = function_ref @$sIeg_ytIegr_TR : $@convention(thin) (@guaranteed @callee_guaranteed () -> ()) -> @out ()
// CHECK: [[ABSTRACT:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[THICK]])
// CHECK: [[CONV:%.*]] = convert_function [[ABSTRACT]]
// CHECK: store [[CONV]] to [init] [[BOX]]
func requires_address_and_reabstraction() {
  switch hello {
  case let a as Any: goodbye(a)
  }
}
