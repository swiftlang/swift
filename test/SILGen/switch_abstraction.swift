// RUN: %target-swift-frontend -emit-silgen -parse-stdlib %s | %FileCheck %s

struct A {}

enum Optionable<T> {
  case Summn(T)
  case Nuttn
}

// CHECK-LABEL: sil hidden @_T018switch_abstraction18enum_reabstractionyAA10OptionableOyAA1AVAFcG1x_AF1atF : $@convention(thin) (@owned Optionable<(A) -> A>, A) -> ()
// CHECK: switch_enum {{%.*}} : $Optionable<(A) -> A>, case #Optionable.Summn!enumelt.1: [[DEST:bb[0-9]+]]
// CHECK: [[DEST]]([[ORIG:%.*]] : $@callee_owned (@in A) -> @out A):
// CHECK:   [[REABSTRACT:%.*]] = function_ref @_T0{{.*}}TR :
// CHECK:   [[SUBST:%.*]] = partial_apply [[REABSTRACT]]([[ORIG]])
func enum_reabstraction(x x: Optionable<(A) -> A>, a: A) {
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

// CHECK-LABEL: sil hidden @_T018switch_abstraction45enum_addr_only_to_loadable_with_reabstraction{{[_0-9a-zA-Z]*}}F : $@convention(thin) <T> (@in Wacky<T, A>, A) -> @out T {
// CHECK: switch_enum_addr [[ENUM:%.*]] : $*Wacky<T, A>, {{.*}} case #Wacky.Bar!enumelt.1: [[DEST:bb[0-9]+]]
// CHECK: [[DEST]]:
// CHECK:   [[ORIG_ADDR:%.*]] = unchecked_take_enum_data_addr [[ENUM]] : $*Wacky<T, A>, #Wacky.Bar
// CHECK:   [[ORIG:%.*]] = load [take] [[ORIG_ADDR]]
// CHECK:   [[REABSTRACT:%.*]] = function_ref @_T0{{.*}}TR :
// CHECK:   [[SUBST:%.*]] = partial_apply [[REABSTRACT]]<T>([[ORIG]])
func enum_addr_only_to_loadable_with_reabstraction<T>(x x: Wacky<T, A>, a: A)
  -> T
{
  switch x {
  case .Foo(var b):
    return b
  case .Bar(var f):
    return f(a)
  }
}
