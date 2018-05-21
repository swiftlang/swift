
// RUN: %target-swift-emit-silgen -module-name switch_abstraction -enable-sil-ownership -parse-stdlib %s | %FileCheck %s

struct A {}

enum Optionable<T> {
  case Summn(T)
  case Nuttn
}

// CHECK-LABEL: sil hidden @$S18switch_abstraction18enum_reabstraction1x1ayAA10OptionableOyAA1AVAHcG_AHtF : $@convention(thin) (@guaranteed Optionable<(A) -> A>, A) -> ()
// CHECK: switch_enum {{%.*}} : $Optionable<(A) -> A>, case #Optionable.Summn!enumelt.1: [[DEST:bb[0-9]+]]
// CHECK: [[DEST]]([[ORIG:%.*]] : @owned $@callee_guaranteed (@in_guaranteed A) -> @out A):
// CHECK:   [[REABSTRACT:%.*]] = function_ref @$S{{.*}}TR :
// CHECK:   [[SUBST:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACT]]([[ORIG]])
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

// CHECK-LABEL: sil hidden @$S18switch_abstraction45enum_addr_only_to_loadable_with_reabstraction{{[_0-9a-zA-Z]*}}F : $@convention(thin) <T> (@in_guaranteed Wacky<T, A>, A) -> @out T {
// CHECK: switch_enum_addr [[ENUM:%.*]] : $*Wacky<T, A>, {{.*}} case #Wacky.Bar!enumelt.1: [[DEST:bb[0-9]+]]
// CHECK: [[DEST]]:
// CHECK:   [[ORIG_ADDR:%.*]] = unchecked_take_enum_data_addr [[ENUM]] : $*Wacky<T, A>, #Wacky.Bar
// CHECK:   [[ORIG:%.*]] = load [take] [[ORIG_ADDR]]
// CHECK:   [[REABSTRACT:%.*]] = function_ref @$S{{.*}}TR :
// CHECK:   [[SUBST:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACT]]<T>([[ORIG]])
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
