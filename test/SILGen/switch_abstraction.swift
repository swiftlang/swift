// RUN: %swift -emit-silgen -parse-stdlib %s | FileCheck %s

struct A {}

enum Optionable<T> {
  case Summn(T)
  case Nuttn
}

// CHECK-LABEL: sil @_TF18switch_abstraction18enum_reabstractionFT1xGOS_10OptionableFVS_1AS1__1aS1__T_ : $@thin (@owned Optionable<A -> A>, A) -> ()
// CHECK: switch_enum {{%.*}} : $Optionable<A -> A>, case #Optionable.Summn!enumelt.1: [[DEST:bb[0-9]+]]
// CHECK: [[DEST]]([[ORIG:%.*]] : $@callee_owned (@out A, @in A) -> ()):
// CHECK:   [[REABSTRACT:%.*]] = function_ref @_TTRXFo_iV18switch_abstraction1A_iS0__XFo_dS0__dS0__
// CHECK:   [[SUBST:%.*]] = partial_apply [[REABSTRACT]]([[ORIG]])
func enum_reabstraction(x: Optionable<A -> A>, a: A) {
  switch x {
  case .Summn(var f):
    f(a)
  case .Nuttn:
  }
}

enum Wacky<A, B> {
  case Foo(A)
  case Bar(B -> A)
}

// CHECK-LABEL: sil @_TF18switch_abstraction45enum_addr_only_to_loadable_with_reabstractionU__FT1xGOS_5WackyQ_VS_1A_1aS1__Q_ : $@thin <T> (@out T, @in Wacky<T, A>, A) -> () {
// CHECK: destructive_switch_enum_addr {{%.*}} : $*Wacky<T, A>, {{.*}} case #Wacky.Bar!enumelt.1: [[DEST:bb[0-9]+]]
// CHECK: [[DEST]]([[ORIG_ADDR:%.*]] : $*@callee_owned (@out T, @in A) -> ()):
// CHECK:   [[ORIG:%.*]] = load [[ORIG_ADDR]]
// CHECK:   [[REABSTRACT:%.*]] = function_ref @_TTRG__XFo_iV18switch_abstraction1A_iQ__XFo_dS0__iQ__
// CHECK:   [[SUBST:%.*]] = partial_apply [[REABSTRACT]]<T = T>([[ORIG]])
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
