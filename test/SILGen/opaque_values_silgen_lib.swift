// RUN: %target-swift-frontend -enable-sil-opaque-values -emit-sorted-sil -Xllvm -new-mangling-for-tests -Xllvm -sil-full-demangle -parse-stdlib -parse-as-library -emit-silgen -module-name Swift %s | %FileCheck %s
// UNSUPPORTED: resilient_stdlib

precedencegroup AssignmentPrecedence { assignment: true }

enum Optional<Wrapped> {
  case none
  case some(Wrapped)
}

protocol EmptyP {}

struct String { var ptr: Builtin.NativeObject }

// Init of Empty protocol + Builtin.NativeObject enum (including opaque tuples as a return value)
// ---
// CHECK-LABEL: sil shared [transparent] [thunk] @_TFOs9PAndSEnum1AFMS_FTPs6EmptyP_SS_S_ : $@convention(thin) (@thin PAndSEnum.Type) -> @owned @callee_owned (@in EmptyP, @owned String) -> @out PAndSEnum {
// CHECK: bb0([[ARG:%.*]] : $@thin PAndSEnum.Type):
// CHECK:   [[RETVAL:%.*]] = partial_apply {{.*}}([[ARG]]) : $@convention(method) (@in EmptyP, @owned String, @thin PAndSEnum.Type) -> @out PAndSEnum
// CHECK:   return [[RETVAL]] : $@callee_owned (@in EmptyP, @owned String) -> @out PAndSEnum
// CHECK-LABEL: } // end sil function '_TFOs9PAndSEnum1AFMS_FTPs6EmptyP_SS_S_'
// CHECK-LABEL: sil shared [transparent] @_TFOs9PAndSEnum1AfMS_FTPs6EmptyP_SS_S_ : $@convention(method) (@in EmptyP, @owned String, @thin PAndSEnum.Type) -> @out PAndSEnum {
// CHECK: bb0([[ARG0:%.*]] : $EmptyP, [[ARG1:%.*]]  : $String, [[ARG2:%.*]] : $@thin PAndSEnum.Type):
// CHECK:   [[RTUPLE:%.*]] = tuple ([[ARG0]] : $EmptyP, [[ARG1]] : $String)
// CHECK:   [[RETVAL:%.*]] = enum $PAndSEnum, #PAndSEnum.A!enumelt.1, [[RTUPLE]] : $(EmptyP, String)
// CHECK:   return [[RETVAL]] : $PAndSEnum
// CHECK-LABEL: } // end sil function '_TFOs9PAndSEnum1AfMS_FTPs6EmptyP_SS_S_'
enum PAndSEnum { case A(EmptyP, String) }

// Tests Empty protocol + Builtin.NativeObject enum (including opaque tuples as a return value)
// ---
// CHECK-LABEL: sil hidden @_TFs21s010______PAndS_casesFT_T_ : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:   [[MTYPE:%.*]] = metatype $@thin PAndSEnum.Type
// CHECK:   [[EAPPLY:%.*]] = apply {{.*}}([[MTYPE]]) : $@convention(thin) (@thin PAndSEnum.Type) -> @owned @callee_owned (@in EmptyP, @owned String) -> @out PAndSEnum
// CHECK:   destroy_value [[EAPPLY]]
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '_TFs21s010______PAndS_casesFT_T_'
func s010______PAndS_cases() {
  _ = PAndSEnum.A
}
