// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name default_arguments -Xllvm -sil-full-demangle -swift-version 4 %s | %FileCheck %s

protocol DefaultInitializable {
  init()
}

enum NonTrivialDefaults<T: DefaultInitializable> {
  case empty

  // CHECK-LABEL: sil hidden [ossa] @$s17default_arguments18NonTrivialDefaultsO7defarg1yACyxGSi_SdSSxtcAEmAA20DefaultInitializableRzlFfA_{{.*}}
  // CHECK:   [[INT_VAL:%[0-9]+]]  = integer_literal $Builtin.IntLiteral, 17
  // CHECK:   [[META:%[0-9]+]] = metatype $@thin Int.Type
  // CHECK:   [[LIT_FN:%[0-9]+]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC
  // CHECK:   [[LIT_VAL:%[0-9]+]] = apply [[LIT_FN]]([[INT_VAL]], [[META]]) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
  // CHECK:   return [[LIT_VAL]] : $Int
  // CHECK: } // end sil function '$s17default_arguments18NonTrivialDefaultsO7defarg1yACyxGSi_SdSSxtcAEmAA20DefaultInitializableRzlFfA_'

  // CHECK-LABEL: sil hidden [ossa] @$s17default_arguments18NonTrivialDefaultsO7defarg1yACyxGSi_SdSSxtcAEmAA20DefaultInitializableRzlFfA1_{{.*}}
  // CHECK:  [[LIT:%[0-9]+]] = string_literal utf8 "Hello"
  // CHECK:  [[LEN:%[0-9]+]] = integer_literal $Builtin.Word, 5
  // CHECK:  [[STRING:%[0-9]+]] = metatype $@thin String.Type
  // CHECK:  [[LIT_FN:%.*]]  = function_ref @$sSS21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcfC
  // CHECK:  [[LIT_VAL:%.*]] = apply [[LIT_FN]]([[LIT]], [[LEN]], {{[^,]+}}, [[STRING]])
  // CHECK:  return [[LIT_VAL]] : $String
  // CHECK: } // end sil function '$s17default_arguments18NonTrivialDefaultsO7defarg1yACyxGSi_SdSSxtcAEmAA20DefaultInitializableRzlFfA1_'

  // CHECK-LABEL: sil hidden [ossa] @$s17default_arguments18NonTrivialDefaultsO7defarg1yACyxGSi_SdSSxtcAEmAA20DefaultInitializableRzlFfA2_{{.*}}
  // CHECK: bb0([[OUT:%[0-9]+]] : $*T):
  // CHECK:  [[META:%[0-9]+]] = metatype $@thick T.Type
  // CHECK:  [[DEF_INIT_WITNESS:%[0-9]+]] = witness_method $T, #DefaultInitializable.init!allocator : <Self where Self : DefaultInitializable> (Self.Type) -> () -> Self : $@convention(witness_method: DefaultInitializable) <τ_0_0 where τ_0_0 : DefaultInitializable> (@thick τ_0_0.Type) -> @out τ_0_0
  // CHECK:  [[LIT:%[0-9]+]] = apply [[DEF_INIT_WITNESS]]<T>([[OUT]], [[META]]) : $@convention(witness_method: DefaultInitializable) <τ_0_0 where τ_0_0 : DefaultInitializable> (@thick τ_0_0.Type) -> @out τ_0_0
  // CHECK:  [[RET:%[0-9]+]] = tuple ()
  // CHECK:  return [[RET]] : $()
  // CHECK: } // end sil function '$s17default_arguments18NonTrivialDefaultsO7defarg1yACyxGSi_SdSSxtcAEmAA20DefaultInitializableRzlFfA2_'

  case defarg1(i: Int = 17, d: Double, s: String = "Hello", t: T = .init())

  // CHECK-LABEL: sil hidden [ossa] @$s17default_arguments18NonTrivialDefaultsO16indirect_defarg1yACyxGAE_AEt_tcAEmAA20DefaultInitializableRzlFfA_
  // CHECK: bb0([[OUT_VAL1:%[0-9]+]] : $*NonTrivialDefaults<T>, [[OUT_VAL2:%[0-9]+]] : $*NonTrivialDefaults<T>):
  // CHECK:   inject_enum_addr [[OUT_VAL1]] : $*NonTrivialDefaults<T>, #NonTrivialDefaults.empty!enumelt
  // CHECK:   inject_enum_addr [[OUT_VAL2]] : $*NonTrivialDefaults<T>, #NonTrivialDefaults.empty!enumelt
  // CHECK:   [[RET:%[0-9]+]] = tuple ()
  // CHECK:   return [[RET]] : $()
  // CHECK: } // end sil function '$s17default_arguments18NonTrivialDefaultsO16indirect_defarg1yACyxGAE_AEt_tcAEmAA20DefaultInitializableRzlFfA_'

  indirect case indirect_defarg1(f: (NonTrivialDefaults, NonTrivialDefaults) = (.empty, .empty))

  // CHECK-LABEL: sil private [transparent] [ossa] @$s17default_arguments15testDefaultArg2AA18NonTrivialDefaultsOyxGyAA0D13InitializableRzlFSSycfu_{{.*}}
  // CHECK:   string_literal utf8 {{.*}}enum_default_arguments.swift
  // CHECK: } // end sil function '$s17default_arguments15testDefaultArg2AA18NonTrivialDefaultsOyxGyAA0D13InitializableRzlFSSycfu_'

  // CHECK-LABEL: sil private [transparent] [ossa] @$s17default_arguments15testDefaultArg2AA18NonTrivialDefaultsOyxGyAA0D13InitializableRzlFSiycfu0_{{.*}}
  // CHECK:   %0 = integer_literal $Builtin.IntLiteral, [[@LINE+12]]
  // CHECK: } // end sil function '$s17default_arguments15testDefaultArg2AA18NonTrivialDefaultsOyxGyAA0D13InitializableRzlFSiycfu0_'

  case defarg2(x: @autoclosure () -> String = #file, t: @autoclosure () -> Int = #line)

}

func testDefaultArg1<T: DefaultInitializable>() -> NonTrivialDefaults<T> {
  return .defarg1(d: 3.125)
}

func testDefaultArg2<T: DefaultInitializable>() -> NonTrivialDefaults<T> {
  return .defarg2()
}

func testIndirectDefarg1<T: DefaultInitializable>() -> NonTrivialDefaults<T> {
  return .indirect_defarg1()
}

