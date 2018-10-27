// RUN: %target-swift-emit-silgen -module-name default_arguments -Xllvm -sil-full-demangle -enable-sil-ownership -swift-version 4 %s | %FileCheck %s

protocol DefaultInitializble {
  init()
}

enum NonTrivialDefaults<T: DefaultInitializble> {
  case empty

  // CHECK-LABEL: sil hidden @$s17default_arguments18NonTrivialDefaultsO7defarg1yACyxGSi_SdSSxtcAEmAA19DefaultInitializbleRzlFfa_{{.*}}
  // CHECK:   [[META:%[0-9]+]] = metatype $@thin Int.Type
  // CHECK:   [[INT_VAL:%[0-9]+]]  = integer_literal $Builtin.IntLiteral, 17
  // CHECK:   [[LIT_FN:%[0-9]+]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC
  // CHECK:   [[LIT_VAL:%[0-9]+]] = apply [[LIT_FN]]([[INT_VAL]], [[META]]) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
  // CHECK:   return [[LIT_VAL]] : $Int
  // CHECK: } // end sil function '$s17default_arguments18NonTrivialDefaultsO7defarg1yACyxGSi_SdSSxtcAEmAA19DefaultInitializbleRzlFfa_'

  // CHECK-LABEL: sil hidden @$s17default_arguments18NonTrivialDefaultsO7defarg1yACyxGSi_SdSSxtcAEmAA19DefaultInitializbleRzlFfa1_{{.*}}
  // CHECK:  [[LIT:%[0-9]+]] = string_literal utf8 "Hello"
  // CHECK:  [[LEN:%[0-9]+]] = integer_literal $Builtin.Word, 5
  // CHECK:  [[STRING:%[0-9]+]] = metatype $@thin String.Type
  // CHECK:  [[LIT_FN:%.*]]  = function_ref @$sSS21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcfC
  // CHECK:  [[LIT_VAL:%.*]] = apply [[LIT_FN]]([[LIT]], [[LEN]], {{[^,]+}}, [[STRING]])
  // CHECK:  return [[LIT_VAL]] : $String
  // CHECK: } // end sil function '$s17default_arguments18NonTrivialDefaultsO7defarg1yACyxGSi_SdSSxtcAEmAA19DefaultInitializbleRzlFfa1_'

  // CHECK-LABEL: sil hidden @$s17default_arguments18NonTrivialDefaultsO7defarg1yACyxGSi_SdSSxtcAEmAA19DefaultInitializbleRzlFfa2_{{.*}}
  // CHECK: bb0([[OUT:%[0-9]+]] : @trivial $*T):
  // CHECK:  [[META:%[0-9]+]] = metatype $@thick T.Type
  // CHECK:  [[DEF_INIT_WITNESS:%[0-9]+]] = witness_method $T, #DefaultInitializble.init!allocator.1 : <Self where Self : DefaultInitializble> (Self.Type) -> () -> Self : $@convention(witness_method: DefaultInitializble) <τ_0_0 where τ_0_0 : DefaultInitializble> (@thick τ_0_0.Type) -> @out τ_0_0
  // CHECK:  [[LIT:%[0-9]+]] = apply [[DEF_INIT_WITNESS]]<T>([[OUT]], [[META]]) : $@convention(witness_method: DefaultInitializble) <τ_0_0 where τ_0_0 : DefaultInitializble> (@thick τ_0_0.Type) -> @out τ_0_0
  // CHECK:  [[RET:%[0-9]+]] = tuple ()
  // CHECK:  return [[RET]] : $()
  // CHECK: } // end sil function '$s17default_arguments18NonTrivialDefaultsO7defarg1yACyxGSi_SdSSxtcAEmAA19DefaultInitializbleRzlFfa2_'

  case defarg1(i: Int = 17, d: Double, s: String = "Hello", t: T = .init())

  // CHECK-LABEL: sil private [transparent] @$s17default_arguments15testDefaultArg2AA18NonTrivialDefaultsOyxGyAA0D12InitializbleRzlFSSyXAfu_{{.*}}
  // CHECK:   string_literal utf8 {{.*}}enum_default_arguments.swift
  // CHECK: } // end sil function '$s17default_arguments15testDefaultArg2AA18NonTrivialDefaultsOyxGyAA0D12InitializbleRzlFSSyXAfu_'

  // CHECK-LABEL: sil private [transparent] @$s17default_arguments15testDefaultArg2AA18NonTrivialDefaultsOyxGyAA0D12InitializbleRzlFSiyXAfu0_{{.*}}
  // CHECK:   %1 = integer_literal $Builtin.IntLiteral, [[@LINE+21]]
  // CHECK: } // end sil function '$s17default_arguments15testDefaultArg2AA18NonTrivialDefaultsOyxGyAA0D12InitializbleRzlFSiyXAfu0_'

  case defarg2(x: @autoclosure () -> String = #file, t: @autoclosure () -> Int = #line)

  // CHECK-LABEL: sil hidden @$s17default_arguments18NonTrivialDefaultsO16indirect_defarg1yACyxGAE_AEt_tcAEmAA19DefaultInitializbleRzlFfa_
  // CHECK: bb0([[OUT_VAL1:%[0-9]+]] : @trivial $*NonTrivialDefaults<T>, [[OUT_VAL2:%[0-9]+]] : @trivial $*NonTrivialDefaults<T>):
  // CHECK:   inject_enum_addr [[OUT_VAL1]] : $*NonTrivialDefaults<T>, #NonTrivialDefaults.empty!enumelt
  // CHECK:   inject_enum_addr [[OUT_VAL2]] : $*NonTrivialDefaults<T>, #NonTrivialDefaults.empty!enumelt
  // CHECK:   [[RET:%[0-9]+]] = tuple ()
  // CHECK:   return [[RET]] : $()
  // CHECK: } // end sil function '$s17default_arguments18NonTrivialDefaultsO16indirect_defarg1yACyxGAE_AEt_tcAEmAA19DefaultInitializbleRzlFfa_'

  indirect case indirect_defarg1(f: (NonTrivialDefaults, NonTrivialDefaults) = (.empty, .empty))
}

func testDefaultArg1<T: DefaultInitializble>() -> NonTrivialDefaults<T> {
  return .defarg1(d: 3.125)
}

func testDefaultArg2<T: DefaultInitializble>() -> NonTrivialDefaults<T> {
  return .defarg2()
}

func testIndirectDefarg1<T: DefaultInitializble>() -> NonTrivialDefaults<T> {
  return .indirect_defarg1()
}

