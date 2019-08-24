// RUN: %target-swift-emit-silgen %s | %FileCheck %s

// rdar://45671537

class Converter<Input, Output> {
  func convert(input: Input) -> Output {
    fatalError("Has to be overriden")
  }
}

class StringIntConverter: Converter<String, Int> {
  override func convert(input: String) -> Int {
    return 0
  }
}

public func convert(strings: [String]) -> [Int] {
  return strings.map(StringIntConverter().convert)
}

// CHECK-LABEL: sil [ossa] @$s22partial_apply_override7convert7stringsSaySiGSaySSG_tF :
// CHECK:      [[CONVERTER_TYPE:%.*]] = metatype $@thick StringIntConverter.Type
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[ALLOC_CONVERTER:%.*]] = function_ref @$s22partial_apply_override18StringIntConverterCACycfC :
// CHECK-NEXT: [[CONVERTER:%.*]] = apply [[ALLOC_CONVERTER]]([[CONVERTER_TYPE]])
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[CURRY_THUNK:%.*]] = function_ref @$s22partial_apply_override18StringIntConverterC7convert5inputSiSS_tFTc : $@convention(thin) (@guaranteed StringIntConverter) -> @owned @callee_guaranteed (@guaranteed String) -> Int
// CHECK-NEXT: [[CURRY_RESULT:%.*]] = apply [[CURRY_THUNK]]([[CONVERTER]])
// CHECK-NEXT: destroy_value [[CONVERTER]] :

// CHECK-LABEL: sil shared [thunk] [ossa] @$s22partial_apply_override18StringIntConverterC7convert5inputSiSS_tFTc : 
// CHECK-SAME: $@convention(thin) (@guaranteed StringIntConverter) -> @owned @callee_guaranteed (@guaranteed String) -> Int
// CHECK:      [[METHOD:%.*]] = class_method %0 : $StringIntConverter, #StringIntConverter.convert!1 : (StringIntConverter) -> (String) -> Int, $@convention(method) (@in_guaranteed String, @guaranteed StringIntConverter) -> @out Int
// CHECK-NEXT: [[SELF_COPY:%.*]] = copy_value %0 :
// CHECK-NEXT: [[BOUND_METHOD:%.*]] = partial_apply [callee_guaranteed] [[METHOD]]([[SELF_COPY]])
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[THUNK:%.*]] = function_ref @$sSSSiIegnr_SSSiIeggd_TR : $@convention(thin) (@guaranteed String, @guaranteed @callee_guaranteed (@in_guaranteed String) -> @out Int) -> Int 
// CHECK-NEXT: [[REABSTRACTED:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[BOUND_METHOD]])
// CHECK-NEXT: return [[REABSTRACTED]]
