// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -emit-silgen %s | %FileCheck %s
// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -emit-silgen %s | %FileCheck %s --check-prefix=GUARANTEED

func test_type_lowering(_ x: Error) { }
// CHECK-LABEL: sil hidden @_TF18boxed_existentials18test_type_loweringFPs5Error_T_ : $@convention(thin) (@owned Error) -> () {
// CHECK:         strong_release %0 : $Error

class Document {}

enum ClericalError: Error {
  case MisplacedDocument(Document)

  var _domain: String { return "" }
  var _code: Int { return 0 }
}

func test_concrete_erasure(_ x: ClericalError) -> Error {
  return x
}
// CHECK-LABEL: sil hidden @_TF18boxed_existentials21test_concrete_erasureFOS_13ClericalErrorPs5Error_
// CHECK:         [[EXISTENTIAL:%.*]] = alloc_existential_box $Error, $ClericalError
// CHECK:         [[ADDR:%.*]] = project_existential_box $ClericalError in [[EXISTENTIAL]] : $Error
// CHECK:         store %0 to [[ADDR]] : $*ClericalError
// CHECK:         return [[EXISTENTIAL]] : $Error

protocol HairType {}

func test_composition_erasure(_ x: HairType & Error) -> Error {
  return x
}
// CHECK-LABEL: sil hidden @_TF18boxed_existentials24test_composition_erasureFPs5ErrorS_8HairType_PS0__
// CHECK:         [[VALUE_ADDR:%.*]] = open_existential_addr [[OLD_EXISTENTIAL:%.*]] : $*Error & HairType to $*[[VALUE_TYPE:@opened\(.*\) Error & HairType]]
// CHECK:         [[NEW_EXISTENTIAL:%.*]] = alloc_existential_box $Error, $[[VALUE_TYPE]]
// CHECK:         [[ADDR:%.*]] = project_existential_box $[[VALUE_TYPE]] in [[NEW_EXISTENTIAL]] : $Error
// CHECK:         copy_addr [[VALUE_ADDR]] to [initialization] [[ADDR]]
// CHECK:         destroy_addr [[OLD_EXISTENTIAL]]
// CHECK:         return [[NEW_EXISTENTIAL]]

protocol HairClass: class {}

func test_class_composition_erasure(_ x: HairClass & Error) -> Error {
  return x
}
// CHECK-LABEL: sil hidden @_TF18boxed_existentials30test_class_composition_erasureFPs5ErrorS_9HairClass_PS0__
// CHECK:         [[VALUE:%.*]] = open_existential_ref [[OLD_EXISTENTIAL:%.*]] : $Error & HairClass to $[[VALUE_TYPE:@opened\(.*\) Error & HairClass]]
// CHECK:         [[NEW_EXISTENTIAL:%.*]] = alloc_existential_box $Error, $[[VALUE_TYPE]]
// CHECK:         [[ADDR:%.*]] = project_existential_box $[[VALUE_TYPE]] in [[NEW_EXISTENTIAL]] : $Error
// CHECK:         store [[VALUE]] to [[ADDR]]
// CHECK:         return [[NEW_EXISTENTIAL]]

func test_property(_ x: Error) -> String {
  return x._domain
}
// CHECK-LABEL: sil hidden @_TF18boxed_existentials13test_propertyFPs5Error_SS
// CHECK:         [[VALUE:%.*]] = open_existential_box %0 : $Error to $*[[VALUE_TYPE:@opened\(.*\) Error]]
// FIXME: Extraneous copy here
// CHECK-NEXT:    [[COPY:%[0-9]+]] = alloc_stack $[[VALUE_TYPE]]
// CHECK-NEXT:    copy_addr [[VALUE]] to [initialization] [[COPY]] : $*[[VALUE_TYPE]]
// CHECK:         [[METHOD:%.*]] = witness_method $[[VALUE_TYPE]], #Error._domain!getter.1
// -- self parameter of witness is @in_guaranteed; no need to copy since
//    value in box is immutable and box is guaranteed
// CHECK:         [[RESULT:%.*]] = apply [[METHOD]]<[[VALUE_TYPE]]>([[COPY]])
// CHECK:         strong_release %0
// CHECK:         return [[RESULT]]

func test_property_of_lvalue(_ x: Error) -> String {
  var x = x
  return x._domain
}
// CHECK-LABEL: sil hidden @_TF18boxed_existentials23test_property_of_lvalueFPs5Error_SS
// CHECK:         [[VAR:%.*]] = alloc_box $Error
// CHECK-NEXT:    [[PVAR:%.*]] = project_box [[VAR]]
// CHECK-NEXT:    strong_retain %0 : $Error
// CHECK-NEXT:    store %0 to [[PVAR]]
// CHECK-NEXT:    [[VALUE_BOX:%.*]] = load [[PVAR]]
// CHECK-NEXT:    strong_retain [[VALUE_BOX]]
// CHECK-NEXT:    [[VALUE:%.*]] = open_existential_box [[VALUE_BOX]] : $Error to $*[[VALUE_TYPE:@opened\(.*\) Error]]
// CHECK-NEXT:    [[COPY:%.*]] = alloc_stack $[[VALUE_TYPE]]
// CHECK-NEXT:    copy_addr [[VALUE]] to [initialization] [[COPY]]
// CHECK-NEXT:    [[METHOD:%.*]] = witness_method $[[VALUE_TYPE]], #Error._domain!getter.1
// CHECK-NEXT:    [[RESULT:%.*]] = apply [[METHOD]]<[[VALUE_TYPE]]>([[COPY]])
// CHECK-NEXT:    destroy_addr [[COPY]]
// CHECK-NEXT:    dealloc_stack [[COPY]]
// CHECK-NEXT:    strong_release [[VALUE_BOX]]
// CHECK-NEXT:    strong_release [[VAR]]
// CHECK-NEXT:    strong_release %0
// CHECK-NEXT:    return [[RESULT]]

extension Error {
  final func extensionMethod() { }
}

// CHECK-LABEL: sil hidden @_TF18boxed_existentials21test_extension_methodFPs5Error_T_
func test_extension_method(_ error: Error) {
  // CHECK: [[VALUE:%.*]] = open_existential_box %0
  // CHECK: [[METHOD:%.*]] = function_ref
  // CHECK-NOT: copy_addr
  // CHECK: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // CHECK-NOT: destroy_addr [[COPY]]
  // CHECK-NOT: destroy_addr [[VALUE]]
  // CHECK-NOT: destroy_addr [[VALUE]]
  // -- release the owned argument
  // CHECK: strong_release %0
  error.extensionMethod()
}

func plusOneError() -> Error { }

// CHECK-LABEL: sil hidden @_TF18boxed_existentials31test_open_existential_semanticsFTPs5Error_PS0___T_
// GUARANTEED-LABEL: sil hidden @_TF18boxed_existentials31test_open_existential_semanticsFTPs5Error_PS0___T_
func test_open_existential_semantics(_ guaranteed: Error,
                                     _ immediate: Error) {
  var immediate = immediate
  // CHECK: [[IMMEDIATE_BOX:%.*]] = alloc_box $Error
  // CHECK: [[PB:%.*]] = project_box [[IMMEDIATE_BOX]]
  // GUARANTEED: [[IMMEDIATE_BOX:%.*]] = alloc_box $Error
  // GUARANTEED: [[PB:%.*]] = project_box [[IMMEDIATE_BOX]]

  // CHECK-NOT: strong_retain %0
  // CHECK: [[VALUE:%.*]] = open_existential_box %0
  // CHECK: [[METHOD:%.*]] = function_ref
  // CHECK-NOT: copy_addr
  // CHECK: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // CHECK-NOT: strong_release %0

  // GUARANTEED-NOT: strong_retain %0
  // GUARANTEED: [[VALUE:%.*]] = open_existential_box [[GUARANTEED:%0]]
  // GUARANTEED: [[METHOD:%.*]] = function_ref
  // GUARANTEED: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // GUARANTEED-NOT: destroy_addr [[VALUE]]
  // GUARANTEED-NOT: strong_release [[GUARANTEED]]
  guaranteed.extensionMethod()

  // CHECK: [[IMMEDIATE:%.*]] = load [[PB]]
  // -- need a retain to guarantee
  // CHECK: strong_retain [[IMMEDIATE]]
  // CHECK: [[VALUE:%.*]] = open_existential_box [[IMMEDIATE]]
  // CHECK: [[METHOD:%.*]] = function_ref
  // CHECK-NOT: copy_addr
  // CHECK: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // -- end the guarantee
  // -- TODO: could in theory do this sooner, after the value's been copied
  //    out.
  // CHECK: strong_release [[IMMEDIATE]]

  // GUARANTEED: [[IMMEDIATE:%.*]] = load [[PB]]
  // -- need a retain to guarantee
  // GUARANTEED: strong_retain [[IMMEDIATE]]
  // GUARANTEED: [[VALUE:%.*]] = open_existential_box [[IMMEDIATE]]
  // GUARANTEED: [[METHOD:%.*]] = function_ref
  // GUARANTEED: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // GUARANTEED-NOT: destroy_addr [[VALUE]]
  // -- end the guarantee
  // GUARANTEED: strong_release [[IMMEDIATE]]
  immediate.extensionMethod()

  // CHECK: [[F:%.*]] = function_ref {{.*}}plusOneError
  // CHECK: [[PLUS_ONE:%.*]] = apply [[F]]()
  // CHECK: [[VALUE:%.*]] = open_existential_box [[PLUS_ONE]]
  // CHECK: [[METHOD:%.*]] = function_ref
  // CHECK-NOT: copy_addr
  // CHECK: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // CHECK: strong_release [[PLUS_ONE]]

  // GUARANTEED: [[F:%.*]] = function_ref {{.*}}plusOneError
  // GUARANTEED: [[PLUS_ONE:%.*]] = apply [[F]]()
  // GUARANTEED: [[VALUE:%.*]] = open_existential_box [[PLUS_ONE]]
  // GUARANTEED: [[METHOD:%.*]] = function_ref
  // GUARANTEED: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // GUARANTEED-NOT: destroy_addr [[VALUE]]
  // GUARANTEED: strong_release [[PLUS_ONE]]
  plusOneError().extensionMethod()
}

// CHECK-LABEL: sil hidden @_TF18boxed_existentials14erasure_to_anyFTPs5Error_PS0___P_
// CHECK:       bb0([[OUT:%.*]] : $*Any, [[GUAR:%.*]] : $Error,
func erasure_to_any(_ guaranteed: Error, _ immediate: Error) -> Any {
  var immediate = immediate
  // CHECK:       [[IMMEDIATE_BOX:%.*]] = alloc_box $Error
  // CHECK:       [[PB:%.*]] = project_box [[IMMEDIATE_BOX]]
  if true {
    // CHECK-NOT: retain [[GUAR]]
    // CHECK:     [[FROM_VALUE:%.*]] = open_existential_box [[GUAR:%.*]]
    // CHECK:     [[TO_VALUE:%.*]] = init_existential_addr [[OUT]]
    // CHECK:     copy_addr [[FROM_VALUE]] to [initialization] [[TO_VALUE]]
    // CHECK-NOT: release [[GUAR]]
    return guaranteed
  } else if true {
    // CHECK:     [[IMMEDIATE:%.*]] = load [[PB]]
    // CHECK:     retain [[IMMEDIATE]]
    // CHECK:     [[FROM_VALUE:%.*]] = open_existential_box [[IMMEDIATE]]
    // CHECK:     [[TO_VALUE:%.*]] = init_existential_addr [[OUT]]
    // CHECK:     copy_addr [[FROM_VALUE]] to [initialization] [[TO_VALUE]]
    // CHECK:     release [[IMMEDIATE]]
    return immediate
  } else if true {
    // CHECK:     function_ref boxed_existentials.plusOneError
    // CHECK:     [[PLUS_ONE:%.*]] = apply
    // CHECK:     [[FROM_VALUE:%.*]] = open_existential_box [[PLUS_ONE]]
    // CHECK:     [[TO_VALUE:%.*]] = init_existential_addr [[OUT]]
    // CHECK:     copy_addr [[FROM_VALUE]] to [initialization] [[TO_VALUE]]
    // CHECK:     release [[PLUS_ONE]]

    return plusOneError()
  }
}
