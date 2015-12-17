// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s
// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s --check-prefix=GUARANTEED

func test_type_lowering(x: ErrorProtocol) { }
// CHECK-LABEL: sil hidden @_TF18boxed_existentials18test_type_loweringFPs13ErrorProtocol_T_ : $@convention(thin) (@owned ErrorProtocol) -> () {
// CHECK:         strong_release %0 : $ErrorProtocol

class Document {}

enum ClericalError: ErrorProtocol {
  case MisplacedDocument(Document)

  var _domain: String { return "" }
  var _code: Int { return 0 }
}

func test_concrete_erasure(x: ClericalError) -> ErrorProtocol {
  return x
}
// CHECK-LABEL: sil hidden @_TF18boxed_existentials21test_concrete_erasureFOS_13ClericalErrorPs13ErrorProtocol_
// CHECK:         [[EXISTENTIAL:%.*]] = alloc_existential_box $ErrorProtocol, $ClericalError
// CHECK:         store %0 to [[EXISTENTIAL]]#1 : $*ClericalError
// CHECK:         return [[EXISTENTIAL]]#0 : $ErrorProtocol

protocol HairType {}

func test_composition_erasure(x: protocol<HairType, ErrorProtocol>) -> ErrorProtocol {
  return x
}
// CHECK-LABEL: sil hidden @_TF18boxed_existentials24test_composition_erasureFPs13ErrorProtocolS_8HairType_PS0__
// CHECK:         [[VALUE_ADDR:%.*]] = open_existential_addr [[OLD_EXISTENTIAL:%.*]] : $*protocol<ErrorProtocol, HairType> to $*[[VALUE_TYPE:@opened\(.*\) protocol<ErrorProtocol, HairType>]]
// CHECK:         [[NEW_EXISTENTIAL:%.*]] = alloc_existential_box $ErrorProtocol, $[[VALUE_TYPE]]
// CHECK:         copy_addr [[VALUE_ADDR]] to [initialization] [[NEW_EXISTENTIAL]]#1
// CHECK:         destroy_addr [[OLD_EXISTENTIAL]]
// CHECK:         return [[NEW_EXISTENTIAL]]#0

protocol HairClass: class {}

func test_class_composition_erasure(x: protocol<HairClass, ErrorProtocol>) -> ErrorProtocol {
  return x
}
// CHECK-LABEL: sil hidden @_TF18boxed_existentials30test_class_composition_erasureFPs13ErrorProtocolS_9HairClass_PS0__
// CHECK:         [[VALUE:%.*]] = open_existential_ref [[OLD_EXISTENTIAL:%.*]] : $protocol<ErrorProtocol, HairClass> to $[[VALUE_TYPE:@opened\(.*\) protocol<ErrorProtocol, HairClass>]]
// CHECK:         [[NEW_EXISTENTIAL:%.*]] = alloc_existential_box $ErrorProtocol, $[[VALUE_TYPE]]
// CHECK:         store [[VALUE]] to [[NEW_EXISTENTIAL]]#1
// CHECK:         return [[NEW_EXISTENTIAL]]#0

func test_property(x: ErrorProtocol) -> String {
  return x._domain
}
// CHECK-LABEL: sil hidden @_TF18boxed_existentials13test_propertyFPs13ErrorProtocol_SS
// CHECK:         [[VALUE:%.*]] = open_existential_box %0 : $ErrorProtocol to $*[[VALUE_TYPE:@opened\(.*\) ErrorProtocol]]
// FIXME: Extraneous copy here
// CHECK-NEXT:    [[COPY:%[0-9]+]] = alloc_stack $[[VALUE_TYPE]]
// CHECK-NEXT:    copy_addr [[VALUE]] to [initialization] [[COPY]]#1 : $*[[VALUE_TYPE]]
// CHECK:         [[METHOD:%.*]] = witness_method $[[VALUE_TYPE]], #ErrorProtocol._domain!getter.1
// -- self parameter of witness is @in_guaranteed; no need to copy since
//    value in box is immutable and box is guaranteed
// CHECK:         [[RESULT:%.*]] = apply [[METHOD]]<[[VALUE_TYPE]]>([[COPY]]#1)
// CHECK:         strong_release %0
// CHECK:         return [[RESULT]]

func test_property_of_lvalue(x: ErrorProtocol) -> String {
  var x = x
  return x._domain
}
// CHECK-LABEL: sil hidden @_TF18boxed_existentials23test_property_of_lvalueFPs13ErrorProtocol_SS
// CHECK:         [[VAR:%.*]] = alloc_box $ErrorProtocol
// CHECK:         store %0 to [[VAR]]#1
// CHECK-NEXT:    [[VALUE_BOX:%.*]] = load [[VAR]]#1
// CHECK-NEXT:    strong_retain [[VALUE_BOX]]
// CHECK-NEXT:    [[VALUE:%.*]] = open_existential_box [[VALUE_BOX]] : $ErrorProtocol to $*[[VALUE_TYPE:@opened\(.*\) ErrorProtocol]]
// CHECK-NEXT:    [[COPY:%.*]] = alloc_stack $[[VALUE_TYPE]]
// CHECK-NEXT:    copy_addr [[VALUE]] to [initialization] [[COPY]]#1
// CHECK-NEXT:    [[METHOD:%.*]] = witness_method $[[VALUE_TYPE]], #ErrorProtocol._domain!getter.1
// CHECK-NEXT:    [[RESULT:%.*]] = apply [[METHOD]]<[[VALUE_TYPE]]>([[COPY]]#1)
// CHECK-NEXT:    destroy_addr [[COPY]]#1
// CHECK-NEXT:    dealloc_stack [[COPY]]#0
// CHECK-NEXT:    strong_release [[VALUE_BOX]]
// CHECK-NEXT:    strong_release [[VAR]]#0
// CHECK-NEXT:    strong_release %0
// CHECK-NEXT:    return [[RESULT]]

extension ErrorProtocol {
  final func extensionMethod() { }
}

// CHECK-LABEL: sil hidden @_TF18boxed_existentials21test_extension_methodFPs13ErrorProtocol_T_
func test_extension_method(error: ErrorProtocol) {
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

func plusOneErrorProtocol() -> ErrorProtocol { }

// CHECK-LABEL: sil hidden @_TF18boxed_existentials31test_open_existential_semanticsFTPs13ErrorProtocol_PS0___T_
// GUARANTEED-LABEL: sil hidden @_TF18boxed_existentials31test_open_existential_semanticsFTPs13ErrorProtocol_PS0___T_
func test_open_existential_semantics(guaranteed: ErrorProtocol,
                                     _ immediate: ErrorProtocol) {
  var immediate = immediate
  // CHECK: [[IMMEDIATE_BOX:%.*]] = alloc_box $ErrorProtocol
  // GUARANTEED: [[IMMEDIATE_BOX:%.*]] = alloc_box $ErrorProtocol

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

  // CHECK: [[IMMEDIATE:%.*]] = load [[IMMEDIATE_BOX]]#1
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

  // GUARANTEED: [[IMMEDIATE:%.*]] = load [[IMMEDIATE_BOX]]#1
  // -- need a retain to guarantee
  // GUARANTEED: strong_retain [[IMMEDIATE]]
  // GUARANTEED: [[VALUE:%.*]] = open_existential_box [[IMMEDIATE]]
  // GUARANTEED: [[METHOD:%.*]] = function_ref
  // GUARANTEED: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // GUARANTEED-NOT: destroy_addr [[VALUE]]
  // -- end the guarantee
  // GUARANTEED: strong_release [[IMMEDIATE]]
  immediate.extensionMethod()

  // CHECK: [[F:%.*]] = function_ref {{.*}}plusOneErrorProtocol
  // CHECK: [[PLUS_ONE:%.*]] = apply [[F]]()
  // CHECK: [[VALUE:%.*]] = open_existential_box [[PLUS_ONE]]
  // CHECK: [[METHOD:%.*]] = function_ref
  // CHECK-NOT: copy_addr
  // CHECK: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // CHECK: strong_release [[PLUS_ONE]]

  // GUARANTEED: [[F:%.*]] = function_ref {{.*}}plusOneErrorProtocol
  // GUARANTEED: [[PLUS_ONE:%.*]] = apply [[F]]()
  // GUARANTEED: [[VALUE:%.*]] = open_existential_box [[PLUS_ONE]]
  // GUARANTEED: [[METHOD:%.*]] = function_ref
  // GUARANTEED: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // GUARANTEED-NOT: destroy_addr [[VALUE]]
  // GUARANTEED: strong_release [[PLUS_ONE]]
  plusOneErrorProtocol().extensionMethod()
}

// CHECK-LABEL: sil hidden @_TF18boxed_existentials14erasure_to_anyFTPs13ErrorProtocol_PS0___P_
// CHECK:       bb0([[OUT:%.*]] : $*protocol<>, [[GUAR:%.*]] : $ErrorProtocol,
func erasure_to_any(guaranteed: ErrorProtocol, _ immediate: ErrorProtocol) -> Any {
  var immediate = immediate
  // CHECK:       [[IMMEDIATE_BOX:%.*]] = alloc_box $ErrorProtocol
  if true {
    // CHECK-NOT: retain [[GUAR]]
    // CHECK:     [[FROM_VALUE:%.*]] = open_existential_box [[GUAR:%.*]]
    // CHECK:     [[TO_VALUE:%.*]] = init_existential_addr [[OUT]]
    // CHECK:     copy_addr [[FROM_VALUE]] to [initialization] [[TO_VALUE]]
    // CHECK-NOT: release [[GUAR]]
    return guaranteed
  } else if true {
    // CHECK:     [[IMMEDIATE:%.*]] = load [[IMMEDIATE_BOX]]
    // CHECK:     retain [[IMMEDIATE]]
    // CHECK:     [[FROM_VALUE:%.*]] = open_existential_box [[IMMEDIATE]]
    // CHECK:     [[TO_VALUE:%.*]] = init_existential_addr [[OUT]]
    // CHECK:     copy_addr [[FROM_VALUE]] to [initialization] [[TO_VALUE]]
    // CHECK:     release [[IMMEDIATE]]
    return immediate
  } else if true {
    // CHECK:     function_ref boxed_existentials.plusOneErrorProtocol
    // CHECK:     [[PLUS_ONE:%.*]] = apply
    // CHECK:     [[FROM_VALUE:%.*]] = open_existential_box [[PLUS_ONE]]
    // CHECK:     [[TO_VALUE:%.*]] = init_existential_addr [[OUT]]
    // CHECK:     copy_addr [[FROM_VALUE]] to [initialization] [[TO_VALUE]]
    // CHECK:     release [[PLUS_ONE]]

    return plusOneErrorProtocol()
  }
}
