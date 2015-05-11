// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s
// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s --check-prefix=GUARANTEED

func test_type_lowering(x: ErrorType) { }
// CHECK-LABEL: sil hidden @_TF18boxed_existentials18test_type_loweringFPSs9ErrorType_T_ : $@convention(thin) (@owned ErrorType) -> () {
// CHECK:         strong_release %0 : $ErrorType

class Document {}

enum ClericalError: ErrorType {
  case MisplacedDocument(Document)

  var domain: String { return "" }
  var code: Int { return 0 }
}

func test_concrete_erasure(x: ClericalError) -> ErrorType {
  return x
}
// CHECK-LABEL: sil hidden @_TF18boxed_existentials21test_concrete_erasureFOS_13ClericalErrorPSs9ErrorType_
// CHECK:         [[EXISTENTIAL:%.*]] = alloc_existential_box $ErrorType, $ClericalError
// CHECK:         store %0 to [[EXISTENTIAL]]#1 : $*ClericalError
// CHECK:         return [[EXISTENTIAL]]#0 : $ErrorType

protocol HairType {}

func test_composition_erasure(x: protocol<HairType, ErrorType>) -> ErrorType {
  return x
}
// CHECK-LABEL: sil hidden @_TF18boxed_existentials24test_composition_erasureFPSs9ErrorTypeS_8HairType_PS0__
// CHECK:         [[VALUE_ADDR:%.*]] = open_existential_addr [[OLD_EXISTENTIAL:%.*]] : $*protocol<ErrorType, HairType> to $*[[VALUE_TYPE:@opened\(.*\) protocol<ErrorType, HairType>]]
// CHECK:         [[NEW_EXISTENTIAL:%.*]] = alloc_existential_box $ErrorType, $[[VALUE_TYPE]]
// CHECK:         copy_addr [take] [[VALUE_ADDR]] to [initialization] [[NEW_EXISTENTIAL]]#1
// CHECK:         deinit_existential_addr [[OLD_EXISTENTIAL]]
// CHECK:         return [[NEW_EXISTENTIAL]]#0

protocol HairClassType: class {}

func test_class_composition_erasure(x: protocol<HairClassType, ErrorType>) -> ErrorType {
  return x
}
// CHECK-LABEL: sil hidden @_TF18boxed_existentials30test_class_composition_erasureFPSs9ErrorTypeS_13HairClassType_PS0__
// CHECK:         [[VALUE:%.*]] = open_existential_ref [[OLD_EXISTENTIAL:%.*]] : $protocol<ErrorType, HairClassType> to $[[VALUE_TYPE:@opened\(.*\) protocol<ErrorType, HairClassType>]]
// CHECK:         [[NEW_EXISTENTIAL:%.*]] = alloc_existential_box $ErrorType, $[[VALUE_TYPE]]
// CHECK:         store [[VALUE]] to [[NEW_EXISTENTIAL]]#1
// CHECK:         return [[NEW_EXISTENTIAL]]#0

func test_property(x: ErrorType) -> String {
  return x.domain
}
// CHECK-LABEL: sil hidden @_TF18boxed_existentials13test_propertyFPSs9ErrorType_SS
// CHECK:         [[VALUE:%.*]] = open_existential_box %0 : $ErrorType to $*[[VALUE_TYPE:@opened\(.*\) ErrorType]]
// FIXME: Extraneous copy here
// CHECK-NEXT: [[COPY:%[0-9]+]] = alloc_stack $[[VALUE_TYPE]]
// CHECK-NEXT: copy_addr [[VALUE]] to [initialization] [[COPY]]#1 : $*[[VALUE_TYPE]]
// CHECK:         [[METHOD:%.*]] = witness_method $[[VALUE_TYPE]], #ErrorType.domain!getter.1
// -- self parameter of witness is @in_guaranteed; no need to copy since
//    value in box is immutable and box is guaranteed
// CHECK:         [[RESULT:%.*]] = apply [[METHOD]]<[[VALUE_TYPE]]>([[COPY]]#1)
// CHECK:         strong_release %0
// CHECK:         return [[RESULT]]

extension ErrorType {
  final func extensionMethod() { }
}

// CHECK-LABEL: sil hidden @_TF18boxed_existentials21test_extension_methodFPSs9ErrorType_T_
func test_extension_method(error: ErrorType) {
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

func plusOneErrorType() -> ErrorType { }

// CHECK-LABEL: sil hidden @_TF18boxed_existentials31test_open_existential_semanticsFTPSs9ErrorType_PS0___T_
// GUARANTEED-LABEL: sil hidden @_TF18boxed_existentials31test_open_existential_semanticsFTPSs9ErrorType_PS0___T_
func test_open_existential_semantics(guaranteed: ErrorType,
                                     var _ immediate: ErrorType) {
  // CHECK: [[IMMEDIATE_BOX:%.*]] = alloc_box $ErrorType
  // GUARANTEED: [[IMMEDIATE_BOX:%.*]] = alloc_box $ErrorType

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

  // CHECK: [[F:%.*]] = function_ref {{.*}}plusOneErrorType
  // CHECK: [[PLUS_ONE:%.*]] = apply [[F]]()
  // CHECK: [[VALUE:%.*]] = open_existential_box [[PLUS_ONE]]
  // CHECK: [[METHOD:%.*]] = function_ref
  // CHECK-NOT: copy_addr
  // CHECK: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // CHECK: strong_release [[PLUS_ONE]]

  // GUARANTEED: [[F:%.*]] = function_ref {{.*}}plusOneErrorType
  // GUARANTEED: [[PLUS_ONE:%.*]] = apply [[F]]()
  // GUARANTEED: [[VALUE:%.*]] = open_existential_box [[PLUS_ONE]]
  // GUARANTEED: [[METHOD:%.*]] = function_ref
  // GUARANTEED: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // GUARANTEED-NOT: destroy_addr [[VALUE]]
  // GUARANTEED: strong_release [[PLUS_ONE]]
  plusOneErrorType().extensionMethod()
}
