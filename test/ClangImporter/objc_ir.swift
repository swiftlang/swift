// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-clang-importer-objc-overlays
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -I %S/Inputs/custom-modules -emit-ir -o - -primary-file %s | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// CHECK: [[B:%TSo1BC]] = type

import ObjectiveC
import Foundation
import objc_ext
import TestProtocols
import ObjCIRExtras

// CHECK: @"\01L_selector_data(method:withFloat:)" = private global [18 x i8] c"method:withFloat:\00"
// CHECK: @"\01L_selector_data(method:withDouble:)" = private global [19 x i8] c"method:withDouble:\00"
// CHECK: @"\01L_selector_data(method:separateExtMethod:)" = private global [26 x i8] c"method:separateExtMethod:\00", section "__TEXT,__objc_methname,cstring_literals"

// Instance method invocation
// CHECK: define hidden swiftcc void @_T07objc_ir15instanceMethodsySo1BCF([[B]]*
func instanceMethods(_ b: B) {
  // CHECK: load i8*, i8** @"\01L_selector(method:withFloat:)"
  // CHECK: call i32 bitcast (void ()* @objc_msgSend to i32
  var i = b.method(1, with: 2.5 as Float)
  // CHECK: load i8*, i8** @"\01L_selector(method:withDouble:)"
  // CHECK: call i32 bitcast (void ()* @objc_msgSend to i32
  i = i + b.method(1, with: 2.5 as Double)
}

// CHECK: define hidden swiftcc void @_T07objc_ir16extensionMethodsySo1BC1b_tF
func extensionMethods(b b: B) {
  // CHECK:      load i8*, i8** @"\01L_selector(method:separateExtMethod:)", align 8
  // CHECK:      [[T0:%.*]] = call i8* bitcast (void ()* @objc_msgSend to i8*
  // CHECK-NEXT: [[T1:%.*]] = call i8* @objc_retainAutoreleasedReturnValue(i8* [[T0]])
  // CHECK-NOT:  [[T0]]
  // CHECK:      [[T1]]
  b.method(1, separateExtMethod:1.5)
}

// CHECK: define hidden swiftcc void @_T07objc_ir19initCallToAllocInitys5Int32V1i_tF
func initCallToAllocInit(i i: CInt) {
  // CHECK: call {{.*}} @_T0So1BCSQyABGs5Int32V3int_tcfC
 
  B(int: i)
}

// CHECK: linkonce_odr hidden {{.*}} @_T0So1BCSQyABGs5Int32V3int_tcfC
// CHECK: call [[OPAQUE:%.*]]* @objc_allocWithZone

// Indexed subscripting
// CHECK: define hidden swiftcc void @_T07objc_ir19indexedSubscriptingySo1BC1b_Si3idxSo1AC1atF
func indexedSubscripting(b b: B, idx: Int, a: A) {
  // CHECK: load i8*, i8** @"\01L_selector(setObject:atIndexedSubscript:)", align 8
  b[idx] = a

  // CHECK: load i8*, i8** @"\01L_selector(objectAtIndexedSubscript:)"
  var a2 = b[idx] as! A
}

// CHECK: define hidden swiftcc void @_T07objc_ir17keyedSubscriptingySo1BC1b_So1AC3idxAG1atF
func keyedSubscripting(b b: B, idx: A, a: A) {
  // CHECK: load i8*, i8** @"\01L_selector(setObject:forKeyedSubscript:)"
  b[a] = a
  // CHECK: load i8*, i8** @"\01L_selector(objectForKeyedSubscript:)"
  var a2 = b[a] as! A
}

// CHECK: define hidden swiftcc void @_T07objc_ir14propertyAccessySo1BC1b_tF
func propertyAccess(b b: B) {
   // CHECK: load i8*, i8** @"\01L_selector(counter)"
   // CHECK: load i8*, i8** @"\01L_selector(setCounter:)"
   b.counter = b.counter + 1

   // CHECK: call %swift.type* @_T0So1BCMa()
   // CHECK: bitcast %swift.type* {{%.+}} to %objc_class*
   // CHECK: load i8*, i8** @"\01L_selector(sharedCounter)"
   // CHECK: load i8*, i8** @"\01L_selector(setSharedCounter:)"
   B.sharedCounter = B.sharedCounter + 1
}

// CHECK: define hidden swiftcc [[B]]* @_T07objc_ir8downcastSo1BCSo1AC1a_tF(
func downcast(a a: A) -> B {
  // CHECK: [[CLASS:%.*]] = load %objc_class*, %objc_class** @"OBJC_CLASS_REF_$_B"
  // CHECK: [[T0:%.*]] = call %objc_class* @swift_rt_swift_getInitializedObjCClass(%objc_class* [[CLASS]])
  // CHECK: [[T1:%.*]] = bitcast %objc_class* [[T0]] to i8*
  // CHECK: call i8* @swift_dynamicCastObjCClassUnconditional(i8* [[A:%.*]], i8* [[T1]]) [[NOUNWIND:#[0-9]+]]
  return a as! B
}

// CHECK: define hidden swiftcc void @_T07objc_ir19almostSubscriptableySo06AlmostD0C3as1_So1AC1atF
func almostSubscriptable(as1 as1: AlmostSubscriptable, a: A) {
  as1.objectForKeyedSubscript(a)
}

// CHECK: define hidden swiftcc void @_T07objc_ir13protocolTypesySo7NSMinceC1a_So9NSRuncing_p1btF(%TSo7NSMinceC*, %objc_object*) {{.*}} {
func protocolTypes(a a: NSMince, b: NSRuncing) {
  // - (void)eatWith:(id <NSRuncing>)runcer;
  a.eat(with: b)
  // CHECK: [[SEL:%.*]] = load i8*, i8** @"\01L_selector(eatWith:)", align 8
  // CHECK: call void bitcast (void ()* @objc_msgSend to void ([[OPAQUE:%.*]]*, i8*, i8*)*)([[OPAQUE:%.*]]* {{%.*}}, i8* [[SEL]], i8* {{%.*}})
}

// CHECK-LABEL: define hidden swiftcc void @_T07objc_ir6getsetySo8FooProto_p1p_tF(%objc_object*) {{.*}} {
func getset(p p: FooProto) {
  // CHECK: load i8*, i8** @"\01L_selector(bar)"
  // CHECK: load i8*, i8** @"\01L_selector(setBar:)"
  let prop = p.bar
  p.bar = prop
}

// CHECK-LABEL: define hidden swiftcc %swift.type* @_T07objc_ir16protocolMetatypeSo8FooProto_pXpSoAC_p1p_tF(%objc_object*) {{.*}} {
func protocolMetatype(p: FooProto) -> FooProto.Type {
  // CHECK: = call %swift.type* @swift_getObjectType(%objc_object* %0)
  // CHECK-NOT: {{retain|release}}
  // CHECK: [[RAW_RESULT:%.+]] = call i8* @processFooType(i8* {{%.+}})
  // CHECK: [[CASTED_RESULT:%.+]] = bitcast i8* [[RAW_RESULT]] to %objc_class*
  // CHECK: [[SWIFT_RESULT:%.+]] = call %swift.type* @swift_getObjCClassMetadata(%objc_class* [[CASTED_RESULT]])
  // CHECK: call void @swift_unknownRelease(%objc_object* %0)
  // CHECK: ret %swift.type* [[SWIFT_RESULT]]
  let type = processFooType(type(of: p))
  return type
} // CHECK: }

class Impl: FooProto, AnotherProto {
  @objc var bar: Int32 = 0
}

// CHECK-LABEL: define hidden swiftcc %swift.type* @_T07objc_ir27protocolCompositionMetatypeSo12AnotherProto_So03FooG0pXpAA4ImplC1p_tF(%T7objc_ir4ImplC*) {{.*}} {
func protocolCompositionMetatype(p: Impl) -> (FooProto & AnotherProto).Type {
  // CHECK: = getelementptr inbounds %T7objc_ir4ImplC, %T7objc_ir4ImplC* %0, i32 0, i32 0, i32 0
  // CHECK-NOT: {{retain|release}}
  // CHECK: [[RAW_RESULT:%.+]] = call i8* @processComboType(i8* {{%.+}})
  // CHECK: [[CASTED_RESULT:%.+]] = bitcast i8* [[RAW_RESULT]] to %objc_class*
  // CHECK: [[SWIFT_RESULT:%.+]] = call %swift.type* @swift_getObjCClassMetadata(%objc_class* [[CASTED_RESULT]])
  // CHECK: call void bitcast (void (%swift.refcounted*)* @swift_rt_swift_release to void (%T7objc_ir4ImplC*)*)(%T7objc_ir4ImplC* %0)
  // CHECK: ret %swift.type* [[SWIFT_RESULT]]
  let type = processComboType(type(of: p))
  return type
} // CHECK: }

// CHECK-LABEL: define hidden swiftcc %swift.type* @_T07objc_ir28protocolCompositionMetatype2So12AnotherProto_So03FooG0pXpAA4ImplC1p_tF(%T7objc_ir4ImplC*) {{.*}} {
func protocolCompositionMetatype2(p: Impl) -> (FooProto & AnotherProto).Type {
  // CHECK: = getelementptr inbounds %T7objc_ir4ImplC, %T7objc_ir4ImplC* %0, i32 0, i32 0, i32 0
  // CHECK-NOT: {{retain|release}}
  // CHECK: [[RAW_RESULT:%.+]] = call i8* @processComboType2(i8* {{%.+}})
  // CHECK: [[CASTED_RESULT:%.+]] = bitcast i8* [[RAW_RESULT]] to %objc_class*
  // CHECK: [[SWIFT_RESULT:%.+]] = call %swift.type* @swift_getObjCClassMetadata(%objc_class* [[CASTED_RESULT]])
  // CHECK: call void bitcast (void (%swift.refcounted*)* @swift_rt_swift_release to void (%T7objc_ir4ImplC*)*)(%T7objc_ir4ImplC* %0)
  // CHECK: ret %swift.type* [[SWIFT_RESULT]]
  let type = processComboType2(type(of: p))
  return type
} // CHECK: }

// CHECK-LABEL: define hidden swiftcc void @_T07objc_ir17pointerPropertiesySo14PointerWrapperCF(%TSo14PointerWrapperC*) {{.*}} {
func pointerProperties(_ obj: PointerWrapper) {
  // CHECK: load i8*, i8** @"\01L_selector(setVoidPtr:)"
  // CHECK: load i8*, i8** @"\01L_selector(setIntPtr:)"
  // CHECK: load i8*, i8** @"\01L_selector(setIdPtr:)"
  obj.voidPtr = nil as UnsafeMutableRawPointer?
  obj.intPtr = nil as UnsafeMutablePointer?
  obj.idPtr = nil as AutoreleasingUnsafeMutablePointer?
}

// CHECK-LABEL: define hidden swiftcc void @_T07objc_ir16strangeSelectorsySo13SwiftNameTestCF(%TSo13SwiftNameTestC*) {{.*}} {
func strangeSelectors(_ obj: SwiftNameTest) {
  // CHECK: load i8*, i8** @"\01L_selector(:b:)"
  obj.empty(a: 0, b: 0)
}

// CHECK-LABEL: define hidden swiftcc void @_T07objc_ir20customFactoryMethodsyyF() {{.*}} {
func customFactoryMethods() {
  // CHECK: call swiftcc %TSo13SwiftNameTestC* @_T0So13SwiftNameTestCAByt10dummyParam_tcfCTO
  // CHECK: call swiftcc %TSo13SwiftNameTestC* @_T0So13SwiftNameTestCABypSg2cc_tcfCTO
  // CHECK: call swiftcc %TSo13SwiftNameTestC* @_T0So13SwiftNameTestCABs5Int32V5empty_tcfCTO
  _ = SwiftNameTest(dummyParam: ())
  _ = SwiftNameTest(cc: nil)
  _ = SwiftNameTest(empty: 0)

  // CHECK: load i8*, i8** @"\01L_selector(testZ)"
  // CHECK: load i8*, i8** @"\01L_selector(testY:)"
  // CHECK: load i8*, i8** @"\01L_selector(testX:xx:)"
  // CHECK: load i8*, i8** @"\01L_selector(::)"
  _ = SwiftNameTest.zz()
  _ = SwiftNameTest.yy(aa: nil)
  _ = SwiftNameTest.xx(nil, bb: nil)
  _ = SwiftNameTest.empty(1, 2)

  do {
    // CHECK: call swiftcc %TSo18SwiftNameTestErrorC* @_T0So18SwiftNameTestErrorCAByt5error_tKcfCTO
    // CHECK: call swiftcc %TSo18SwiftNameTestErrorC* @_T0So18SwiftNameTestErrorCABypSg2aa_yt5errortKcfCTO
    // CHECK: call swiftcc %TSo18SwiftNameTestErrorC* @_T0So18SwiftNameTestErrorCABypSg2aa_yt5erroryyc5blocktKcfCTO
    // CHECK: call swiftcc %TSo18SwiftNameTestErrorC* @_T0So18SwiftNameTestErrorCAByt5error_yyc5blocktKcfCTO
    // CHECK: call swiftcc %TSo18SwiftNameTestErrorC* @_T0So18SwiftNameTestErrorCABypSg2aa_tKcfCTO
    // CHECK: call swiftcc %TSo18SwiftNameTestErrorC* @_T0So18SwiftNameTestErrorCABypSg2aa_yyc5blocktKcfCTO
    // CHECK: call swiftcc %TSo18SwiftNameTestErrorC* @_T0So18SwiftNameTestErrorCAByyc5block_tKcfCTO
    _ = try SwiftNameTestError(error: ())
    _ = try SwiftNameTestError(aa: nil, error: ())
    _ = try SwiftNameTestError(aa: nil, error: (), block: {})
    _ = try SwiftNameTestError(error: (), block: {})

    _ = try SwiftNameTestError(aa: nil)
    _ = try SwiftNameTestError(aa: nil, block: {})
    _ = try SwiftNameTestError(block: {})

    // CHECK: load i8*, i8** @"\01L_selector(testW:error:)"
    // CHECK: load i8*, i8** @"\01L_selector(testW2:error:)"
    // CHECK: load i8*, i8** @"\01L_selector(testV:)"
    // CHECK: load i8*, i8** @"\01L_selector(testV2:)"
    _ = try SwiftNameTestError.ww(nil)
    _ = try SwiftNameTestError.w2(nil, error: ())
    _ = try SwiftNameTestError.vv()
    _ = try SwiftNameTestError.v2(error: ())
  } catch _ {
  }
}

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo13SwiftNameTestC* @_T0So13SwiftNameTestCAByt10dummyParam_tcfCTO
// CHECK: load i8*, i8** @"\01L_selector(b)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo13SwiftNameTestC* @_T0So13SwiftNameTestCABypSg2cc_tcfCTO
// CHECK: load i8*, i8** @"\01L_selector(c:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo18SwiftNameTestErrorC* @_T0So18SwiftNameTestErrorCAByt5error_tKcfCTO
// CHECK: load i8*, i8** @"\01L_selector(err1:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo18SwiftNameTestErrorC* @_T0So18SwiftNameTestErrorCABypSg2aa_yt5errortKcfCTO
// CHECK: load i8*, i8** @"\01L_selector(err2:error:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo18SwiftNameTestErrorC* @_T0So18SwiftNameTestErrorCABypSg2aa_yt5erroryyc5blocktKcfCTO
// CHECK: load i8*, i8** @"\01L_selector(err3:error:callback:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo18SwiftNameTestErrorC* @_T0So18SwiftNameTestErrorCAByt5error_yyc5blocktKcfCTO
// CHECK: load i8*, i8** @"\01L_selector(err4:callback:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo18SwiftNameTestErrorC* @_T0So18SwiftNameTestErrorCABypSg2aa_tKcfCTO
// CHECK: load i8*, i8** @"\01L_selector(err5:error:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo18SwiftNameTestErrorC* @_T0So18SwiftNameTestErrorCABypSg2aa_yyc5blocktKcfCTO
// CHECK: load i8*, i8** @"\01L_selector(err6:error:callback:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo18SwiftNameTestErrorC* @_T0So18SwiftNameTestErrorCAByyc5block_tKcfCTO
// CHECK: load i8*, i8** @"\01L_selector(err7:callback:)"
// CHECK: }

// CHECK-LABEL: define hidden swiftcc void @_T07objc_ir29customFactoryMethodsInheritedyyF() {{.*}} {
func customFactoryMethodsInherited() {
  // CHECK: call swiftcc %TSo16SwiftNameTestSubC* @_T0So16SwiftNameTestSubCAByt10dummyParam_tcfCTO
  // CHECK: call swiftcc %TSo16SwiftNameTestSubC* @_T0So16SwiftNameTestSubCABypSg2cc_tcfCTO
  _ = SwiftNameTestSub(dummyParam: ())
  _ = SwiftNameTestSub(cc: nil)

  // CHECK: load i8*, i8** @"\01L_selector(testZ)"
  // CHECK: load i8*, i8** @"\01L_selector(testY:)"
  // CHECK: load i8*, i8** @"\01L_selector(testX:xx:)"
  _ = SwiftNameTestSub.zz()
  _ = SwiftNameTestSub.yy(aa: nil)
  _ = SwiftNameTestSub.xx(nil, bb: nil)

  do {
    // CHECK: call swiftcc %TSo21SwiftNameTestErrorSubC* @_T0So21SwiftNameTestErrorSubCAByt5error_tKcfCTO
    // CHECK: call swiftcc %TSo21SwiftNameTestErrorSubC* @_T0So21SwiftNameTestErrorSubCABypSg2aa_yt5errortKcfCTO
    // CHECK: call swiftcc %TSo21SwiftNameTestErrorSubC* @_T0So21SwiftNameTestErrorSubCABypSg2aa_yt5erroryyc5blocktKcfCTO
    // CHECK: call swiftcc %TSo21SwiftNameTestErrorSubC* @_T0So21SwiftNameTestErrorSubCAByt5error_yyc5blocktKcfCTO
    // CHECK: call swiftcc %TSo21SwiftNameTestErrorSubC* @_T0So21SwiftNameTestErrorSubCABypSg2aa_tKcfCTO
    // CHECK: call swiftcc %TSo21SwiftNameTestErrorSubC* @_T0So21SwiftNameTestErrorSubCABypSg2aa_yyc5blocktKcfCTO
    // CHECK: call swiftcc %TSo21SwiftNameTestErrorSubC* @_T0So21SwiftNameTestErrorSubCAByyc5block_tKcfCTO
    _ = try SwiftNameTestErrorSub(error: ())
    _ = try SwiftNameTestErrorSub(aa: nil, error: ())
    _ = try SwiftNameTestErrorSub(aa: nil, error: (), block: {})
    _ = try SwiftNameTestErrorSub(error: (), block: {})

    _ = try SwiftNameTestErrorSub(aa: nil)
    _ = try SwiftNameTestErrorSub(aa: nil, block: {})
    _ = try SwiftNameTestErrorSub(block: {})

    // CHECK: load i8*, i8** @"\01L_selector(testW:error:)"
    // CHECK: load i8*, i8** @"\01L_selector(testW2:error:)"
    // CHECK: load i8*, i8** @"\01L_selector(testV:)"
    // CHECK: load i8*, i8** @"\01L_selector(testV2:)"
    _ = try SwiftNameTestErrorSub.ww(nil)
    _ = try SwiftNameTestErrorSub.w2(nil, error: ())
    _ = try SwiftNameTestErrorSub.vv()
    _ = try SwiftNameTestErrorSub.v2(error: ())
  } catch _ {
  }
}

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo16SwiftNameTestSubC* @_T0So16SwiftNameTestSubCAByt10dummyParam_tcfCTO
// CHECK: load i8*, i8** @"\01L_selector(b)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo16SwiftNameTestSubC* @_T0So16SwiftNameTestSubCABypSg2cc_tcfCTO
// CHECK: load i8*, i8** @"\01L_selector(c:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo21SwiftNameTestErrorSubC* @_T0So21SwiftNameTestErrorSubCAByt5error_tKcfCTO
// CHECK: load i8*, i8** @"\01L_selector(err1:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo21SwiftNameTestErrorSubC* @_T0So21SwiftNameTestErrorSubCABypSg2aa_yt5errortKcfCTO
// CHECK: load i8*, i8** @"\01L_selector(err2:error:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo21SwiftNameTestErrorSubC* @_T0So21SwiftNameTestErrorSubCABypSg2aa_yt5erroryyc5blocktKcfCTO
// CHECK: load i8*, i8** @"\01L_selector(err3:error:callback:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo21SwiftNameTestErrorSubC* @_T0So21SwiftNameTestErrorSubCAByt5error_yyc5blocktKcfCTO
// CHECK: load i8*, i8** @"\01L_selector(err4:callback:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo21SwiftNameTestErrorSubC* @_T0So21SwiftNameTestErrorSubCABypSg2aa_tKcfCTO
// CHECK: load i8*, i8** @"\01L_selector(err5:error:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo21SwiftNameTestErrorSubC* @_T0So21SwiftNameTestErrorSubCABypSg2aa_yyc5blocktKcfCTO
// CHECK: load i8*, i8** @"\01L_selector(err6:error:callback:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo21SwiftNameTestErrorSubC* @_T0So21SwiftNameTestErrorSubCAByyc5block_tKcfCTO
// CHECK: load i8*, i8** @"\01L_selector(err7:callback:)"
// CHECK: }

// CHECK: linkonce_odr hidden {{.*}} @_T0So1BCSQyABGs5Int32V3int_tcfcTO
// CHECK: load i8*, i8** @"\01L_selector(initWithInt:)"
// CHECK: call [[OPAQUE:%.*]]* bitcast (void ()* @objc_msgSend


// CHECK: attributes [[NOUNWIND]] = { nounwind }

