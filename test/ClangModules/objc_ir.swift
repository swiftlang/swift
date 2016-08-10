// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-clang-importer-objc-overlays
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -I %S/Inputs/custom-modules -emit-ir -o - -primary-file %s | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// CHECK: [[B:%CSo1B]] = type

import ObjectiveC
import Foundation
import objc_ext
import TestProtocols
import ObjCIRExtras

// CHECK: @"\01L_selector_data(method:withFloat:)" = private global [18 x i8] c"method:withFloat:\00"
// CHECK: @"\01L_selector_data(method:withDouble:)" = private global [19 x i8] c"method:withDouble:\00"
// CHECK: @"\01L_selector_data(method:separateExtMethod:)" = private global [26 x i8] c"method:separateExtMethod:\00", section "__TEXT,__objc_methname,cstring_literals"

// Instance method invocation
// CHECK: define hidden void @_TF7objc_ir15instanceMethodsFCSo1BT_([[B]]*
func instanceMethods(_ b: B) {
  // CHECK: load i8*, i8** @"\01L_selector(method:withFloat:)"
  // CHECK: call i32 bitcast (void ()* @objc_msgSend to i32
  var i = b.method(1, with: 2.5 as Float)
  // CHECK: load i8*, i8** @"\01L_selector(method:withDouble:)"
  // CHECK: call i32 bitcast (void ()* @objc_msgSend to i32
  i = i + b.method(1, with: 2.5 as Double)
}

// CHECK: define hidden void @_TF7objc_ir16extensionMethodsFT1bCSo1B_T_
func extensionMethods(b b: B) {
  // CHECK:      load i8*, i8** @"\01L_selector(method:separateExtMethod:)", align 8
  // CHECK:      [[T0:%.*]] = call i8* bitcast (void ()* @objc_msgSend to i8*
  // CHECK-NEXT: [[T1:%.*]] = call i8* @objc_retainAutoreleasedReturnValue(i8* [[T0]])
  // CHECK-NOT:  [[T0]]
  // CHECK:      [[T1]]
  b.method(1, separateExtMethod:1.5)
}

// CHECK: define hidden void @_TF7objc_ir19initCallToAllocInitFT1iVs5Int32_T_
func initCallToAllocInit(i i: CInt) {
  // CHECK: call {{.*}} @_TFCSo1BCfT3intVs5Int32_GSQS__
 
  B(int: i)
}

// CHECK: linkonce_odr hidden {{.*}} @_TFCSo1BCfT3intVs5Int32_GSQS__
// CHECK: load i8*, i8** @"\01L_selector(allocWithZone:)"
// CHECK: call [[OPAQUE:%.*]]* bitcast (void ()* @objc_msgSend

// Indexed subscripting
// CHECK: define hidden void @_TF7objc_ir19indexedSubscriptingFT1bCSo1B3idxSi1aCSo1A_T_
func indexedSubscripting(b b: B, idx: Int, a: A) {
  // CHECK: load i8*, i8** @"\01L_selector(setObject:atIndexedSubscript:)", align 8
  b[idx] = a

  // CHECK: load i8*, i8** @"\01L_selector(objectAtIndexedSubscript:)"
  var a2 = b[idx] as! A
}

// CHECK: define hidden void @_TF7objc_ir17keyedSubscriptingFT1bCSo1B3idxCSo1A1aS1__T_
func keyedSubscripting(b b: B, idx: A, a: A) {
  // CHECK: load i8*, i8** @"\01L_selector(setObject:forKeyedSubscript:)"
  b[a] = a
  // CHECK: load i8*, i8** @"\01L_selector(objectForKeyedSubscript:)"
  var a2 = b[a] as! A
}

// CHECK: define hidden void @_TF7objc_ir14propertyAccessFT1bCSo1B_T_
func propertyAccess(b b: B) {
   // CHECK: load i8*, i8** @"\01L_selector(counter)"
   // CHECK: load i8*, i8** @"\01L_selector(setCounter:)"
   b.counter = b.counter + 1

   // CHECK: call %swift.type* @_TMaCSo1B()
   // CHECK: bitcast %swift.type* {{%.+}} to %objc_class*
   // CHECK: load i8*, i8** @"\01L_selector(sharedCounter)"
   // CHECK: load i8*, i8** @"\01L_selector(setSharedCounter:)"
   B.sharedCounter = B.sharedCounter + 1
}

// CHECK: define hidden [[B]]* @_TF7objc_ir8downcastFT1aCSo1A_CSo1B(
func downcast(a a: A) -> B {
  // CHECK: [[CLASS:%.*]] = load %objc_class*, %objc_class** @"OBJC_CLASS_REF_$_B"
  // CHECK: [[T0:%.*]] = call %objc_class* @rt_swift_getInitializedObjCClass(%objc_class* [[CLASS]])
  // CHECK: [[T1:%.*]] = bitcast %objc_class* [[T0]] to i8*
  // CHECK: call i8* @swift_dynamicCastObjCClassUnconditional(i8* [[A:%.*]], i8* [[T1]]) [[NOUNWIND:#[0-9]+]]
  return a as! B
}

// CHECK: define hidden void @_TF7objc_ir19almostSubscriptableFT3as1CSo19AlmostSubscriptable1aCSo1A_T_
func almostSubscriptable(as1 as1: AlmostSubscriptable, a: A) {
  as1.objectForKeyedSubscript(a)
}

// CHECK: define hidden void @_TF7objc_ir13protocolTypesFT1aCSo7NSMince1bPSo9NSRuncing__T_(%CSo7NSMince*, %objc_object*) {{.*}} {
func protocolTypes(a a: NSMince, b: NSRuncing) {
  // - (void)eatWith:(id <NSRuncing>)runcer;
  a.eat(with: b)
  // CHECK: [[SEL:%.*]] = load i8*, i8** @"\01L_selector(eatWith:)", align 8
  // CHECK: call void bitcast (void ()* @objc_msgSend to void ([[OPAQUE:%.*]]*, i8*, i8*)*)([[OPAQUE:%.*]]* {{%.*}}, i8* [[SEL]], i8* {{%.*}})
}

// CHECK-LABEL: define hidden void @_TF7objc_ir6getsetFT1pPSo8FooProto__T_(%objc_object*) {{.*}} {
func getset(p p: FooProto) {
  // CHECK: load i8*, i8** @"\01L_selector(bar)"
  // CHECK: load i8*, i8** @"\01L_selector(setBar:)"
  let prop = p.bar
  p.bar = prop
}

// CHECK-LABEL: define hidden %swift.type* @_TF7objc_ir16protocolMetatypeFT1pPSo8FooProto__PMPS0__(%objc_object*) {{.*}} {
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

// CHECK-LABEL: define hidden %swift.type* @_TF7objc_ir27protocolCompositionMetatypeFT1pCS_4Impl_PMPSo12AnotherProtoSo8FooProto_(%C7objc_ir4Impl*) {{.*}} {
func protocolCompositionMetatype(p: Impl) -> (FooProto & AnotherProto).Type {
  // CHECK: = getelementptr inbounds %C7objc_ir4Impl, %C7objc_ir4Impl* %0, i32 0, i32 0, i32 0
  // CHECK-NOT: {{retain|release}}
  // CHECK: [[RAW_RESULT:%.+]] = call i8* @processComboType(i8* {{%.+}})
  // CHECK: [[CASTED_RESULT:%.+]] = bitcast i8* [[RAW_RESULT]] to %objc_class*
  // CHECK: [[SWIFT_RESULT:%.+]] = call %swift.type* @swift_getObjCClassMetadata(%objc_class* [[CASTED_RESULT]])
  // CHECK: call void bitcast (void (%swift.refcounted*)* @rt_swift_release to void (%C7objc_ir4Impl*)*)(%C7objc_ir4Impl* %0)
  // CHECK: ret %swift.type* [[SWIFT_RESULT]]
  let type = processComboType(type(of: p))
  return type
} // CHECK: }

// CHECK-LABEL: define hidden %swift.type* @_TF7objc_ir28protocolCompositionMetatype2FT1pCS_4Impl_PMPSo12AnotherProtoSo8FooProto_(%C7objc_ir4Impl*) {{.*}} {
func protocolCompositionMetatype2(p: Impl) -> (FooProto & AnotherProto).Type {
  // CHECK: = getelementptr inbounds %C7objc_ir4Impl, %C7objc_ir4Impl* %0, i32 0, i32 0, i32 0
  // CHECK-NOT: {{retain|release}}
  // CHECK: [[RAW_RESULT:%.+]] = call i8* @processComboType2(i8* {{%.+}})
  // CHECK: [[CASTED_RESULT:%.+]] = bitcast i8* [[RAW_RESULT]] to %objc_class*
  // CHECK: [[SWIFT_RESULT:%.+]] = call %swift.type* @swift_getObjCClassMetadata(%objc_class* [[CASTED_RESULT]])
  // CHECK: call void bitcast (void (%swift.refcounted*)* @rt_swift_release to void (%C7objc_ir4Impl*)*)(%C7objc_ir4Impl* %0)
  // CHECK: ret %swift.type* [[SWIFT_RESULT]]
  let type = processComboType2(type(of: p))
  return type
} // CHECK: }

// CHECK-LABEL: define hidden void @_TF7objc_ir17pointerPropertiesFCSo14PointerWrapperT_(%CSo14PointerWrapper*) {{.*}} {
func pointerProperties(_ obj: PointerWrapper) {
  // CHECK: load i8*, i8** @"\01L_selector(setVoidPtr:)"
  // CHECK: load i8*, i8** @"\01L_selector(setIntPtr:)"
  // CHECK: load i8*, i8** @"\01L_selector(setIdPtr:)"
  obj.voidPtr = nil as UnsafeMutableRawPointer?
  obj.intPtr = nil as UnsafeMutablePointer?
  obj.idPtr = nil as AutoreleasingUnsafeMutablePointer?
}

// CHECK-LABEL: define hidden void @_TF7objc_ir20customFactoryMethodsFT_T_() {{.*}} {
func customFactoryMethods() {
  // CHECK: call %CSo13SwiftNameTest* @_TTOFCSo13SwiftNameTestCfT10dummyParamT__S_
  // CHECK: call %CSo13SwiftNameTest* @_TTOFCSo13SwiftNameTestCfT2ccGSqP___S_
  _ = SwiftNameTest(dummyParam: ())
  _ = SwiftNameTest(cc: nil)

  // CHECK: load i8*, i8** @"\01L_selector(testZ)"
  // CHECK: load i8*, i8** @"\01L_selector(testY:)"
  // CHECK: load i8*, i8** @"\01L_selector(testX:xx:)"
  _ = SwiftNameTest.zz()
  _ = SwiftNameTest.yy(aa: nil)
  _ = SwiftNameTest.xx(nil, bb: nil)

  do {
    // CHECK: call %CSo18SwiftNameTestError* @_TTOFCSo18SwiftNameTestErrorCfzT5errorT__S_
    // CHECK: call %CSo18SwiftNameTestError* @_TTOFCSo18SwiftNameTestErrorCfzT2aaGSqP__5errorT__S_
    // CHECK: call %CSo18SwiftNameTestError* @_TTOFCSo18SwiftNameTestErrorCfzT2aaGSqP__5errorT_5blockFT_T__S_
    // CHECK: call %CSo18SwiftNameTestError* @_TTOFCSo18SwiftNameTestErrorCfzT5errorT_5blockFT_T__S_
    // CHECK: call %CSo18SwiftNameTestError* @_TTOFCSo18SwiftNameTestErrorCfzT2aaGSqP___S_
    // CHECK: call %CSo18SwiftNameTestError* @_TTOFCSo18SwiftNameTestErrorCfzT2aaGSqP__5blockFT_T__S_
    // CHECK: call %CSo18SwiftNameTestError* @_TTOFCSo18SwiftNameTestErrorCfzT5blockFT_T__S_
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

// CHECK-LABEL: define linkonce_odr hidden %CSo13SwiftNameTest* @_TTOFCSo13SwiftNameTestCfT10dummyParamT__S_
// CHECK: load i8*, i8** @"\01L_selector(b)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden %CSo13SwiftNameTest* @_TTOFCSo13SwiftNameTestCfT2ccGSqP___S_
// CHECK: load i8*, i8** @"\01L_selector(c:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden %CSo18SwiftNameTestError* @_TTOFCSo18SwiftNameTestErrorCfzT5errorT__S_
// CHECK: load i8*, i8** @"\01L_selector(err1:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden %CSo18SwiftNameTestError* @_TTOFCSo18SwiftNameTestErrorCfzT2aaGSqP__5errorT__S_
// CHECK: load i8*, i8** @"\01L_selector(err2:error:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden %CSo18SwiftNameTestError* @_TTOFCSo18SwiftNameTestErrorCfzT2aaGSqP__5errorT_5blockFT_T__S_
// CHECK: load i8*, i8** @"\01L_selector(err3:error:callback:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden %CSo18SwiftNameTestError* @_TTOFCSo18SwiftNameTestErrorCfzT5errorT_5blockFT_T__S_
// CHECK: load i8*, i8** @"\01L_selector(err4:callback:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden %CSo18SwiftNameTestError* @_TTOFCSo18SwiftNameTestErrorCfzT2aaGSqP___S_
// CHECK: load i8*, i8** @"\01L_selector(err5:error:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden %CSo18SwiftNameTestError* @_TTOFCSo18SwiftNameTestErrorCfzT2aaGSqP__5blockFT_T__S_
// CHECK: load i8*, i8** @"\01L_selector(err6:error:callback:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden %CSo18SwiftNameTestError* @_TTOFCSo18SwiftNameTestErrorCfzT5blockFT_T__S_
// CHECK: load i8*, i8** @"\01L_selector(err7:callback:)"
// CHECK: }

// CHECK-LABEL: define hidden void @_TF7objc_ir29customFactoryMethodsInheritedFT_T_() {{.*}} {
func customFactoryMethodsInherited() {
  // CHECK: call %CSo16SwiftNameTestSub* @_TTOFCSo16SwiftNameTestSubCfT10dummyParamT__S_
  // CHECK: call %CSo16SwiftNameTestSub* @_TTOFCSo16SwiftNameTestSubCfT2ccGSqP___S_
  _ = SwiftNameTestSub(dummyParam: ())
  _ = SwiftNameTestSub(cc: nil)

  // CHECK: load i8*, i8** @"\01L_selector(testZ)"
  // CHECK: load i8*, i8** @"\01L_selector(testY:)"
  // CHECK: load i8*, i8** @"\01L_selector(testX:xx:)"
  _ = SwiftNameTestSub.zz()
  _ = SwiftNameTestSub.yy(aa: nil)
  _ = SwiftNameTestSub.xx(nil, bb: nil)

  do {
    // CHECK: call %CSo21SwiftNameTestErrorSub* @_TTOFCSo21SwiftNameTestErrorSubCfzT5errorT__S_
    // CHECK: call %CSo21SwiftNameTestErrorSub* @_TTOFCSo21SwiftNameTestErrorSubCfzT2aaGSqP__5errorT__S_
    // CHECK: call %CSo21SwiftNameTestErrorSub* @_TTOFCSo21SwiftNameTestErrorSubCfzT2aaGSqP__5errorT_5blockFT_T__S_
    // CHECK: call %CSo21SwiftNameTestErrorSub* @_TTOFCSo21SwiftNameTestErrorSubCfzT5errorT_5blockFT_T__S_
    // CHECK: call %CSo21SwiftNameTestErrorSub* @_TTOFCSo21SwiftNameTestErrorSubCfzT2aaGSqP___S_
    // CHECK: call %CSo21SwiftNameTestErrorSub* @_TTOFCSo21SwiftNameTestErrorSubCfzT2aaGSqP__5blockFT_T__S_
    // CHECK: call %CSo21SwiftNameTestErrorSub* @_TTOFCSo21SwiftNameTestErrorSubCfzT5blockFT_T__S_
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

// CHECK-LABEL: define linkonce_odr hidden %CSo16SwiftNameTestSub* @_TTOFCSo16SwiftNameTestSubCfT10dummyParamT__S_
// CHECK: load i8*, i8** @"\01L_selector(b)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden %CSo16SwiftNameTestSub* @_TTOFCSo16SwiftNameTestSubCfT2ccGSqP___S_
// CHECK: load i8*, i8** @"\01L_selector(c:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden %CSo21SwiftNameTestErrorSub* @_TTOFCSo21SwiftNameTestErrorSubCfzT5errorT__S_
// CHECK: load i8*, i8** @"\01L_selector(err1:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden %CSo21SwiftNameTestErrorSub* @_TTOFCSo21SwiftNameTestErrorSubCfzT2aaGSqP__5errorT__S_
// CHECK: load i8*, i8** @"\01L_selector(err2:error:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden %CSo21SwiftNameTestErrorSub* @_TTOFCSo21SwiftNameTestErrorSubCfzT2aaGSqP__5errorT_5blockFT_T__S_
// CHECK: load i8*, i8** @"\01L_selector(err3:error:callback:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden %CSo21SwiftNameTestErrorSub* @_TTOFCSo21SwiftNameTestErrorSubCfzT5errorT_5blockFT_T__S_
// CHECK: load i8*, i8** @"\01L_selector(err4:callback:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden %CSo21SwiftNameTestErrorSub* @_TTOFCSo21SwiftNameTestErrorSubCfzT2aaGSqP___S_
// CHECK: load i8*, i8** @"\01L_selector(err5:error:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden %CSo21SwiftNameTestErrorSub* @_TTOFCSo21SwiftNameTestErrorSubCfzT2aaGSqP__5blockFT_T__S_
// CHECK: load i8*, i8** @"\01L_selector(err6:error:callback:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden %CSo21SwiftNameTestErrorSub* @_TTOFCSo21SwiftNameTestErrorSubCfzT5blockFT_T__S_
// CHECK: load i8*, i8** @"\01L_selector(err7:callback:)"
// CHECK: }

// CHECK: linkonce_odr hidden {{.*}} @_TTOFCSo1BcfT3intVs5Int32_GSQS__
// CHECK: load i8*, i8** @"\01L_selector(initWithInt:)"
// CHECK: call [[OPAQUE:%.*]]* bitcast (void ()* @objc_msgSend


// CHECK: attributes [[NOUNWIND]] = { nounwind }

