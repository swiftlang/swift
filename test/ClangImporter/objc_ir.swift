
// RUN: %empty-directory(%t)
// RUN: %build-clang-importer-objc-overlays
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -module-name objc_ir -I %S/Inputs/custom-modules -emit-ir -g -o - -primary-file %s | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// CHECK: %TSo1BC = type

import ObjectiveC
import Foundation
import objc_ext
import TestProtocols
import ObjCIRExtras

// CHECK: @"\01L_selector_data(method:withFloat:)" = private global [18 x i8] c"method:withFloat:\00"
// CHECK: @"\01L_selector_data(method:withDouble:)" = private global [19 x i8] c"method:withDouble:\00"
// CHECK: @"\01L_selector_data(method:separateExtMethod:)" = private global [26 x i8] c"method:separateExtMethod:\00", section "__TEXT,__objc_methname,cstring_literals"

// Instance method invocation
// CHECK-LABEL: define hidden swiftcc void @"$S7objc_ir15instanceMethodsyySo1BCF"(%TSo1BC*
func instanceMethods(_ b: B) {
  // CHECK: load i8*, i8** @"\01L_selector(method:withFloat:)"
  // CHECK: call i32 bitcast (void ()* @objc_msgSend to i32
  var i = b.method(1, with: 2.5 as Float)
  // CHECK: load i8*, i8** @"\01L_selector(method:withDouble:)"
  // CHECK: call i32 bitcast (void ()* @objc_msgSend to i32
  i = i + b.method(1, with: 2.5 as Double)
}

// CHECK-LABEL: define hidden swiftcc void @"$S7objc_ir16extensionMethods1bySo1BC_tF"
func extensionMethods(b b: B) {
  // CHECK:      load i8*, i8** @"\01L_selector(method:separateExtMethod:)", align 8
  // CHECK:      [[T0:%.*]] = call i8* bitcast (void ()* @objc_msgSend to i8*
  // CHECK-NEXT: [[T1:%.*]] = {{.*}}call i8* @objc_retainAutoreleasedReturnValue(i8* [[T0]])
  // CHECK-NOT:  [[T0]]
  // CHECK:      [[T1]]
  b.method(1, separateExtMethod:1.5)
}

// CHECK-LABEL: define hidden swiftcc void @"$S7objc_ir19initCallToAllocInit1iys5Int32V_tF"
func initCallToAllocInit(i i: CInt) {
  // CHECK: call {{.*}} @"$SSo1BC3intABSgs5Int32V_tcfC"
 
  B(int: i)
}

// CHECK-LABEL: linkonce_odr hidden {{.*}} @"$SSo1BC3intABSgs5Int32V_tcfC"
// CHECK: call [[OPAQUE:%.*]]* @objc_allocWithZone

// Indexed subscripting
// CHECK-LABEL: define hidden swiftcc void @"$S7objc_ir19indexedSubscripting1b3idx1aySo1BC_SiSo1ACtF"
func indexedSubscripting(b b: B, idx: Int, a: A) {
  // CHECK: load i8*, i8** @"\01L_selector(setObject:atIndexedSubscript:)", align 8
  b[idx] = a

  // CHECK: load i8*, i8** @"\01L_selector(objectAtIndexedSubscript:)"
  var a2 = b[idx] as! A
}

// CHECK-LABEL: define hidden swiftcc void @"$S7objc_ir17keyedSubscripting1b3idx1aySo1BC_So1ACAItF"
func keyedSubscripting(b b: B, idx: A, a: A) {
  // CHECK: load i8*, i8** @"\01L_selector(setObject:forKeyedSubscript:)"
  b[a] = a
  // CHECK: load i8*, i8** @"\01L_selector(objectForKeyedSubscript:)"
  var a2 = b[a] as! A
}

// CHECK-LABEL: define hidden swiftcc void @"$S7objc_ir14propertyAccess1bySo1BC_tF"
func propertyAccess(b b: B) {
   // CHECK: load i8*, i8** @"\01L_selector(counter)"
   // CHECK: load i8*, i8** @"\01L_selector(setCounter:)"
   b.counter = b.counter + 1

   // CHECK: load %objc_class*, %objc_class** @"\01l_OBJC_CLASS_REF_$_B"
   // CHECK: load i8*, i8** @"\01L_selector(sharedCounter)"
   // CHECK: load i8*, i8** @"\01L_selector(setSharedCounter:)"
   B.sharedCounter = B.sharedCounter + 1
}

// CHECK-LABEL: define hidden swiftcc %TSo1BC* @"$S7objc_ir8downcast1aSo1BCSo1AC_tF"(
func downcast(a a: A) -> B {
  // CHECK: [[CLASS:%.*]] = load %objc_class*, %objc_class** @"\01l_OBJC_CLASS_REF_$_B"
  // CHECK: [[T0:%.*]] = call %objc_class* @swift_getInitializedObjCClass(%objc_class* [[CLASS]])
  // CHECK: [[T1:%.*]] = bitcast %objc_class* [[T0]] to i8*
  // CHECK: call i8* @swift_dynamicCastObjCClassUnconditional(i8* [[A:%.*]], i8* [[T1]]) [[NOUNWIND:#[0-9]+]]
  return a as! B
}

// CHECK-LABEL: define hidden swiftcc void @"$S7objc_ir19almostSubscriptable3as11aySo06AlmostD0C_So1ACtF"
func almostSubscriptable(as1 as1: AlmostSubscriptable, a: A) {
  as1.objectForKeyedSubscript(a)
}

// CHECK-LABEL: define hidden swiftcc void @"$S7objc_ir13protocolTypes1a1bySo7NSMinceC_So9NSRuncing_ptF"(%TSo7NSMinceC*, %objc_object*) {{.*}} {
func protocolTypes(a a: NSMince, b: NSRuncing) {
  // - (void)eatWith:(id <NSRuncing>)runcer;
  a.eat(with: b)
  // CHECK: [[SEL:%.*]] = load i8*, i8** @"\01L_selector(eatWith:)", align 8
  // CHECK: call void bitcast (void ()* @objc_msgSend to void ([[OPAQUE:%.*]]*, i8*, i8*)*)([[OPAQUE:%.*]]* {{%.*}}, i8* [[SEL]], i8* {{%.*}})
}

// CHECK-LABEL: define hidden swiftcc void @"$S7objc_ir6getset1pySo8FooProto_p_tF"(%objc_object*) {{.*}} {
func getset(p p: FooProto) {
  // CHECK: load i8*, i8** @"\01L_selector(bar)"
  // CHECK: load i8*, i8** @"\01L_selector(setBar:)"
  let prop = p.bar
  p.bar = prop
}

// CHECK-LABEL: define hidden swiftcc %swift.type* @"$S7objc_ir16protocolMetatype1pSo8FooProto_pXpSoAD_p_tF"(%objc_object*) {{.*}} {
func protocolMetatype(p: FooProto) -> FooProto.Type {
  // CHECK: = call %swift.type* @swift_getObjectType(%objc_object* %0)
  // CHECK-NOT: {{retain|release}}
  // CHECK: [[RAW_RESULT:%.+]] = call i8* @processFooType(i8* {{%.+}})
  // CHECK: [[CASTED_RESULT:%.+]] = bitcast i8* [[RAW_RESULT]] to %objc_class*
  // CHECK: [[SWIFT_RESULT:%.+]] = call %swift.type* @swift_getObjCClassMetadata(%objc_class* [[CASTED_RESULT]])
  // CHECK-NOT: call void @swift_unknownRelease(%objc_object* %0)
  // CHECK: ret %swift.type* [[SWIFT_RESULT]]
  let type = processFooType(type(of: p))
  return type
} // CHECK: }

class Impl: FooProto, AnotherProto {
  @objc var bar: Int32 = 0
}

// CHECK-LABEL: define hidden swiftcc %swift.type* @"$S7objc_ir27protocolCompositionMetatype1pSo12AnotherProto_So03FooG0pXpAA4ImplC_tF"(%T7objc_ir4ImplC*) {{.*}} {
func protocolCompositionMetatype(p: Impl) -> (FooProto & AnotherProto).Type {
  // CHECK: = getelementptr inbounds %T7objc_ir4ImplC, %T7objc_ir4ImplC* %0, i32 0, i32 0, i32 0
  // CHECK-NOT: {{retain|release}}
  // CHECK: [[RAW_RESULT:%.+]] = call i8* @processComboType(i8* {{%.+}})
  // CHECK: [[CASTED_RESULT:%.+]] = bitcast i8* [[RAW_RESULT]] to %objc_class*
  // CHECK: [[SWIFT_RESULT:%.+]] = call %swift.type* @swift_getObjCClassMetadata(%objc_class* [[CASTED_RESULT]])
  // CHECK-NOT: call void bitcast (void (%swift.refcounted*)* @swift_release to void (%T7objc_ir4ImplC*)*)(%T7objc_ir4ImplC* %0)
  // CHECK: ret %swift.type* [[SWIFT_RESULT]]
  let type = processComboType(type(of: p))
  return type
} // CHECK: }

// CHECK-LABEL: define hidden swiftcc %swift.type* @"$S7objc_ir28protocolCompositionMetatype21pSo12AnotherProto_So03FooG0pXpAA4ImplC_tF"(%T7objc_ir4ImplC*) {{.*}} {
func protocolCompositionMetatype2(p: Impl) -> (FooProto & AnotherProto).Type {
  // CHECK: = getelementptr inbounds %T7objc_ir4ImplC, %T7objc_ir4ImplC* %0, i32 0, i32 0, i32 0
  // CHECK-NOT: {{retain|release}}
  // CHECK: [[RAW_RESULT:%.+]] = call i8* @processComboType2(i8* {{%.+}})
  // CHECK: [[CASTED_RESULT:%.+]] = bitcast i8* [[RAW_RESULT]] to %objc_class*
  // CHECK: [[SWIFT_RESULT:%.+]] = call %swift.type* @swift_getObjCClassMetadata(%objc_class* [[CASTED_RESULT]])
  // CHECK-NOT: @swift_release
  // CHECK: ret %swift.type* [[SWIFT_RESULT]]
  let type = processComboType2(type(of: p))
  return type
} // CHECK: }

// CHECK-LABEL: define hidden swiftcc void @"$S7objc_ir17pointerPropertiesyySo14PointerWrapperCF"(%TSo14PointerWrapperC*) {{.*}} {
func pointerProperties(_ obj: PointerWrapper) {
  // CHECK: load i8*, i8** @"\01L_selector(setVoidPtr:)"
  // CHECK: load i8*, i8** @"\01L_selector(setIntPtr:)"
  // CHECK: load i8*, i8** @"\01L_selector(setIdPtr:)"
  obj.voidPtr = nil as UnsafeMutableRawPointer?
  obj.intPtr = nil as UnsafeMutablePointer?
  obj.idPtr = nil as AutoreleasingUnsafeMutablePointer?
}

// CHECK-LABEL: define hidden swiftcc void @"$S7objc_ir16strangeSelectorsyySo13SwiftNameTestCF"(%TSo13SwiftNameTestC*) {{.*}} {
func strangeSelectors(_ obj: SwiftNameTest) {
  // CHECK: load i8*, i8** @"\01L_selector(:b:)"
  obj.empty(a: 0, b: 0)
}

// CHECK-LABEL: define hidden swiftcc void @"$S7objc_ir20customFactoryMethodsyyF"() {{.*}} {
func customFactoryMethods() {
  // CHECK: call swiftcc %TSo13SwiftNameTestC* @"$SSo13SwiftNameTestC10dummyParamAByt_tcfCTO"
  // CHECK: call swiftcc %TSo13SwiftNameTestC* @"$SSo13SwiftNameTestC2ccABypSg_tcfCTO"
  // CHECK: call swiftcc %TSo13SwiftNameTestC* @"$SSo13SwiftNameTestC5emptyABs5Int32V_tcfCTO"
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
    // CHECK: call swiftcc %TSo18SwiftNameTestErrorC* @"$SSo18SwiftNameTestErrorC5errorAByt_tKcfCTO"
    // CHECK: call swiftcc %TSo18SwiftNameTestErrorC* @"$SSo18SwiftNameTestErrorC2aa5errorABypSg_yttKcfCTO"
    // CHECK: call swiftcc %TSo18SwiftNameTestErrorC* @"$SSo18SwiftNameTestErrorC2aa5error5blockABypSg_ytyyctKcfCTO"
    // CHECK: call swiftcc %TSo18SwiftNameTestErrorC* @"$SSo18SwiftNameTestErrorC5error5blockAByt_yyctKcfCTO"
    // CHECK: call swiftcc %TSo18SwiftNameTestErrorC* @"$SSo18SwiftNameTestErrorC2aaABypSg_tKcfCTO"
    // CHECK: call swiftcc %TSo18SwiftNameTestErrorC* @"$SSo18SwiftNameTestErrorC2aa5blockABypSg_yyctKcfCTO"
    // CHECK: call swiftcc %TSo18SwiftNameTestErrorC* @"$SSo18SwiftNameTestErrorC5blockAByyc_tKcfCTO"
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

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo13SwiftNameTestC* @"$SSo13SwiftNameTestC10dummyParamAByt_tcfCTO"
// CHECK: load i8*, i8** @"\01L_selector(b)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo13SwiftNameTestC* @"$SSo13SwiftNameTestC2ccABypSg_tcfCTO"
// CHECK: load i8*, i8** @"\01L_selector(c:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo18SwiftNameTestErrorC* @"$SSo18SwiftNameTestErrorC5errorAByt_tKcfCTO"
// CHECK: load i8*, i8** @"\01L_selector(err1:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo18SwiftNameTestErrorC* @"$SSo18SwiftNameTestErrorC2aa5errorABypSg_yttKcfCTO"
// CHECK: load i8*, i8** @"\01L_selector(err2:error:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo18SwiftNameTestErrorC* @"$SSo18SwiftNameTestErrorC2aa5error5blockABypSg_ytyyctKcfCTO"
// CHECK: load i8*, i8** @"\01L_selector(err3:error:callback:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo18SwiftNameTestErrorC* @"$SSo18SwiftNameTestErrorC5error5blockAByt_yyctKcfCTO"
// CHECK: load i8*, i8** @"\01L_selector(err4:callback:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo18SwiftNameTestErrorC* @"$SSo18SwiftNameTestErrorC2aaABypSg_tKcfCTO"
// CHECK: load i8*, i8** @"\01L_selector(err5:error:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo18SwiftNameTestErrorC* @"$SSo18SwiftNameTestErrorC2aa5blockABypSg_yyctKcfCTO"
// CHECK: load i8*, i8** @"\01L_selector(err6:error:callback:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo18SwiftNameTestErrorC* @"$SSo18SwiftNameTestErrorC5blockAByyc_tKcfCTO"
// CHECK: load i8*, i8** @"\01L_selector(err7:callback:)"
// CHECK: }

// CHECK-LABEL: define hidden swiftcc void @"$S7objc_ir29customFactoryMethodsInheritedyyF"() {{.*}} {
func customFactoryMethodsInherited() {
  // CHECK: call swiftcc %TSo16SwiftNameTestSubC* @"$SSo16SwiftNameTestSubC10dummyParamAByt_tcfCTO"
  // CHECK: call swiftcc %TSo16SwiftNameTestSubC* @"$SSo16SwiftNameTestSubC2ccABypSg_tcfCTO"
  _ = SwiftNameTestSub(dummyParam: ())
  _ = SwiftNameTestSub(cc: nil)

  // CHECK: load i8*, i8** @"\01L_selector(testZ)"
  // CHECK: load i8*, i8** @"\01L_selector(testY:)"
  // CHECK: load i8*, i8** @"\01L_selector(testX:xx:)"
  _ = SwiftNameTestSub.zz()
  _ = SwiftNameTestSub.yy(aa: nil)
  _ = SwiftNameTestSub.xx(nil, bb: nil)

  do {
    // CHECK: call swiftcc %TSo21SwiftNameTestErrorSubC* @"$SSo21SwiftNameTestErrorSubC5errorAByt_tKcfCTO"
    // CHECK: call swiftcc %TSo21SwiftNameTestErrorSubC* @"$SSo21SwiftNameTestErrorSubC2aa5errorABypSg_yttKcfCTO"
    // CHECK: call swiftcc %TSo21SwiftNameTestErrorSubC* @"$SSo21SwiftNameTestErrorSubC2aa5error5blockABypSg_ytyyctKcfCTO"
    // CHECK: call swiftcc %TSo21SwiftNameTestErrorSubC* @"$SSo21SwiftNameTestErrorSubC5error5blockAByt_yyctKcfCTO"
    // CHECK: call swiftcc %TSo21SwiftNameTestErrorSubC* @"$SSo21SwiftNameTestErrorSubC2aaABypSg_tKcfCTO"
    // CHECK: call swiftcc %TSo21SwiftNameTestErrorSubC* @"$SSo21SwiftNameTestErrorSubC2aa5blockABypSg_yyctKcfCTO"
    // CHECK: call swiftcc %TSo21SwiftNameTestErrorSubC* @"$SSo21SwiftNameTestErrorSubC5blockAByyc_tKcfCTO"
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

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo16SwiftNameTestSubC* @"$SSo16SwiftNameTestSubC10dummyParamAByt_tcfCTO"
// CHECK: load i8*, i8** @"\01L_selector(b)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo16SwiftNameTestSubC* @"$SSo16SwiftNameTestSubC2ccABypSg_tcfCTO"
// CHECK: load i8*, i8** @"\01L_selector(c:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo21SwiftNameTestErrorSubC* @"$SSo21SwiftNameTestErrorSubC5errorAByt_tKcfCTO"
// CHECK: load i8*, i8** @"\01L_selector(err1:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo21SwiftNameTestErrorSubC* @"$SSo21SwiftNameTestErrorSubC2aa5errorABypSg_yttKcfCTO"
// CHECK: load i8*, i8** @"\01L_selector(err2:error:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo21SwiftNameTestErrorSubC* @"$SSo21SwiftNameTestErrorSubC2aa5error5blockABypSg_ytyyctKcfCTO"
// CHECK: load i8*, i8** @"\01L_selector(err3:error:callback:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo21SwiftNameTestErrorSubC* @"$SSo21SwiftNameTestErrorSubC5error5blockAByt_yyctKcfCTO"
// CHECK: load i8*, i8** @"\01L_selector(err4:callback:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo21SwiftNameTestErrorSubC* @"$SSo21SwiftNameTestErrorSubC2aaABypSg_tKcfCTO"
// CHECK: load i8*, i8** @"\01L_selector(err5:error:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo21SwiftNameTestErrorSubC* @"$SSo21SwiftNameTestErrorSubC2aa5blockABypSg_yyctKcfCTO"
// CHECK: load i8*, i8** @"\01L_selector(err6:error:callback:)"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc %TSo21SwiftNameTestErrorSubC* @"$SSo21SwiftNameTestErrorSubC5blockAByyc_tKcfCTO"
// CHECK: load i8*, i8** @"\01L_selector(err7:callback:)"
// CHECK: }


// CHECK-LABEL: define hidden swiftcc void @"$S7objc_ir30testCompatibilityAliasMangling3objySo13SwiftNameTestC_tF"
func testCompatibilityAliasMangling(obj: SwiftNameAlias) {
  // CHECK: call void @llvm.dbg.declare(metadata %TSo13SwiftNameTestC** {{%.+}}, metadata ![[SWIFT_NAME_ALIAS_VAR:[0-9]+]], metadata !DIExpression())
}


// CHECK: linkonce_odr hidden {{.*}} @"$SSo1BC3intABSgs5Int32V_tcfcTO"
// CHECK: load i8*, i8** @"\01L_selector(initWithInt:)"
// CHECK: call [[OPAQUE:%.*]]* bitcast (void ()* @objc_msgSend


// CHECK: attributes [[NOUNWIND]] = { nounwind }

// CHECK: ![[SWIFT_NAME_ALIAS_VAR]] = !DILocalVariable(name: "obj", arg: 1, scope: !{{[0-9]+}}, file: !{{[0-9]+}}, line: {{[0-9]+}}, type: ![[SWIFT_NAME_ALIAS_TYPE:[0-9]+]])
// CHECK: ![[SWIFT_NAME_ALIAS_TYPE]] = !DIDerivedType(tag: DW_TAG_typedef, name: "$SSo14SwiftNameAliasaD", scope: !{{[0-9]+}}, file: !{{[0-9]+}}, baseType: !{{[0-9]+}})

