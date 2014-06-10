// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -module-cache-path %t/clang-module-cache -I %S/Inputs/custom-modules -target x86_64-apple-darwin13 -emit-ir -o - %s | FileCheck %s

// CHECK: [[B:%CSo1B]] = type

import ObjectiveC
import Foundation
import objc_ext
import TestProtocols
import ObjCIRExtras

// CHECK: @"\01L_selector_data(method:withFloat:)" = internal constant [18 x i8] c"method:withFloat:\00"
// CHECK: @"\01L_selector_data(method:withDouble:)" = internal constant [19 x i8] c"method:withDouble:\00"
// CHECK: @"\01L_selector_data(method:separateExtMethod:)" = internal constant [26 x i8] c"method:separateExtMethod:\00", section "__TEXT,__objc_methname,cstring_literals"

// Instance method invocation
// CHECK: define void @_TF7objc_ir15instanceMethodsFCSo1BT_([[B]]*
func instanceMethods(b: B) {
  // CHECK: load i8** @"\01L_selector(method:withFloat:)"
  // CHECK: call i32 bitcast (void ()* @objc_msgSend to i32
  var i = b.method(1, withFloat:2.5)
  // CHECK: load i8** @"\01L_selector(method:withDouble:)"
  // CHECK: call i32 bitcast (void ()* @objc_msgSend to i32
  i = i + b.method(1, withDouble:2.5)
}

// CHECK: define void @_TF7objc_ir16extensionMethodsFT1bCSo1B_T_
func extensionMethods(#b: B) {
  // CHECK: load i8** @"\01L_selector(method:separateExtMethod:)", align 8
  // CHECK: [[T0:%.*]] = call i8* bitcast (void ()* @objc_msgSend to i8*
  // CHECK: [[T1:%.*]] = ptrtoint i8* [[T0]] to i64
  // CHECK: call i64 bitcast (%objc_object* (%objc_object*)* @objc_retainAutoreleasedReturnValue to i64 (i64)*)(i64 [[T1]])
  b.method(1, separateExtMethod:1.5)
}

// CHECK: define void @_TF7objc_ir19initCallToAllocInitFT1iVSs5Int32_T_
func initCallToAllocInit(#i: CInt) {
  // CHECK: call [[B]]* @_TFCSo1BCfMS_FT3intVSs5Int32_S_
 
  B(int: i)
}

// CHECK: linkonce_odr hidden [[B]]* @_TFCSo1BCfMS_FT3intVSs5Int32_S_
// CHECK: load i8** @"\01L_selector(allocWithZone:)"
// CHECK: call [[OPAQUE:%.*]]* bitcast (void ()* @objc_msgSend

// Indexed subscripting
// CHECK: define void @_TF7objc_ir19indexedSubscriptingFT1bCSo1B3idxSi1aCSo1A_T_
func indexedSubscripting(#b: B, #idx: Int, #a: A) {
  // CHECK: load i8** @"\01L_selector(setObject:atIndexedSubscript:)", align 8
  b[idx] = a

  // CHECK: load i8** @"\01L_selector(objectAtIndexedSubscript:)"
  var a2 = b[idx] as A
}

// CHECK: define void @_TF7objc_ir17keyedSubscriptingFT1bCSo1B3idxCSo1A1aS1__T_
func keyedSubscripting(#b: B, #idx: A, #a: A) {
  // CHECK: load i8** @"\01L_selector(setObject:forKeyedSubscript:)"
  b[a] = a
  // CHECK: load i8** @"\01L_selector(objectForKeyedSubscript:)"
  var a2 = b[a] as A
}

// CHECK: define void @_TF7objc_ir14propertyAccessFT1bCSo1B_T_
func propertyAccess(#b: B) {
   // CHECK: load i8** @"\01L_selector(counter)"
   // CHECK: load i8** @"\01L_selector(setCounter:)"
   b.counter = b.counter + 1
}

// CHECK: define [[B]]* @_TF7objc_ir8downcastFT1aCSo1A_CSo1B(
func downcast(#a: A) -> B {
  // CHECK: call i8* @swift_dynamicCastObjCClassUnconditional(i8* [[A:%.*]], i8* bitcast (%objc_class* @"OBJC_CLASS_$_B" to i8*)) [[NOUNWIND:#[0-9]+]]
  return a as B
}

// CHECK: define void @_TF7objc_ir19almostSubscriptableFT3as1CSo19AlmostSubscriptable1aCSo1A_T_
func almostSubscriptable(#as1: AlmostSubscriptable, #a: A) {
  as1.objectForKeyedSubscript(a)
}

// CHECK: define void @_TF7objc_ir13protocolTypesFT1aCSo7NSMince1bPSo9NSRuncing__T_(%CSo7NSMince*, %objc_object*) {
func protocolTypes(#a: NSMince, #b: NSRuncing) {
  // - (void)eatWith:(id <NSRuncing>)runcer;
  a.eatWith(b)
  // CHECK: [[SEL:%.*]] = load i8** @"\01L_selector(eatWith:)", align 8
  // CHECK: call void bitcast (void ()* @objc_msgSend to void ([[OPAQUE:%.*]]*, i8*, i8*)*)([[OPAQUE:%.*]]* {{%.*}}, i8* [[SEL]], i8* {{%.*}})
}

// CHECK-LABEL: define void @_TF7objc_ir6getsetFT1pPSo8FooProto__T_(%objc_object*) {
func getset(#p: FooProto) {
  // CHECK: load i8** @"\01L_selector(bar)"
  // CHECK: load i8** @"\01L_selector(setBar:)"
  let prop = p.bar
  p.bar = prop
}

// CHECK-LABEL: define void @_TF7objc_ir17pointerPropertiesFCSo14PointerWrapperT_(%CSo14PointerWrapper*) {
func pointerProperties(obj: PointerWrapper) {
  // CHECK: load i8** @"\01L_selector(setVoidPtr:)"
  // CHECK: load i8** @"\01L_selector(setIntPtr:)"
  // CHECK: load i8** @"\01L_selector(setIdPtr:)"
  obj.voidPtr = COpaquePointer()
  obj.intPtr = UnsafePointer()
  obj.idPtr = UnsafePointer()
}

// CHECK: linkonce_odr hidden [[B]]* @_TTOFCSo1BcfMS_FT3intVSs5Int32_S_
// CHECK: load i8** @"\01L_selector(initWithInt:)"
// CHECK: call [[OPAQUE:%.*]]* bitcast (void ()* @objc_msgSend


// CHECK: attributes [[NOUNWIND]] = { nounwind }

