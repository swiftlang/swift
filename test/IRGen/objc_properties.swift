// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -triple x86_64-apple-darwin10 -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-llvm | FileCheck %s

class [objc] SomeObject {
  var readonly : SomeObject {
  get:
    return this
  }

  var _readwrite : SomeObject

  var readwrite : SomeObject {
  get:
    return _readwrite
  set:
    _readwrite = readwrite
  }

  var bareIvar : SomeObject
}

extension SomeObject {
  var extensionProperty : SomeObject {
  get:
    return this
  set:
    bareIvar = this
  }
}

// CHECK: [[GETTER_SIGNATURE:@.*]] = private unnamed_addr constant [4 x i8] c"@@:\00"

// CHECK: [[READONLY_NAME:@.*]] = private unnamed_addr constant [9 x i8] c"readonly\00"
// CHECK: [[READONLY_ATTRS:@.*]] = private unnamed_addr constant [29 x i8] c"T@\22SomeObject\22,R,N,Vreadonly\00"

// CHECK: [[READWRITE_NAME:@.*]] = private unnamed_addr constant [10 x i8] c"readwrite\00"
// CHECK: [[READWRITE_ATTRS:@.*]] = private unnamed_addr constant [30 x i8] c"T@\22SomeObject\22,&,N,Vreadwrite\00"

// CHECK: @_INSTANCE_METHODS_SomeObject = private constant { {{.*}}] } {
// CHECK:   i32 24,
// CHECK:   i32 3,
// CHECK:   [3 x { i8*, i8*, i8* }] [{
// CHECK:     i8* getelementptr inbounds ([9 x i8]* @"\01L_selector_data(readonly)", i64 0, i64 0),
// CHECK:     i8* getelementptr inbounds ([4 x i8]* [[GETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:     i8* bitcast (%CSo10SomeObject* (%CSo10SomeObject*, i8*)* @_TToCSo10SomeObject8readonlyS_g to i8*)
// CHECK:   }, {
// CHECK:     i8* getelementptr inbounds ([10 x i8]* @"\01L_selector_data(readwrite)", i64 0, i64 0),
// CHECK:     i8* getelementptr inbounds ([4 x i8]* [[GETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:     i8* bitcast (%CSo10SomeObject* (%CSo10SomeObject*, i8*)* @_TToCSo10SomeObject9readwriteS_g to i8*)
// CHECK:   }, {
// CHECK:     i8* getelementptr inbounds ([14 x i8]* @"\01L_selector_data(setReadwrite:)", i64 0, i64 0),
// CHECK:     i8* null,
// CHECK:     i8* bitcast (void (%CSo10SomeObject*, i8*, %CSo10SomeObject*)* @_TToCSo10SomeObject9readwriteS_s to i8*)
// CHECK:   }]
// CHECK: }, section "__DATA, __objc_const", align 8

// CHECK: @_PROPERTIES_SomeObject = private constant { {{.*}}] } {
// CHECK:   i32 16,
// CHECK:   i32 2,
// CHECK:   [2 x { i8*, i8* }] [{
// CHECK:     i8* getelementptr inbounds ([9 x i8]* [[READONLY_NAME]], i64 0, i64 0),
// CHECK:     i8* getelementptr inbounds ([29 x i8]* [[READONLY_ATTRS]], i64 0, i64 0)
// CHECK:   }, {
// CHECK:     i8* getelementptr inbounds ([10 x i8]* [[READWRITE_NAME]], i64 0, i64 0),
// CHECK:     i8* getelementptr inbounds ([30 x i8]* [[READWRITE_ATTRS]], i64 0, i64 0)
// CHECK:   }]
// CHECK: }, section "__DATA, __objc_const", align 8

// CHECK: [[EXTENSIONPROPERTY_NAME:@.*]] = private unnamed_addr constant [18 x i8] c"extensionProperty\00"
// CHECK: [[EXTENSIONPROPERTY_ATTRS:@.*]] = private unnamed_addr constant [38 x i8] c"T@\22SomeObject\22,&,N,VextensionProperty\00"

// CHECK: @"_CATEGORY_INSTANCE_METHODS_SomeObject_$_objc_properties" = private constant { {{.*}}] } {
// CHECK:   i32 24,
// CHECK:   i32 2,
// CHECK:   [2 x { i8*, i8*, i8* }] [{
// CHECK:     { i8* getelementptr inbounds ([18 x i8]* @"\01L_selector_data(extensionProperty)", i64 0, i64 0),
// CHECK:     i8* getelementptr inbounds ([4 x i8]* [[GETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:     i8* bitcast (%CSo10SomeObject* (%CSo10SomeObject*, i8*)* @_TToCSo10SomeObject17extensionPropertyS_g to i8*)
// CHECK:   }, {
// CHECK:     i8* getelementptr inbounds ([22 x i8]* @"\01L_selector_data(setExtensionProperty:)", i64 0, i64 0),
// CHECK:     i8* null,
// CHECK:     i8* bitcast (void (%CSo10SomeObject*, i8*, %CSo10SomeObject*)* @_TToCSo10SomeObject17extensionPropertyS_s to i8*)
// CHECK:   }]
// CHECK: }, section "__DATA, __objc_const", align 8

// CHECK: @"_CATEGORY_PROPERTIES_SomeObject_$_objc_properties" = private constant { {{.*}}] } {
// CHECK:   i32 16,
// CHECK:   i32 1,
// CHECK:   [1 x { i8*, i8* }] [{
// CHECK:     i8* getelementptr inbounds ([18 x i8]* [[EXTENSIONPROPERTY_NAME]], i64 0, i64 0),
// CHECK:     i8* getelementptr inbounds ([38 x i8]* [[EXTENSIONPROPERTY_ATTRS]], i64 0, i64 0)
// CHECK:   }]
// CHECK: }, section "__DATA, __objc_const", align 8
