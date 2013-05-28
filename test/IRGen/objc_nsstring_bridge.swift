// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -nsstring-is-string -emit-llvm -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s | FileCheck %s

import Foundation

// CHECK: [[GETTER_SIGNATURE:@.*]] = private unnamed_addr constant [4 x i8] c"@@:\00"
// CHECK: [[SETTER_SIGNATURE:@.*]] = private unnamed_addr constant [5 x i8] c"v@:@\00"

// CHECK: @_INSTANCE_METHODS_Bas = private constant { i32, i32, [12 x { i8*, i8*, i8* }] } {
// CHECK:   i32 24,
// CHECK:   i32 12,
// CHECK:   [12 x { i8*, i8*, i8* }] [
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([12 x i8]* @"\01L_selector_data(strRealProp)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([4 x i8]* [[GETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast (%CSo8NSString* (%CSo3Bas*, i8*)* @_TToCSo3Bas11strRealPropSSg to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([16 x i8]* @"\01L_selector_data(setStrRealProp:)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([5 x i8]* [[SETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast (void (%CSo3Bas*, i8*, %CSo8NSString*)* @_TToCSo3Bas11strRealPropSSs to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([12 x i8]* @"\01L_selector_data(strFakeProp)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([4 x i8]* [[GETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast (%CSo8NSString* (%CSo3Bas*, i8*)* @_TToCSo3Bas11strFakePropSSg to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([16 x i8]* @"\01L_selector_data(setStrFakeProp:)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([5 x i8]* [[SETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast (void (%CSo3Bas*, i8*, %CSo8NSString*)* @_TToCSo3Bas11strFakePropSSs to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([14 x i8]* @"\01L_selector_data(nsstrRealProp)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([4 x i8]* [[GETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast (%CSo8NSString* (%CSo3Bas*, i8*)* @_TToCSo3Bas13nsstrRealPropCSo8NSStringg to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([18 x i8]* @"\01L_selector_data(setNsstrRealProp:)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([5 x i8]* [[SETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast (void (%CSo3Bas*, i8*, %CSo8NSString*)* @_TToCSo3Bas13nsstrRealPropCSo8NSStrings to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([14 x i8]* @"\01L_selector_data(nsstrFakeProp)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([4 x i8]* [[GETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast (%CSo8NSString* (%CSo3Bas*, i8*)* @_TToCSo3Bas13nsstrFakePropCSo8NSStringg to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([18 x i8]* @"\01L_selector_data(setNsstrFakeProp:)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([5 x i8]* [[SETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast (void (%CSo3Bas*, i8*, %CSo8NSString*)* @_TToCSo3Bas13nsstrFakePropCSo8NSStrings to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([10 x i8]* @"\01L_selector_data(strResult)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([4 x i8]* [[GETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast (%CSo8NSString* (%CSo3Bas*, i8*)* @_TToCSo3Bas9strResultfS_FT_SS to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([8 x i8]* @"\01L_selector_data(strArg:)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([5 x i8]* [[SETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast (void (%CSo3Bas*, i8*, %CSo8NSString*)* @_TToCSo3Bas6strArgfS_FT1sSS_T_ to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([12 x i8]* @"\01L_selector_data(nsstrResult)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([4 x i8]* [[GETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast (%CSo8NSString* (%CSo3Bas*, i8*)* @_TToCSo3Bas11nsstrResultfS_FT_CSo8NSString to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([10 x i8]* @"\01L_selector_data(nsstrArg:)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([5 x i8]* [[SETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast (void (%CSo3Bas*, i8*, %CSo8NSString*)* @_TToCSo3Bas8nsstrArgfS_FT1sCSo8NSString_T_ to i8*)
// CHECK:     }
// CHECK:   ]
// CHECK: }, section "__DATA, __objc_const", align 8

// CHECK: @_PROPERTIES_Bas = private constant { i32, i32, [4 x { i8*, i8* }] } {

func getDescription(o:NSObject) -> String {
  return o.description()
}

func getUppercaseString(s:NSString) -> String {
  return s.uppercaseString()
}

// @interface Foo -(void) setFoo: (NSString*)s; @end
func setFoo(f:Foo, s:String) {
  f.setFoo(s)
}

// NSString *bar(int);
func callBar() -> String {
  return bar(0)
}

// void setBar(NSString *s);
func callSetBar(s:String) {
  setBar(s)
}

var NSS : NSString

// -- NSString methods don't convert 'this'
extension NSString {
  // CHECK: define internal %CSo8NSString* @_TToCSo8NSString13nsstrFakePropS_g(%CSo8NSString*, i8*) unnamed_addr
  // CHECK: define internal void @_TToCSo8NSString13nsstrFakePropS_s(%CSo8NSString*, i8*, %CSo8NSString*) unnamed_addr
  var nsstrFakeProp : NSString {
    get: return NSS
    set:
  }

  // CHECK: define internal %CSo8NSString* @_TToCSo8NSString11nsstrResultfS_FT_S_(%CSo8NSString*, i8*) unnamed_addr
  func nsstrResult() -> NSString { return NSS }

  // CHECK: define internal void @_TToCSo8NSString8nsstrArgfS_FT1sS__T_(%CSo8NSString*, i8*, %CSo8NSString*) unnamed_addr
  func nsstrArg(s:NSString) { }
}

class Bas : NSObject {
  // CHECK: define internal %CSo8NSString* @_TToCSo3Bas11strRealPropSSg(%CSo3Bas*, i8*) unnamed_addr {
  // CHECK: define internal void @_TToCSo3Bas11strRealPropSSs(%CSo3Bas*, i8*, %CSo8NSString*) unnamed_addr {
  var strRealProp : String

  // CHECK: define internal %CSo8NSString* @_TToCSo3Bas11strFakePropSSg(%CSo3Bas*, i8*) unnamed_addr {
  // CHECK: define internal void @_TToCSo3Bas11strFakePropSSs(%CSo3Bas*, i8*, %CSo8NSString*) unnamed_addr {
  var strFakeProp : String {
    get: return ""
    set:
  }

  // CHECK: define internal %CSo8NSString* @_TToCSo3Bas13nsstrRealPropCSo8NSStringg(%CSo3Bas*, i8*) unnamed_addr {
  // CHECK: define internal void @_TToCSo3Bas13nsstrRealPropCSo8NSStrings(%CSo3Bas*, i8*, %CSo8NSString*) unnamed_addr {
  var nsstrRealProp : NSString

  // CHECK: define %CSo8NSString* @_TCSo3Bas13nsstrFakePropCSo8NSStringg(%CSo3Bas*) {
  // CHECK: define internal void @_TToCSo3Bas13nsstrFakePropCSo8NSStrings(%CSo3Bas*, i8*, %CSo8NSString*) unnamed_addr {
  var nsstrFakeProp : NSString {
    get: return NSS
    set:
  }

  // CHECK: define internal %CSo8NSString* @_TToCSo3Bas9strResultfS_FT_SS(%CSo3Bas*, i8*) unnamed_addr {
  func strResult() -> String { return "" }
  // CHECK: define internal void @_TToCSo3Bas6strArgfS_FT1sSS_T_(%CSo3Bas*, i8*, %CSo8NSString*) unnamed_addr {
  func strArg(s:String) { }

  // CHECK: define internal %CSo8NSString* @_TToCSo3Bas11nsstrResultfS_FT_CSo8NSString(%CSo3Bas*, i8*) unnamed_addr {
  func nsstrResult() -> NSString { return NSS }
  // CHECK: define internal void @_TToCSo3Bas8nsstrArgfS_FT1sCSo8NSString_T_(%CSo3Bas*, i8*, %CSo8NSString*) unnamed_addr {
  func nsstrArg(s:NSString) { }
}

