// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -emit-ir -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -sdk %S/Inputs -I=%S/Inputs -enable-source-import %s | FileCheck %s

import Foundation

// CHECK: [[GETTER_SIGNATURE:@.*]] = private unnamed_addr constant [8 x i8] c"@16@0:8\00"
// CHECK: [[SETTER_SIGNATURE:@.*]] = private unnamed_addr constant [11 x i8] c"v24@0:8@16\00"

// CHECK: @_INSTANCE_METHODS__TtC20objc_nsstring_bridge3Bas = private constant { i32, i32, [15 x { i8*, i8*, i8* }] } {
// CHECK:   i32 24,
// CHECK:   i32 15,
// CHECK:   [15 x { i8*, i8*, i8* }] [
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([12 x i8]* @"\01L_selector_data(strRealProp)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([8 x i8]* [[GETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast ([[OPAQUE:.*]]* ([[OPAQUE:.*]]*, i8*)* @_TToFC20objc_nsstring_bridge3Basg11strRealPropSS to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([16 x i8]* @"\01L_selector_data(setStrRealProp:)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([11 x i8]* [[SETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast (void ([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*)* @_TToFC20objc_nsstring_bridge3Bass11strRealPropSS to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([12 x i8]* @"\01L_selector_data(strFakeProp)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([8 x i8]* [[GETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast ([[OPAQUE:.*]]* ([[OPAQUE:.*]]*, i8*)* @_TToFC20objc_nsstring_bridge3Basg11strFakePropSS to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([16 x i8]* @"\01L_selector_data(setStrFakeProp:)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([11 x i8]* [[SETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast (void ([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*)* @_TToFC20objc_nsstring_bridge3Bass11strFakePropSS to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([14 x i8]* @"\01L_selector_data(nsstrRealProp)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([8 x i8]* [[GETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast ([[OPAQUE:.*]]* ([[OPAQUE:.*]]*, i8*)* @_TToFC20objc_nsstring_bridge3Basg13nsstrRealPropCSo8NSString to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([18 x i8]* @"\01L_selector_data(setNsstrRealProp:)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([11 x i8]* [[SETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast (void ([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*)* @_TToFC20objc_nsstring_bridge3Bass13nsstrRealPropCSo8NSString to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([14 x i8]* @"\01L_selector_data(nsstrFakeProp)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([8 x i8]* [[GETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast ([[OPAQUE:.*]]* ([[OPAQUE:.*]]*, i8*)* @_TToFC20objc_nsstring_bridge3Basg13nsstrFakePropCSo8NSString to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([18 x i8]* @"\01L_selector_data(setNsstrFakeProp:)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([11 x i8]* [[SETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast (void ([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*)* @_TToFC20objc_nsstring_bridge3Bass13nsstrFakePropCSo8NSString to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([10 x i8]* @"\01L_selector_data(strResult)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([8 x i8]* [[GETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast ([[OPAQUE:.*]]* ([[OPAQUE:.*]]*, i8*)* @_TToFC20objc_nsstring_bridge3Bas9strResultfS0_FT_SS to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([{{[0-9]*}} x i8]* @"\01L_selector_data(strArgWithS:)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([11 x i8]* [[SETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast (void ([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*)* @_TToFC20objc_nsstring_bridge3Bas6strArgfS0_FT1sSS_T_ to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([12 x i8]* @"\01L_selector_data(nsstrResult)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([8 x i8]* [[GETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast ([[OPAQUE:.*]]* ([[OPAQUE:.*]]*, i8*)* @_TToFC20objc_nsstring_bridge3Bas11nsstrResultfS0_FT_CSo8NSString to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([{{[0-9]+}} x i8]* @"\01L_selector_data(nsstrArgWithS:)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([11 x i8]* [[SETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast (void ([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*)* @_TToFC20objc_nsstring_bridge3Bas8nsstrArgfS0_FT1sCSo8NSString_T_ to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } { 
// CHECK:       i8* getelementptr inbounds ([5 x i8]* @"\01L_selector_data(init)", i64 0, i64 0), 
// CHECK:       i8* getelementptr inbounds ([8 x i8]* @3, i64 0, i64 0),
// CHECK:       i8* bitcast ([[OPAQUE:.*]]* ([[OPAQUE:.*]]*, i8*)* @_TToFC20objc_nsstring_bridge3BascfMS0_FT_S0_ to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } { 
// CHECK:       i8* getelementptr inbounds ([8 x i8]* @"\01L_selector_data(dealloc)", i64 0, i64 0), 
// CHECK:       i8* getelementptr inbounds ([11 x i8]* @4, i64 0, i64 0),
// CHECK:       i8* bitcast (void ([[OPAQUE:.*]]*, i8*)* @_TToFC20objc_nsstring_bridge3BasD to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } { 
// CHECK:       i8* getelementptr inbounds ([14 x i8]* @"\01L_selector_data(.cxx_destruct)", i64 0, i64 0), 
// CHECK:       i8* getelementptr inbounds ([3 x i8]* @12, i64 0, i64 0),
// CHECK:       i8* bitcast (void ([[OPAQUE:.*]]*, i8*)* @_TToFC20objc_nsstring_bridge3BasE to i8*)
// CHECK:     }
// CHECK:   ]
// CHECK: }, section "__DATA, __objc_const", align 8

// CHECK: @_PROPERTIES__TtC20objc_nsstring_bridge3Bas = private constant { i32, i32, [4 x { i8*, i8* }] } {

func getDescription(o: NSObject) -> String {
  return o.description
}

func getUppercaseString(s: NSString) -> String {
  return s.uppercaseString()
}

// @interface Foo -(void) setFoo: (NSString*)s; @end
func setFoo(f: Foo, s: String) {
  f.setFoo(s)
}

// NSString *bar(int);
func callBar() -> String {
  return bar(0)
}

// void setBar(NSString *s);
func callSetBar(s: String) {
  setBar(s)
}

var NSS : NSString = NSString()

// -- NSString methods don't convert 'self'
extension NSString {
  // CHECK: define internal [[OPAQUE:.*]]* @_TToFCSo8NSStringg13nsstrFakePropS_([[OPAQUE:.*]]*, i8*) unnamed_addr
  // CHECK: define internal void @_TToFCSo8NSStrings13nsstrFakePropS_([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*) unnamed_addr
  var nsstrFakeProp : NSString {
    get {
      return NSS
    }
    set {}
  }

  // CHECK: define internal [[OPAQUE:.*]]* @_TToFCSo8NSString11nsstrResultfS_FT_S_([[OPAQUE:.*]]*, i8*) unnamed_addr
  func nsstrResult() -> NSString { return NSS }

  // CHECK: define internal void @_TToFCSo8NSString8nsstrArgfS_FT1sS__T_([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*) unnamed_addr
  func nsstrArg(`s: NSString) { }
}

class Bas : NSObject {
  // CHECK: define internal [[OPAQUE:.*]]* @_TToFC20objc_nsstring_bridge3Basg11strRealPropSS([[OPAQUE:.*]]*, i8*) unnamed_addr {
  // CHECK: define internal void @_TToFC20objc_nsstring_bridge3Bass11strRealPropSS([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*) unnamed_addr {
  var strRealProp : String

  // CHECK: define internal [[OPAQUE:.*]]* @_TToFC20objc_nsstring_bridge3Basg11strFakePropSS([[OPAQUE:.*]]*, i8*) unnamed_addr {
  // CHECK: define internal void @_TToFC20objc_nsstring_bridge3Bass11strFakePropSS([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*) unnamed_addr {
  var strFakeProp : String {
    get {
      return ""
    }
    set {}
  }

  // CHECK: define internal [[OPAQUE:.*]]* @_TToFC20objc_nsstring_bridge3Basg13nsstrRealPropCSo8NSString([[OPAQUE:.*]]*, i8*) unnamed_addr {
  // CHECK: define internal void @_TToFC20objc_nsstring_bridge3Bass13nsstrRealPropCSo8NSString([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*) unnamed_addr {
  var nsstrRealProp : NSString

  // CHECK: define %CSo8NSString* @_TFC20objc_nsstring_bridge3Basg13nsstrFakePropCSo8NSString(%C20objc_nsstring_bridge3Bas*) {
  // CHECK: define internal void @_TToFC20objc_nsstring_bridge3Bass13nsstrFakePropCSo8NSString([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*) unnamed_addr {
  var nsstrFakeProp : NSString {
    get {
      return NSS
    }
    set {}
  }

  // CHECK: define internal [[OPAQUE:.*]]* @_TToFC20objc_nsstring_bridge3Bas9strResultfS0_FT_SS([[OPAQUE:.*]]*, i8*) unnamed_addr {
  func strResult() -> String { return "" }
  // CHECK: define internal void @_TToFC20objc_nsstring_bridge3Bas6strArgfS0_FT1sSS_T_([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*) unnamed_addr {
  func strArg(`s: String) { }

  // CHECK: define internal [[OPAQUE:.*]]* @_TToFC20objc_nsstring_bridge3Bas11nsstrResultfS0_FT_CSo8NSString([[OPAQUE:.*]]*, i8*) unnamed_addr {
  func nsstrResult() -> NSString { return NSS }
  // CHECK: define internal void @_TToFC20objc_nsstring_bridge3Bas8nsstrArgfS0_FT1sCSo8NSString_T_([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*) unnamed_addr {
  func nsstrArg(`s: NSString) { }

  init() { 
    strRealProp = String()
    nsstrRealProp = NSString()
    super.init()
  }

  deinit { var x = 10 }
}

