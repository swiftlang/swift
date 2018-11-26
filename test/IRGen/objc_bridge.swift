// RUN: %empty-directory(%t)
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -emit-ir -primary-file %s | %FileCheck %s

// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

import Foundation

// CHECK: [[GETTER_SIGNATURE:@.*]] = private unnamed_addr constant [8 x i8] c"@16@0:8\00"
// CHECK: [[SETTER_SIGNATURE:@.*]] = private unnamed_addr constant [11 x i8] c"v24@0:8@16\00"
// CHECK: [[DEALLOC_SIGNATURE:@.*]] = private unnamed_addr constant [8 x i8] c"v16@0:8\00"

// CHECK: @_INSTANCE_METHODS__TtC11objc_bridge3Bas = private constant { i32, i32, [17 x { i8*, i8*, i8* }] } {
// CHECK:   i32 24,
// CHECK:   i32 17,
// CHECK:   [17 x { i8*, i8*, i8* }] [
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([12 x i8], [12 x i8]* @"\01L_selector_data(strRealProp)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([8 x i8], [8 x i8]* [[GETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast ([[OPAQUE:.*]]* ([[OPAQUE:.*]]*, i8*)* @"$s11objc_bridge3BasC11strRealPropSSvgTo" to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([16 x i8], [16 x i8]* @"\01L_selector_data(setStrRealProp:)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([11 x i8], [11 x i8]* [[SETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast (void ([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*)* @"$s11objc_bridge3BasC11strRealPropSSvsTo" to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([12 x i8], [12 x i8]* @"\01L_selector_data(strFakeProp)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([8 x i8], [8 x i8]* [[GETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast ([[OPAQUE:.*]]* ([[OPAQUE:.*]]*, i8*)* @"$s11objc_bridge3BasC11strFakePropSSvgTo" to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([16 x i8], [16 x i8]* @"\01L_selector_data(setStrFakeProp:)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([11 x i8], [11 x i8]* [[SETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast (void ([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*)* @"$s11objc_bridge3BasC11strFakePropSSvsTo" to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([14 x i8], [14 x i8]* @"\01L_selector_data(nsstrRealProp)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([8 x i8], [8 x i8]* [[GETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast ([[OPAQUE:.*]]* ([[OPAQUE:.*]]*, i8*)* @"$s11objc_bridge3BasC13nsstrRealPropSo8NSStringCvgTo" to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([18 x i8], [18 x i8]* @"\01L_selector_data(setNsstrRealProp:)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([11 x i8], [11 x i8]* [[SETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast (void ([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*)* @"$s11objc_bridge3BasC13nsstrRealPropSo8NSStringCvsTo" to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([14 x i8], [14 x i8]* @"\01L_selector_data(nsstrFakeProp)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([8 x i8], [8 x i8]* [[GETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast ([[OPAQUE:.*]]* ([[OPAQUE:.*]]*, i8*)* @"$s11objc_bridge3BasC13nsstrFakePropSo8NSStringCvgTo" to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([18 x i8], [18 x i8]* @"\01L_selector_data(setNsstrFakeProp:)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([11 x i8], [11 x i8]* [[SETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast (void ([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*)* @"$s11objc_bridge3BasC13nsstrFakePropSo8NSStringCvsTo" to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([10 x i8], [10 x i8]* @"\01L_selector_data(strResult)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([8 x i8], [8 x i8]* [[GETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast ([[OPAQUE:.*]]* ([[OPAQUE:.*]]*, i8*)* @"$s11objc_bridge3BasC9strResultSSyFTo" to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([{{[0-9]*}} x i8], [{{[0-9]*}} x i8]* @"\01L_selector_data(strArgWithS:)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([11 x i8], [11 x i8]* [[SETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast (void ([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*)* @"$s11objc_bridge3BasC6strArg1sySS_tFTo" to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([12 x i8], [12 x i8]* @"\01L_selector_data(nsstrResult)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([8 x i8], [8 x i8]* [[GETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast ([[OPAQUE:.*]]* ([[OPAQUE:.*]]*, i8*)* @"$s11objc_bridge3BasC11nsstrResultSo8NSStringCyFTo" to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([{{[0-9]+}} x i8], [{{[0-9]+}} x i8]* @"\01L_selector_data(nsstrArgWithS:)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([11 x i8], [11 x i8]* [[SETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast (void ([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*)* @"$s11objc_bridge3BasC8nsstrArg1sySo8NSStringC_tFTo" to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } { 
// CHECK:       i8* getelementptr inbounds ([5 x i8], [5 x i8]* @"\01L_selector_data(init)", i64 0, i64 0), 
// CHECK:       i8* getelementptr inbounds ([8 x i8], [8 x i8]* [[GETTER_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast ([[OPAQUE:.*]]* ([[OPAQUE:.*]]*, i8*)* @"$s11objc_bridge3BasCACycfcTo" to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } { 
// CHECK:       i8* getelementptr inbounds ([8 x i8], [8 x i8]* @"\01L_selector_data(dealloc)", i64 0, i64 0), 
// CHECK:       i8* getelementptr inbounds ([8 x i8], [8 x i8]* [[DEALLOC_SIGNATURE]], i64 0, i64 0),
// CHECK:       i8* bitcast (void ([[OPAQUE:.*]]*, i8*)* @"$s11objc_bridge3BasCfDTo" to i8*)
// CHECK:     },
// CHECK:     { i8*, i8*, i8* } {
// CHECK:       i8* getelementptr inbounds ([11 x i8], [11 x i8]* @"\01L_selector_data(acceptSet:)", i64 0, i64 0),
// CHECK:       i8* getelementptr inbounds ([11 x i8], [11 x i8]* @{{[0-9]+}}, i64 0, i64 0),
// CHECK:       i8* bitcast (void (%3*, i8*, %4*)* @"$s11objc_bridge3BasC9acceptSetyyShyACSo8NSObjectCSH10ObjectiveCg_GFTo" to i8*)
// CHECK:     }
// CHECK:     { i8*, i8*, i8* } { 
// CHECK:       i8* getelementptr inbounds ([14 x i8], [14 x i8]* @"\01L_selector_data(.cxx_destruct)", i64 0, i64 0), 
// CHECK:       i8* getelementptr inbounds ([8 x i8], [8 x i8]* @{{.*}}, i64 0, i64 0),
// CHECK:       i8* bitcast (void ([[OPAQUE:.*]]*, i8*)* @"$s11objc_bridge3BasCfETo" to i8*)
// CHECK:     }
// CHECK:   ]
// CHECK: }, section "__DATA, __objc_const", align 8

// CHECK: @_PROPERTIES__TtC11objc_bridge3Bas = private constant { i32, i32, [5 x { i8*, i8* }] } {

// CHECK: [[OBJC_BLOCK_PROPERTY:@.*]] = private unnamed_addr constant [11 x i8] c"T@?,N,C,Vx\00"
// CHECK: @_PROPERTIES__TtC11objc_bridge21OptionalBlockProperty = private constant {{.*}} [[OBJC_BLOCK_PROPERTY]]

func getDescription(_ o: NSObject) -> String {
  return o.description
}

func getUppercaseString(_ s: NSString) -> String {
  return s.uppercase()
}

// @interface Foo -(void) setFoo: (NSString*)s; @end
func setFoo(_ f: Foo, s: String) {
  f.setFoo(s)
}

// NSString *bar(int);
func callBar() -> String {
  return bar(0)
}

// void setBar(NSString *s);
func callSetBar(_ s: String) {
  setBar(s)
}

var NSS : NSString = NSString()

// -- NSString methods don't convert 'self'
extension NSString {
  // CHECK: define internal [[OPAQUE:.*]]* @"$sSo8NSStringC11objc_bridgeE13nsstrFakePropABvgTo"([[OPAQUE:.*]]*, i8*) unnamed_addr
  // CHECK: define internal void @"$sSo8NSStringC11objc_bridgeE13nsstrFakePropABvsTo"([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*) unnamed_addr
  @objc var nsstrFakeProp : NSString {
    get {
      return NSS
    }
    set {}
  }

  // CHECK: define internal [[OPAQUE:.*]]* @"$sSo8NSStringC11objc_bridgeE11nsstrResultAByFTo"([[OPAQUE:.*]]*, i8*) unnamed_addr
  @objc func nsstrResult() -> NSString { return NSS }

  // CHECK: define internal void @"$sSo8NSStringC11objc_bridgeE8nsstrArg1syAB_tFTo"([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*) unnamed_addr
  @objc func nsstrArg(s s: NSString) { }
}

class Bas : NSObject {
  // CHECK: define internal [[OPAQUE:.*]]* @"$s11objc_bridge3BasC11strRealPropSSvgTo"([[OPAQUE:.*]]*, i8*) unnamed_addr {{.*}} {
  // CHECK: define internal void @"$s11objc_bridge3BasC11strRealPropSSvsTo"([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*) unnamed_addr {{.*}} {
  @objc var strRealProp : String

  // CHECK: define internal [[OPAQUE:.*]]* @"$s11objc_bridge3BasC11strFakePropSSvgTo"([[OPAQUE:.*]]*, i8*) unnamed_addr {{.*}} {
  // CHECK: define internal void @"$s11objc_bridge3BasC11strFakePropSSvsTo"([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*) unnamed_addr {{.*}} {
  @objc var strFakeProp : String {
    get {
      return ""
    }
    set {}
  }

  // CHECK: define internal [[OPAQUE:.*]]* @"$s11objc_bridge3BasC13nsstrRealPropSo8NSStringCvgTo"([[OPAQUE:.*]]*, i8*) unnamed_addr {{.*}} {
  // CHECK: define internal void @"$s11objc_bridge3BasC13nsstrRealPropSo8NSStringCvsTo"([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*) unnamed_addr {{.*}} {
  @objc var nsstrRealProp : NSString

  // CHECK: define hidden swiftcc %TSo8NSStringC* @"$s11objc_bridge3BasC13nsstrFakePropSo8NSStringCvg"(%T11objc_bridge3BasC* swiftself) {{.*}} {
  // CHECK: define internal void @"$s11objc_bridge3BasC13nsstrFakePropSo8NSStringCvsTo"([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*) unnamed_addr {{.*}} {
  @objc var nsstrFakeProp : NSString {
    get {
      return NSS
    }
    set {}
  }

  // CHECK: define internal [[OPAQUE:.*]]* @"$s11objc_bridge3BasC9strResultSSyFTo"([[OPAQUE:.*]]*, i8*) unnamed_addr {{.*}} {
  @objc func strResult() -> String { return "" }
  // CHECK: define internal void @"$s11objc_bridge3BasC6strArg1sySS_tFTo"([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*) unnamed_addr {{.*}} {
  @objc func strArg(s s: String) { }

  // CHECK: define internal [[OPAQUE:.*]]* @"$s11objc_bridge3BasC11nsstrResultSo8NSStringCyFTo"([[OPAQUE:.*]]*, i8*) unnamed_addr {{.*}} {
  @objc func nsstrResult() -> NSString { return NSS }
  // CHECK: define internal void @"$s11objc_bridge3BasC8nsstrArg1sySo8NSStringC_tFTo"([[OPAQUE:.*]]*, i8*, [[OPAQUE:.*]]*) unnamed_addr {{.*}} {
  @objc func nsstrArg(s s: NSString) { }

  override init() { 
    strRealProp = String()
    nsstrRealProp = NSString()
    super.init()
  }

  deinit { var x = 10 }

  override var hashValue: Int { return 0 }

  @objc func acceptSet(_ set: Set<Bas>) { }
}

func ==(lhs: Bas, rhs: Bas) -> Bool { return true }

class OptionalBlockProperty: NSObject {
  @objc var x: (([AnyObject]) -> [AnyObject])?
}
