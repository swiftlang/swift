// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir -disable-objc-attr-requires-foundation-module | %FileCheck %s

// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

// CHECK: @_INSTANCE_METHODS__TtC15objc_subscripts10SomeObject = 
// CHECK:   private constant { i32, i32, [5 x { i8*, i8*, i8* }] } 
// CHECK:   { i32 24, i32 5, [5 x { i8*, i8*, i8* }] 
// CHECK:     [
// CHECK:       { i8*, i8*, i8* } 
// CHECK:         { 
// CHECK:           i8* getelementptr inbounds ([26 x i8], [26 x i8]* @"\01L_selector_data(objectAtIndexedSubscript:)", i64 0, i64 0), 
// CHECK:           i8* null, 
// CHECK:           i8* bitcast ([[OPAQUE0:%.*]]* ([[OPAQUE1:%.*]]*, i8*, i64)* @_T015objc_subscripts10SomeObjectC9subscriptACSicfgTo to i8*)
// CHECK:         }, 
// CHECK:       { i8*, i8*, i8* } 
// CHECK:         { 
// CHECK:           i8* getelementptr inbounds ([30 x i8], [30 x i8]* @"\01L_selector_data(setObject:atIndexedSubscript:)", i64 0, i64 0), 
// CHECK:           i8* null, 
// CHECK:           i8* bitcast (void ([[OPAQUE2:%.*]]*, i8*, [[OPAQUE3:%.*]]*, i64)* @_T015objc_subscripts10SomeObjectC9subscriptACSicfsTo to i8*)
// CHECK:         },
// CHECK:       { i8*, i8*, i8* } 
// CHECK:         { 
// CHECK:           i8* getelementptr inbounds ([25 x i8], [25 x i8]* @"\01L_selector_data(objectForKeyedSubscript:)", i64 0, i64 0), 
// CHECK:           i8* null, 
// CHECK:           i8* bitcast (i64 ([[OPAQUE4:%.*]]*, i8*, [[OPAQUE5:%.*]]*)* @_T015objc_subscripts10SomeObjectC9subscriptSiACcfgTo to i8*)
// CHECK:         }, 
// CHECK:       { i8*, i8*, i8* } 
// CHECK:         { 
// CHECK:           i8* getelementptr inbounds ([29 x i8], [29 x i8]* @"\01L_selector_data(setObject:forKeyedSubscript:)", i64 0, i64 0), 
// CHECK:           i8* null, 
// CHECK:           i8* bitcast (void ([[OPAQUE6:%.*]]*, i8*, i64, [[OPAQUE7:%.*]]*)* @_T015objc_subscripts10SomeObjectC9subscriptSiACcfsTo to i8*)
// CHECK:         },
// CHECK:       { i8*, i8*, i8* } 
// CHECK:         { i8* getelementptr inbounds ([5 x i8], [5 x i8]* @"\01L_selector_data(init)", i64 0, i64 0), i8* getelementptr inbounds ([8 x i8], [8 x i8]* @{{[0-9]+}}, i64 0, i64 0), i8* bitcast ([[OPAQUE8:%.*]]* ([[OPAQUE9:%.*]]*, i8*)* @_T015objc_subscripts10SomeObjectCACycfcTo to i8*) }
// CHECK:    ]
// CHECK:  }, section "__DATA, __objc_const", align 8

@objc class SomeObject {
  subscript (i : Int) -> SomeObject {
    // CHECK: define internal [[OPAQUE0:%.*]]* @_T015objc_subscripts10SomeObjectC9subscriptACSicfgTo([[OPAQUE1]]*, i8*, i64) unnamed_addr
    get {
      // CHECK: call swiftcc %T15objc_subscripts10SomeObjectC* @_T015objc_subscripts10SomeObjectC9subscriptACSicfg
      return self
    }

    // CHECK-LABEL: define internal void @_T015objc_subscripts10SomeObjectC9subscriptACSicfsTo
    set {
      // CHECK: swiftcc void @_T015objc_subscripts10SomeObjectC9subscriptACSicfs
    }
  }

  subscript (s : SomeObject) -> Int {
  // CHECK-LABEL: define internal i64 @_T015objc_subscripts10SomeObjectC9subscriptSiACcfgTo
    get {
      // CHECK: call swiftcc i64 @_T015objc_subscripts10SomeObjectC9subscriptSiACcfg
      return 5
    }

    // CHECK-LABEL: define internal void @_T015objc_subscripts10SomeObjectC9subscriptSiACcfsTo
    set {
      // CHECK: call swiftcc void @_T015objc_subscripts10SomeObjectC9subscriptSiACcfs
    }
  }
}

