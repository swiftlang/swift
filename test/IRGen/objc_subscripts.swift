// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir -enable-objc-interop -disable-objc-attr-requires-foundation-module | %FileCheck %s

// REQUIRES: CPU=x86_64

// CHECK: @_INSTANCE_METHODS__TtC15objc_subscripts10SomeObject = 
// CHECK:   private constant { i32, i32, [5 x { i8*, i8*, i8* }] } 
// CHECK:   { i32 24, i32 5, [5 x { i8*, i8*, i8* }] 
// CHECK:     [
// CHECK:       { i8*, i8*, i8* } 
// CHECK:         { 
// CHECK:           i8* getelementptr inbounds ([26 x i8], [26 x i8]* @"\01L_selector_data(objectAtIndexedSubscript:)", i64 0, i64 0), 
// CHECK:           i8* null, 
// CHECK:           i8* bitcast ([[OPAQUE0:%.*]]* ([[OPAQUE1:%.*]]*, i8*, i64)* @"$S15objc_subscripts10SomeObjectCyACSicigTo" to i8*)
// CHECK:         }, 
// CHECK:       { i8*, i8*, i8* } 
// CHECK:         { 
// CHECK:           i8* getelementptr inbounds ([30 x i8], [30 x i8]* @"\01L_selector_data(setObject:atIndexedSubscript:)", i64 0, i64 0), 
// CHECK:           i8* null, 
// CHECK:           i8* bitcast (void ([[OPAQUE2:%.*]]*, i8*, [[OPAQUE3:%.*]]*, i64)* @"$S15objc_subscripts10SomeObjectCyACSicisTo" to i8*)
// CHECK:         },
// CHECK:       { i8*, i8*, i8* } 
// CHECK:         { 
// CHECK:           i8* getelementptr inbounds ([25 x i8], [25 x i8]* @"\01L_selector_data(objectForKeyedSubscript:)", i64 0, i64 0), 
// CHECK:           i8* null, 
// CHECK:           i8* bitcast (i64 ([[OPAQUE4:%.*]]*, i8*, [[OPAQUE5:%.*]]*)* @"$S15objc_subscripts10SomeObjectCySiACcigTo" to i8*)
// CHECK:         }, 
// CHECK:       { i8*, i8*, i8* } 
// CHECK:         { 
// CHECK:           i8* getelementptr inbounds ([29 x i8], [29 x i8]* @"\01L_selector_data(setObject:forKeyedSubscript:)", i64 0, i64 0), 
// CHECK:           i8* null, 
// CHECK:           i8* bitcast (void ([[OPAQUE6:%.*]]*, i8*, i64, [[OPAQUE7:%.*]]*)* @"$S15objc_subscripts10SomeObjectCySiACcisTo" to i8*)
// CHECK:         },
// CHECK:       { i8*, i8*, i8* } 
// CHECK:         { i8* getelementptr inbounds ([5 x i8], [5 x i8]* @"\01L_selector_data(init)", i64 0, i64 0), i8* getelementptr inbounds ([8 x i8], [8 x i8]* @{{[0-9]+}}, i64 0, i64 0), i8* bitcast ([[OPAQUE8:%.*]]* ([[OPAQUE9:%.*]]*, i8*)* @"$S15objc_subscripts10SomeObjectCACycfcTo" to i8*) }
// CHECK:    ]
// CHECK:  }

@objc class SomeObject {
  @objc subscript (i : Int) -> SomeObject {
    // CHECK: define internal [[OPAQUE0:%.*]]* @"$S15objc_subscripts10SomeObjectCyACSicigTo"([[OPAQUE1]]*, i8*, i64) unnamed_addr
    get {
      // CHECK: call swiftcc %T15objc_subscripts10SomeObjectC* @"$S15objc_subscripts10SomeObjectCyACSicig"
      return self
    }

    // CHECK-LABEL: define internal void @"$S15objc_subscripts10SomeObjectCyACSicisTo"
    set {
      // CHECK: swiftcc void @"$S15objc_subscripts10SomeObjectCyACSicis"
    }
  }

  @objc subscript (s : SomeObject) -> Int {
  // CHECK-LABEL: define internal i64 @"$S15objc_subscripts10SomeObjectCySiACcigTo"
    get {
      // CHECK: call swiftcc i64 @"$S15objc_subscripts10SomeObjectCySiACcig"
      return 5
    }

    // CHECK-LABEL: define internal void @"$S15objc_subscripts10SomeObjectCySiACcisTo"
    set {
      // CHECK: call swiftcc void @"$S15objc_subscripts10SomeObjectCySiACcis"
    }
  }

  @objc init() {}
}

