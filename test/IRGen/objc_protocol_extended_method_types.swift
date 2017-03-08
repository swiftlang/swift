// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

import Foundation

// CHECK: [[INIT:@.*]] = private unnamed_addr constant [8 x i8] c"@16@0:8\00"
// CHECK: [[NSNUMBER:@.*]] = private unnamed_addr constant [31 x i8] c"@\22NSNumber\2224@0:8@\22NSNumber\2216\00"
// CHECK: [[NSMUTABLEARRAY_GET:@.*]] = private unnamed_addr constant [24 x i8] c"@\22NSMutableArray\2216@0:8\00"
// CHECK: [[NSMUTABLEARRAY_SET:@.*]] = private unnamed_addr constant [27 x i8] c"v24@0:8@\22NSMutableArray\2216\00"
// CHECK: [[SUBSCRIPT:@.*]] = private unnamed_addr constant [11 x i8] c"q24@0:8q16\00"
// CHECK: [[NSSTRING:@.*]] = private unnamed_addr constant [31 x i8] c"@\22NSString\2224@0:8@\22NSString\2216\00"
// CHECK: [[NSOBJECT:@.*]] = private unnamed_addr constant [31 x i8] c"@\22NSObject\2224@0:8@\22NSObject\2216\00"
// CHECK: [[NSMUTABLESTRING:@.*]] = private unnamed_addr constant [28 x i8] c"v24@0:8@\22NSMutableString\2216\00"

// CHECK-LABEL: @_PROTOCOL_METHOD_TYPES__TtP35objc_protocol_extended_method_types1P_ = private constant [16 x i8*] [
// -- required instance methods:
//   -- requiredInstanceMethod
// CHECK:         i8* getelementptr inbounds ([31 x i8], [31 x i8]* [[NSNUMBER]], i64 0, i64 0),
//   -- requiredInstanceProperty getter
// CHECK:         i8* getelementptr inbounds ([24 x i8], [24 x i8]* [[NSMUTABLEARRAY_GET]], i64 0, i64 0),
//   -- requiredInstanceProperty setter
// CHECK:         i8* getelementptr inbounds ([27 x i8], [27 x i8]* [[NSMUTABLEARRAY_SET]], i64 0, i64 0),
//   -- requiredROInstanceProperty getter
// CHECK:         i8* getelementptr inbounds ([24 x i8], [24 x i8]* [[NSMUTABLEARRAY_GET]], i64 0, i64 0),
//   -- requiredInstanceMethod2
// CHECK:         i8* getelementptr inbounds ([31 x i8], [31 x i8]* [[NSNUMBER]], i64 0, i64 0),
//   -- requiredInstanceProperty2 getter
// CHECK:         i8* getelementptr inbounds ([24 x i8], [24 x i8]* [[NSMUTABLEARRAY_GET]], i64 0, i64 0),
//   -- requiredInstanceProperty2 setter
// CHECK:         i8* getelementptr inbounds ([27 x i8], [27 x i8]* [[NSMUTABLEARRAY_SET]], i64 0, i64 0),
//   -- requiredROInstanceProperty2 getter
// CHECK:         i8* getelementptr inbounds ([24 x i8], [24 x i8]* [[NSMUTABLEARRAY_GET]], i64 0, i64 0),
//   -- subscript getter
// CHECK:         i8* getelementptr inbounds ([11 x i8], [11 x i8]* [[SUBSCRIPT]], i64 0, i64 0),
//   -- init
// CHECK:         i8* getelementptr inbounds ([8 x i8], [8 x i8]* [[INIT]], i64 0, i64 0),
// -- required class methods:
//   -- requiredClassMethod
// CHECK:         i8* getelementptr inbounds ([31 x i8], [31 x i8]* [[NSSTRING]], i64 0, i64 0),
//   -- requiredClassMethod2
// CHECK:         i8* getelementptr inbounds ([31 x i8], [31 x i8]* [[NSSTRING]], i64 0, i64 0),
// -- optional instance methods:
//   -- optionalInstanceMethod
// CHECK:         i8* getelementptr inbounds ([31 x i8], [31 x i8]* [[NSOBJECT]], i64 0, i64 0),
//   -- optionalInstanceMethod2
// CHECK:         i8* getelementptr inbounds ([31 x i8], [31 x i8]* [[NSOBJECT]], i64 0, i64 0),
// -- optional class methods:
//   -- optionalClassMethod
// CHECK:         i8* getelementptr inbounds ([28 x i8], [28 x i8]* [[NSMUTABLESTRING]], i64 0, i64 0),
//   -- optionalClassMethod2
// CHECK:         i8* getelementptr inbounds ([28 x i8], [28 x i8]* [[NSMUTABLESTRING]], i64 0, i64 0)
// CHECK:       ]

@objc protocol P {
  func requiredInstanceMethod(_ o: NSNumber) -> NSNumber
  @objc optional func optionalInstanceMethod(_ o: NSObject) -> NSObject
  static func requiredClassMethod(_ o: NSString) -> NSString
  @objc optional static func optionalClassMethod(_ o: NSMutableString)
  var requiredInstanceProperty: NSMutableArray { get set }
  var requiredROInstanceProperty: NSMutableArray { get }

  func requiredInstanceMethod2(_ o: NSNumber) -> NSNumber
  @objc optional func optionalInstanceMethod2(_ o: NSObject) -> NSObject
  static func requiredClassMethod2(_ o: NSString) -> NSString
  @objc optional static func optionalClassMethod2(_ o: NSMutableString)
  var requiredInstanceProperty2: NSMutableArray { get set }
  var requiredROInstanceProperty2: NSMutableArray { get }

  subscript(x: Int) -> Int { get }

  init()
}

