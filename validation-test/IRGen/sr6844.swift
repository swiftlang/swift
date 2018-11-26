// RUN: %target-swift-frontend -emit-ir %s -import-objc-header %S/Inputs/sr6844.h | %FileCheck %s
// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: @_PROTOCOL_INSTANCE_METHODS_MyJSONSerializing = {{.+}} @"\01L_selector_data(JSONKeyPathsByPropertyKey)"
// CHECK: [[OBJC_PROPNAME:@.+]] = private unnamed_addr constant [{{.+}} x i8] c"JSONKeyPathsByPropertyKey\00"
// CHECK: [[PROPKIND:@.+]] = private unnamed_addr constant [{{.+}} x i8] c"T@\22NSDictionary\22,N,R\00"
// CHECK: @_PROTOCOL_PROPERTIES_MyJSONSerializing =
// CHECK-SAME: [[OBJC_PROPNAME]]
// CHECK-SAME: [[PROPKIND]]

// CHECK-LABEL: @_PROTOCOL_INSTANCE_METHODS__TtP6sr684418MyJSONSerializing2_ = {{.+}} @"\01L_selector_data(JSONKeyPathsByPropertyKey2)"
// CHECK: [[SWIFT_PROPNAME:@.+]] = private unnamed_addr constant [{{.+}} x i8] c"JSONKeyPathsByPropertyKey2\00"
// CHECK: @_PROTOCOL_PROPERTIES__TtP6sr684418MyJSONSerializing2_ =
// CHECK-SAME: [[SWIFT_PROPNAME]]
// CHECK-SAME: [[PROPKIND]]

@objc public protocol MyJSONSerializing2 {
  var JSONKeyPathsByPropertyKey2: [String: MyJSONKeyPath]? { get }
}

extension MyJSONAdapter {
  public func model<Model: MyJSONSerializing>(of _: Model.Type = Model.self) throws -> Model {
    return __model() as! Model
  }
  public func model<Model: MyJSONSerializing2>(of _: Model.Type = Model.self) throws -> Model {
    return __model() as! Model
  }
}
