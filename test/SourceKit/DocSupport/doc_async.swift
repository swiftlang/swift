// RUN: %empty-directory(%t.mod)
// RUN: %swift -enable-experimental-concurrency -emit-module -o %t.mod/async.swiftmodule %s -parse-as-library -emit-module-doc-path %t.mod/async.swiftdoc
// RUN: %sourcekitd-test -req=doc-info -module async -- -I %t.mod | %FileCheck %s

// REQUIRES: concurrency

public protocol AsyncProto {
  func protoAsyncFunc() async
  // CHECK: key.usr: "s:5async10AsyncProtoP05protoB4FuncyyYaF"
  // CHECK-NOT: }
  // CHECK: key.is_async: 1
  // CHECK: }
  func protoNonAsyncFunc()
  // CHECK: key.usr: "s:5async10AsyncProtoP08protoNonB4FuncyyF"
  // CHECK-NOT: key.is_async: 1
  // CHECK: }
}

public struct AsyncStruct: AsyncProto {
  public func structAsyncFunc() async { }
  // CHECK: key.usr: "s:5async11AsyncStructV06structB4FuncyyYaF"
  // CHECK-NOT: }
  // CHECK: key.is_async: 1
  // CHECK: }
  public func structNonAsyncFunc() { }
  // CHECK: key.usr: "s:5async11AsyncStructV09structNonB4FuncyyF"
  // CHECK-NOT: key.is_async: 1
  // CHECK: }

  public func protoAsyncFunc() async { }
  // CHECK: key.usr: "s:5async11AsyncStructV05protoB4FuncyyYaF"
  // CHECK-NOT: }
  // CHECK: key.conforms
  // CHECK: {
  // CHECK: key.usr: "s:5async10AsyncProtoP05protoB4FuncyyYaF"
  // CHECK-NOT: }
  // CHECK: key.is_async: 1
  // CHECK: }
  // CHECK: key.is_async: 1
  // CHECK: }
  public func protoNonAsyncFunc() { }
  // CHECK: key.usr: "s:5async11AsyncStructV08protoNonB4FuncyyF"
  // CHECK-NOT: key.is_async: 1
  // CHECK: }
}

public func topLevelAsyncFunc() async { }
// CHECK: key.usr: "s:5async17topLevelAsyncFuncyyYaF"
// CHECK-NOT: }
// CHECK: key.is_async: 1
// CHECK: }
public func topLevelNonAsyncFunc() { }
// CHECK: key.usr: "s:5async20topLevelNonAsyncFuncyyF"
// CHECK-NOT: key.is_async: 1
// CHECK: }

