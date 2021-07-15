// REQUIRES: concurrency
// REQUIRES: objc_interop

// RUN: %sourcekitd-test -req=doc-info %s -- -module-name asyncmod -target %target-triple | %FileCheck %s

// RUN: %sourcekitd-test -req=doc-info -module asyncmod -- -target %target-triple %clang-importer-sdk-nosource -I %S/Inputs/async/ | %FileCheck %s --check-prefix=CHECK-OBJC
// CHECK-OBJC: key.name: "method()",
// CHECK-OBJC: key.usr: "c:objc(cs)AsyncImports(im)methodWithCompletion:"
// CHECK-OBJC-NOT: },
// CHECK-OBJC: key.is_async: 1
// CHECK-OBJC: },

// CHECK-OBJC: key.name: "asyncProp",
// CHECK-OBJC: key.usr: "c:objc(cs)AsyncImports(im)propWithCompletion:"
// CHECK-OBJC: key.fully_annotated_decl:
// CHECK-OBJC-NEXT: key.is_async: 1
// CHECK-OBJC: }

public protocol AsyncProto {
  // CHECK: key.usr: "s:8asyncmod10AsyncProtoP05protoB4PropSivp"
  // CHECK-NOT: },
  // CHECK: key.is_async: 1
  // CHECK: },
  var protoAsyncProp: Int { get async }
  // CHECK: key.usr: "s:8asyncmod10AsyncProtoP08protoNonB4PropSivp"
  // CHECK-NOT: key.is_async: 1
  // CHECK: },
  var protoNonAsyncProp: Int { get }

  func protoAsyncFunc() async
  // CHECK: key.usr: "s:8asyncmod10AsyncProtoP05protoB4FuncyyYaF"
  // CHECK-NOT: }
  // CHECK: key.is_async: 1
  // CHECK: }
  func protoNonAsyncFunc()
  // CHECK: key.usr: "s:8asyncmod10AsyncProtoP08protoNonB4FuncyyF"
  // CHECK-NOT: key.is_async: 1
  // CHECK: }
}

public struct AsyncStruct: AsyncProto {
  // CHECK: key.usr: "s:8asyncmod11AsyncStructV05protoB4PropSivp"
  // CHECK-NOT: },
  // CHECK: key.is_async: 1
  // CHECK: },
  public var protoAsyncProp: Int { get async { return 1 } }
  // CHECK: key.usr: "s:8asyncmod11AsyncStructV08protoNonB4PropSivp"
  // CHECK-NOT: key.is_async: 1
  // CHECK: },
  public var protoNonAsyncProp: Int

  public func structAsyncFunc() async { }
  // CHECK: key.usr: "s:8asyncmod11AsyncStructV06structB4FuncyyYaF"
  // CHECK-NOT: }
  // CHECK: key.is_async: 1
  // CHECK: }
  public func structNonAsyncFunc() { }
  // CHECK: key.usr: "s:8asyncmod11AsyncStructV09structNonB4FuncyyF"
  // CHECK-NOT: key.is_async: 1
  // CHECK: }

  public func protoAsyncFunc() async { }
  // CHECK: key.usr: "s:8asyncmod11AsyncStructV05protoB4FuncyyYaF"
  // CHECK-NOT: }
  // CHECK: key.conforms
  // CHECK: {
  // CHECK: key.usr: "s:8asyncmod10AsyncProtoP05protoB4FuncyyYaF"
  // CHECK-NOT: }
  // CHECK: key.is_async: 1
  // CHECK: }
  // CHECK: key.is_async: 1
  // CHECK: }
  public func protoNonAsyncFunc() { }
  // CHECK: key.usr: "s:8asyncmod11AsyncStructV08protoNonB4FuncyyF"
  // CHECK-NOT: key.is_async: 1
  // CHECK: }
}

public func topLevelAsyncFunc() async { }
// CHECK: key.usr: "s:8asyncmod17topLevelAsyncFuncyyYaF"
// CHECK-NOT: }
// CHECK: key.is_async: 1
// CHECK: }
public func topLevelNonAsyncFunc() { }
// CHECK: key.usr: "s:8asyncmod20topLevelNonAsyncFuncyyF"
// CHECK-NOT: key.is_async: 1
// CHECK: }

