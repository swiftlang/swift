// RUN: %empty-directory(%t)
//
// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s

protocol SwiftProto {
// CHECK: [[@LINE-1]]:10 | protocol/Swift | SwiftProto | [[Proto_USR:.*]] | Def | rel: 0
  static func staticMethod()
  // CHECK: [[@LINE-1]]:15 | static-method/Swift | staticMethod() | [[ProtoStaticMethod_USR:.*]] | Def,RelChild | rel: 1
}

protocol SwiftProtoSame {
// CHECK: [[@LINE-1]]:10 | protocol/Swift | SwiftProtoSame | [[ProtoSame_USR:.*]] | Def | rel: 0
  static func staticMethod()
  // CHECK: [[@LINE-1]]:15 | static-method/Swift | staticMethod() | [[ProtoSameStaticMethod_USR:.*]] | Def,RelChild | rel: 1
}

protocol SwiftProtoOther {}

protocol SwiftProtoComposed: SwiftProtoSame, SwiftProto, SwiftProtoOther {}

class SwiftClass: SwiftProtoComposed {
// CHECK: [[@LINE-1]]:7 | class/Swift | SwiftClass | [[Class_USR:.*]] | Def | rel: 0
  static func staticMethod() {}
  // CHECK: [[@LINE-1]]:15 | static-method/Swift | staticMethod() | [[ClassStaticMethod_USR:.*]] | Def,RelChild,RelOver | rel: 3
  class func classMethod() {}
  // CHECK: [[@LINE-1]]:14 | class-method/Swift | classMethod() | [[ClassClassMethod_USR:.*]] | Def,Dyn,RelChild | rel: 1
}

struct SwiftStruct: SwiftProtoComposed {
// CHECK: [[@LINE-1]]:8 | struct/Swift | SwiftStruct | [[Struct_USR:.*]] | Def | rel: 0
  static func staticMethod() {}
  // CHECK: [[@LINE-1]]:15 | static-method/Swift | staticMethod() | [[StructStaticMethod_USR:.*]] | Def,RelChild,RelOver | rel: 3
}

enum SwiftEnum: SwiftProtoComposed {
// CHECK: [[@LINE-1]]:6 | enum/Swift | SwiftEnum | [[Enum_USR:.*]] | Def | rel: 0
  static func staticMethod() {}
  // CHECK: [[@LINE-1]]:15 | static-method/Swift | staticMethod() | [[EnumStaticMethod_USR:.*]] | Def,RelChild,RelOver | rel: 3
}

func directCalls() {
  SwiftClass.staticMethod()
  // CHECK: [[@LINE-1]]:14 | static-method/Swift | staticMethod() | [[ClassStaticMethod_USR]] | Ref,Call,RelRec,RelCall,RelCont | rel: 2
  // CHECK-DAG: RelRec | class/Swift | SwiftClass | [[Class_USR]]
  SwiftClass.classMethod()
  // CHECK: [[@LINE-1]]:14 | class-method/Swift | classMethod() | [[ClassClassMethod_USR]] | Ref,Call,RelRec,RelCall,RelCont | rel: 2
  // CHECK-DAG: RelRec | class/Swift | SwiftClass | [[Class_USR]]

  SwiftStruct.staticMethod()
  // CHECK: [[@LINE-1]]:15 | static-method/Swift | staticMethod() | [[StructStaticMethod_USR]] | Ref,Call,RelRec,RelCall,RelCont | rel: 2
  // CHECK-DAG: RelRec | struct/Swift | SwiftStruct | [[Struct_USR]]

  SwiftEnum.staticMethod()
  // CHECK: [[@LINE-1]]:13 | static-method/Swift | staticMethod() | [[EnumStaticMethod_USR]] | Ref,Call,RelRec,RelCall,RelCont | rel: 2
  // CHECK-DAG: RelRec | enum/Swift | SwiftEnum | [[Enum_USR]]
}

func typeofClass(c: SwiftClass) {
  type(of: c).staticMethod()
  // CHECK: [[@LINE-1]]:15 | static-method/Swift | staticMethod() | [[ClassStaticMethod_USR]] | Ref,Call,RelRec,RelCall,RelCont | rel: 2
  // CHECK: RelRec | class/Swift | SwiftClass | [[Class_USR]]
  type(of: c).classMethod()
  // CHECK: [[@LINE-1]]:15 | class-method/Swift | classMethod() | [[ClassClassMethod_USR]] | Ref,Call,Dyn,RelRec,RelCall,RelCont | rel: 2
  // CHECK: RelRec | class/Swift | SwiftClass | [[Class_USR]]
}

func typeofStruct(s: SwiftStruct) {
  type(of: s).staticMethod()
  // CHECK: [[@LINE-1]]:15 | static-method/Swift | staticMethod() | [[StructStaticMethod_USR]] | Ref,Call,RelRec,RelCall,RelCont | rel: 2
  // CHECK: RelRec | struct/Swift | SwiftStruct | [[Struct_USR]]
}

func typeofEnum(e: SwiftEnum) {
  type(of: e).staticMethod()
  // CHECK: [[@LINE-1]]:15 | static-method/Swift | staticMethod() | [[EnumStaticMethod_USR]] | Ref,Call,RelRec,RelCall,RelCont | rel: 2
  // CHECK: RelRec | enum/Swift | SwiftEnum | [[Enum_USR]]
}

func typeofProtocol(proto: SwiftProto) {
  type(of: proto).staticMethod()
  // CHECK: [[@LINE-1]]:19 | static-method/Swift | staticMethod() | [[ProtoStaticMethod_USR]] | Ref,Call,Dyn,RelRec,RelCall,RelCont | rel: 2
  // CHECK: RelRec | protocol/Swift | SwiftProto | [[Proto_USR]]
}

// FIXME: Add the ReceivedBy relation for generics
func genericSingle<T>(proto: T) where T: SwiftProto {
  type(of: proto).staticMethod()
  // CHECK: [[@LINE-1]]:19 | static-method/Swift | staticMethod() | [[ProtoStaticMethod_USR]] | Ref,Call,Dyn,RelCall,RelCont | rel: 1
}

// FIXME: The composed cases currently picks one of the USRs, should we output
//        multiple occurrences?
func genericComposedType<T>(proto: T) where T: SwiftProtoComposed {
  type(of: proto).staticMethod()
  // CHECK: [[@LINE-1]]:19 | static-method/Swift | staticMethod() | [[ProtoStaticMethod_USR]] | Ref,Call,Dyn,RelCall,RelCont | rel: 1
}

func genericComposedWhere<T>(proto: T) where T: SwiftProto & SwiftProtoSame & SwiftProtoOther {
  type(of: proto).staticMethod()
  // CHECK: [[@LINE-1]]:19 | static-method/Swift | staticMethod() | [[ProtoSameStaticMethod_USR]] | Ref,Call,Dyn,RelCall,RelCont | rel: 1
}
