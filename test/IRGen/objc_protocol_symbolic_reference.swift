// RUN: %target-swift-frontend -enable-objective-c-protocol-symbolic-references -target %target-future-triple -enable-objc-interop -emit-ir -primary-file %s -disable-objc-attr-requires-foundation-module | %FileCheck %s

// REQUIRES: PTRSIZE=64
// REQIURES: objc_interop

@objc protocol P {}


func requireMetadata<T>(_ t: T.Type) {}

public func theTest() {
  requireMetadata(P.self)
}

// CHECK: @"flat unique 32objc_protocol_symbolic_reference1P_p" = linkonce_odr hidden constant <{ [38 x i8], i8 }> <{ [38 x i8] c"32objc_protocol_symbolic_reference1P_p", i8 0 }>

// CHECK: @"\01l_OBJC_PROTOCOL_SYMREF_$__TtP32objc_protocol_symbolic_reference1P_" = linkonce hidden constant <{ i32, i32 }> <{ i32 {{.*}} @"\01l_OBJC_PROTOCOL_REFERENCE_$__TtP32objc_protocol_symbolic_reference1P_" {{.*}} i32 {{.*}} @"flat unique 32objc_protocol_symbolic_reference1P_p"

// CHECK: "symbolic ______p 32objc_protocol_symbolic_reference1PP" = linkonce_odr hidden constant <{ i8, i32, [2 x i8], i8 }> <{ i8 12, i32 {{.*}} @"\01l_OBJC_PROTOCOL_SYMREF_$__TtP32objc_protocol_symbolic_reference1P_"
