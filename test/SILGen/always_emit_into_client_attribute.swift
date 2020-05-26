// RUN: %target-swift-emit-silgen -primary-file %s %S/Inputs/always_emit_into_client_other_file.swift | %FileCheck %s

// CHECK-LABEL: sil non_abi [serialized] [ossa] @$s33always_emit_into_client_attribute0A22EmitIntoClientFunctionyyF : $@convention(thin) () -> ()
@_alwaysEmitIntoClient public func alwaysEmitIntoClientFunction() {
  alwaysEmitIntoClientOtherFunction()
}

// CHECK: sil hidden_external [serialized] @$s33always_emit_into_client_attribute0A27EmitIntoClientOtherFunctionyyF : $@convention(thin) () -> ()

// CHECK-LABEL: sil non_abi [serialized] [ossa] @$s33always_emit_into_client_attribute26implicitlyUsableFromInlineyyF : $@convention(thin) () -> ()
@_alwaysEmitIntoClient func implicitlyUsableFromInline() {
  alwaysEmitIntoClientOtherFunction()
}

public struct S {
  // CHECK-LABEL: sil non_abi [serialized] [ossa] @$s33always_emit_into_client_attribute1SV8propertySivg : $@convention(method) (S) -> Int
  // CHECK-LABEL: sil non_abi [serialized] [ossa] @$s33always_emit_into_client_attribute1SV8propertySivs : $@convention(method) (Int, @inout S) -> ()
  // CHECK-LABEL: sil non_abi [transparent] [serialized] [ossa] @$s33always_emit_into_client_attribute1SV8propertySivM : $@yield_once @convention(method) (@inout S) -> @yields @inout Int
  @_alwaysEmitIntoClient public var property: Int {
    get { return 0 }
    set { }
  }

  // CHECK-LABEL: sil non_abi [serialized] [ossa] @$s33always_emit_into_client_attribute1SVyS2icig : $@convention(method) (Int, S) -> Int
  // CHECK-LABEL: sil non_abi [serialized] [ossa] @$s33always_emit_into_client_attribute1SVyS2icis : $@convention(method) (Int, Int, @inout S) -> ()
  // CHECK-LABEL: sil non_abi [transparent] [serialized] [ossa] @$s33always_emit_into_client_attribute1SVyS2iciM : $@yield_once @convention(method) (Int, @inout S) -> @yields @inout Int
  @_alwaysEmitIntoClient public subscript(x: Int) -> Int {
    get { return 0 }
    set { }
  }
}
