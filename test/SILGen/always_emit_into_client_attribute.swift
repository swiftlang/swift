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