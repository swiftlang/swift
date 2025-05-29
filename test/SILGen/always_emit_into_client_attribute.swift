// RUN: %target-swift-emit-silgen -primary-file %s %S/Inputs/always_emit_into_client_other_file.swift -package-name Package | %FileCheck %s

// CHECK-LABEL: sil non_abi [serialized] [ossa] @$s33always_emit_into_client_attribute0A22EmitIntoClientFunctionyyF : $@convention(thin) () -> ()
@_alwaysEmitIntoClient public func alwaysEmitIntoClientFunction() {
  alwaysEmitIntoClientOtherFunction()
}

// CHECK: sil hidden_external [serialized] @$s33always_emit_into_client_attribute0A27EmitIntoClientOtherFunctionyyF : $@convention(thin) () -> ()

// CHECK-LABEL: sil non_abi [serialized] [ossa] @$s33always_emit_into_client_attribute26implicitlyUsableFromInlineyyF : $@convention(thin) () -> ()
@_alwaysEmitIntoClient func implicitlyUsableFromInline() {
  alwaysEmitIntoClientOtherFunction()
}

// CHECK-LABEL: sil non_abi [serialized] [ossa] @$s33always_emit_into_client_attribute35packageAlwaysEmitIntoClientFunctionyyF : $@convention(thin) () -> ()
@_alwaysEmitIntoClient package func packageAlwaysEmitIntoClientFunction() {
  alwaysEmitIntoClientOtherFunction()
}

// FIXME: @_alwaysEmitIntoClient should not be allowed on private decls
// CHECK-LABEL: sil non_abi [serialized] [ossa] @$s33always_emit_into_client_attribute35privateAlwaysEmitIntoClientFunction33_5D0713A780245A446371C699E6F23C4FLLyyF : $@convention(thin) () -> ()
@_alwaysEmitIntoClient private func privateAlwaysEmitIntoClientFunction() {
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

public final class C {
  // C.__allocating_init()
  // CHECK-LABEL: sil non_abi [serialized] [exact_self_class] [ossa] @$s33always_emit_into_client_attribute1CCACycfC : $@convention(method) (@thick C.Type) -> @owned C

  // C.init()
  // CHECK-LABEL: sil non_abi [serialized] [ossa] @$s33always_emit_into_client_attribute1CCACycfc : $@convention(method) (@owned C) -> @owned C
  @_alwaysEmitIntoClient
  public init() {}

  // C.deinit
  // CHECK-LABEL: sil non_abi [serialized] [ossa] @$s33always_emit_into_client_attribute1CCfd : $@convention(method) (@guaranteed C) -> @owned Builtin.NativeObject

  // C.__deallocating_deinit
  // CHECK-LABEL: sil non_abi [serialized] [ossa] @$s33always_emit_into_client_attribute1CCfD : $@convention(method) (@owned C) -> ()
  @_alwaysEmitIntoClient
  deinit {}
}


// We drop AEIC if the containing context does not have effective public
// visibility.
internal struct InternalContext {
// CHECK-LABEL: sil hidden [ossa] @$s33always_emit_into_client_attribute15InternalContextV1vSivgZ
  @_alwaysEmitIntoClient
  internal static var v : Int { 1 }
}

// We drop AEIC if the containing context does not have effective public
// visibility.
package struct PackageContext {
// CHECK-LABEL: sil package [ossa] @$s33always_emit_into_client_attribute14PackageContextV1vSivgZ

  @_alwaysEmitIntoClient
  package static var v : Int { 1 }
}
