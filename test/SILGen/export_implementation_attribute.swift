// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values -primary-file %s %S/Inputs/export_implementation_other_file.swift -package-name Package
// RUN: %target-swift-emit-silgen -primary-file %s %S/Inputs/export_implementation_other_file.swift -package-name Package | %FileCheck %s

// CHECK-LABEL: sil non_abi [serialized] [export_implementation] [ossa] @$s31export_implementation_attribute0A22ImplementationFunctionyyF : $@convention(thin) () -> ()
@export(implementation) public func exportImplementationFunction() {
  exportImplementationOtherFunction()
}

// CHECK: sil hidden_external [serialized] [export_implementation] @$s31export_implementation_attribute0A27ImplementationOtherFunctionyyF : $@convention(thin) () -> ()

// CHECK-LABEL: sil non_abi [serialized] [export_implementation] [ossa] @$s31export_implementation_attribute26implicitlyUsableFromInlineyyF : $@convention(thin) () -> ()
@export(implementation) func implicitlyUsableFromInline() {
  exportImplementationOtherFunction()
}

// CHECK-LABEL: sil non_abi [serialized] [export_implementation] [ossa] @$s31export_implementation_attribute35packageExportImplementationFunctionyyF : $@convention(thin) () -> ()
@export(implementation) package func packageExportImplementationFunction() {
  exportImplementationOtherFunction()
}

// FIXME: @export(implementation) should not be allowed on private decls
// CHECK-LABEL: sil non_abi [serialized] [export_implementation] [ossa] @$s31export_implementation_attribute35privateExportImplementationFunction33_B7449A415F7FAB1D9EFA03D2F0633EF8LLyyF : $@convention(thin) () -> ()
@export(implementation) private func privateExportImplementationFunction() {
  exportImplementationOtherFunction()
}

public struct S {
  // CHECK-LABEL: sil non_abi [serialized] [export_implementation] [ossa] @$s31export_implementation_attribute1SV8propertySivg : $@convention(method) (S) -> Int
  // CHECK-LABEL: sil non_abi [serialized] [export_implementation] [ossa] @$s31export_implementation_attribute1SV8propertySivs : $@convention(method) (Int, @inout S) -> ()
  // CHECK-LABEL: sil non_abi [transparent] [serialized] [export_implementation] [ossa] @$s31export_implementation_attribute1SV8propertySivM : $@yield_once @convention(method) (@inout S) -> @yields @inout Int
  @export(implementation) public var property: Int {
    get { return 0 }
    set { }
  }

  // CHECK-LABEL: sil non_abi [serialized] [export_implementation] [ossa] @$s31export_implementation_attribute1SVyS2icig : $@convention(method) (Int, S) -> Int
  // CHECK-LABEL: sil non_abi [serialized] [export_implementation] [ossa] @$s31export_implementation_attribute1SVyS2icis : $@convention(method) (Int, Int, @inout S) -> ()
  // CHECK-LABEL: sil non_abi [transparent] [serialized] [export_implementation] [ossa] @$s31export_implementation_attribute1SVyS2iciM : $@yield_once @convention(method) (Int, @inout S) -> @yields @inout Int
  @export(implementation) public subscript(x: Int) -> Int {
    get { return 0 }
    set { }
  }
}

public final class C {
  // C.__allocating_init()
  // CHECK-LABEL: sil non_abi [serialized] [exact_self_class] [export_implementation] [ossa] @$s31export_implementation_attribute1CCACycfC : $@convention(method) (@thick C.Type) -> @owned C

  // C.init()
  // CHECK-LABEL: sil non_abi [serialized] [export_implementation] [ossa] @$s31export_implementation_attribute1CCACycfc : $@convention(method) (@owned C) -> @owned C
  @export(implementation)
  public init() {}

  // C.deinit
  // CHECK-LABEL: sil non_abi [serialized] [export_implementation] [ossa] @$s31export_implementation_attribute1CCfd : $@convention(method) (@guaranteed C) -> @owned Builtin.NativeObject

  // C.__deallocating_deinit
  // CHECK-LABEL: sil non_abi [serialized] [export_implementation] [ossa] @$s31export_implementation_attribute1CCfD : $@convention(method) (@owned C) -> ()
  @export(implementation)
  deinit {}
}


// We ignore @export(implementation) if the containing context does not have
// effective public visibility.
internal struct InternalContext {
// CHECK-LABEL: sil hidden [ossa] @$s31export_implementation_attribute15InternalContextV1vSivgZ
  @export(implementation)
  internal static var v : Int { 1 }
}

// We ignore @export(implementation) if the containing context does not have
// effective public visibility.
package struct PackageContext {
// CHECK-LABEL: sil package [ossa] @$s31export_implementation_attribute14PackageContextV1vSivgZ

  @export(implementation)
  package static var v : Int { 1 }
}
