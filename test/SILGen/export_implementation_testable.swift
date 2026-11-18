// RUN: %target-swift-emit-silgen -enable-testing -module-name Test -package-name Package %s | %FileCheck %s --check-prefixes=CHECK,TESTING
// RUN: %target-swift-emit-silgen -module-name Test -package-name Package %s | %FileCheck %s --check-prefixes=CHECK,NO-TESTING

// Under -enable-testing, the effective access of a member of an internal type
// is raised to public, but the member's body is not serialized because the
// formal access scope of the member is still internal. @export(implementation)
// must be ignored for linkage in that case, just like it is without
// -enable-testing.

// CHECK-LABEL: sil non_abi [serialized] [export_implementation] [ossa] @$s4Test16topLevelInternals6UInt32VyF : $@convention(thin) () -> UInt32
@export(implementation)
func topLevelInternal() -> UInt32 { 0 }

// CHECK-LABEL: sil non_abi [serialized] [export_implementation] [ossa] @$s4Test14topLevelPublics6UInt32VyF : $@convention(thin) () -> UInt32
@export(implementation)
public func topLevelPublic() -> UInt32 { 0 }

public struct PublicStruct {
  // CHECK-LABEL: sil non_abi [serialized] [export_implementation] [ossa] @$s4Test12PublicStructV6methods6UInt32VyF : $@convention(method) (PublicStruct) -> UInt32
  @export(implementation)
  public func method() -> UInt32 { 0 }
}

struct InternalStruct {
  // TESTING-LABEL: sil [export_implementation] [ossa] @$s4Test14InternalStructV10exportImpls6UInt32VyF : $@convention(method) (InternalStruct) -> UInt32
  // NO-TESTING-LABEL: sil hidden [export_implementation] [ossa] @$s4Test14InternalStructV10exportImpls6UInt32VyF : $@convention(method) (InternalStruct) -> UInt32
  @export(implementation)
  func exportImpl() -> UInt32 { 0 }

  // TESTING-LABEL: sil [export_implementation] [always_inline] [ossa] @$s4Test14InternalStructV12alwaysInlines6UInt32VyF : $@convention(method) (InternalStruct) -> UInt32
  // NO-TESTING-LABEL: sil hidden [export_implementation] [always_inline] [ossa] @$s4Test14InternalStructV12alwaysInlines6UInt32VyF : $@convention(method) (InternalStruct) -> UInt32
  @inline(always)
  @export(implementation)
  func alwaysInline() -> UInt32 { 0 }

  // TESTING-LABEL: sil [ossa] @$s4Test14InternalStructV8propertySivg : $@convention(method) (InternalStruct) -> Int
  // NO-TESTING-LABEL: sil hidden [ossa] @$s4Test14InternalStructV8propertySivg : $@convention(method) (InternalStruct) -> Int
  // TESTING-LABEL: sil [ossa] @$s4Test14InternalStructV8propertySivs : $@convention(method) (Int, @inout InternalStruct) -> ()
  // NO-TESTING-LABEL: sil hidden [ossa] @$s4Test14InternalStructV8propertySivs : $@convention(method) (Int, @inout InternalStruct) -> ()
  @export(implementation)
  var property: Int {
    get { 0 }
    set { }
  }

  // TESTING-LABEL: sil [ossa] @$s4Test14InternalStructVyS2icig : $@convention(method) (Int, InternalStruct) -> Int
  // NO-TESTING-LABEL: sil hidden [ossa] @$s4Test14InternalStructVyS2icig : $@convention(method) (Int, InternalStruct) -> Int
  @export(implementation)
  subscript(i: Int) -> Int { i }

  // TESTING-LABEL: sil [export_implementation] [ossa] @$s4Test14InternalStructV11withDefaultyS2iFfA_ : $@convention(thin) () -> Int
  // NO-TESTING-LABEL: sil hidden [export_implementation] [ossa] @$s4Test14InternalStructV11withDefaultyS2iFfA_ : $@convention(thin) () -> Int
  // TESTING-LABEL: sil [export_implementation] [ossa] @$s4Test14InternalStructV11withDefaultyS2iF : $@convention(method) (Int, InternalStruct) -> Int
  // NO-TESTING-LABEL: sil hidden [export_implementation] [ossa] @$s4Test14InternalStructV11withDefaultyS2iF : $@convention(method) (Int, InternalStruct) -> Int
  @export(implementation)
  func withDefault(_ x: Int = 3) -> Int { x }

  public struct NestedPublicStruct {
    // TESTING-LABEL: sil [export_implementation] [ossa] @$s4Test14InternalStructV012NestedPublicC0V6methodSiyF : $@convention(method) (InternalStruct.NestedPublicStruct) -> Int
    // NO-TESTING-LABEL: sil hidden [export_implementation] [ossa] @$s4Test14InternalStructV012NestedPublicC0V6methodSiyF : $@convention(method) (InternalStruct.NestedPublicStruct) -> Int
    @export(implementation)
    public func method() -> Int { 0 }
  }
}

extension InternalStruct {
  // TESTING-LABEL: sil [export_implementation] [ossa] @$s4Test14InternalStructV15extensionMethodSiyF : $@convention(method) (InternalStruct) -> Int
  // NO-TESTING-LABEL: sil hidden [export_implementation] [ossa] @$s4Test14InternalStructV15extensionMethodSiyF : $@convention(method) (InternalStruct) -> Int
  @export(implementation)
  func extensionMethod() -> Int { 0 }
}

extension InternalStruct.NestedPublicStruct {
  // TESTING-LABEL: sil [export_implementation] [ossa] @$s4Test14InternalStructV012NestedPublicC0V15extensionMethodSiyF : $@convention(method) (InternalStruct.NestedPublicStruct) -> Int
  // NO-TESTING-LABEL: sil hidden [export_implementation] [ossa] @$s4Test14InternalStructV012NestedPublicC0V15extensionMethodSiyF : $@convention(method) (InternalStruct.NestedPublicStruct) -> Int
  @export(implementation)
  public func extensionMethod() -> Int { 0 }
}

package struct PackageStruct {
  // TESTING-LABEL: sil [export_implementation] [ossa] @$s4Test13PackageStructV6methodSiyF : $@convention(method) (PackageStruct) -> Int
  // NO-TESTING-LABEL: sil package [export_implementation] [ossa] @$s4Test13PackageStructV6methodSiyF : $@convention(method) (PackageStruct) -> Int
  @export(implementation)
  package func method() -> Int { 0 }
}

internal struct InternalStructWithPackageMember {
  // TESTING-LABEL: sil [export_implementation] [ossa] @$s4Test31InternalStructWithPackageMemberV6methodSiyF : $@convention(method) (InternalStructWithPackageMember) -> Int
  // NO-TESTING-LABEL: sil hidden [export_implementation] [ossa] @$s4Test31InternalStructWithPackageMemberV6methodSiyF : $@convention(method) (InternalStructWithPackageMember) -> Int
  @export(implementation)
  package func method() -> Int { 0 }
}
