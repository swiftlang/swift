// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift %t/Lib.swift \
// RUN: -module-name=Lib -package-name Pkg \
// RUN: -parse-as-library -emit-module -emit-module-path %t/Lib.swiftmodule -I%t \
// RUN: -Xfrontend -experimental-package-cmo -Xfrontend -experimental-allow-non-resilient-access \
// RUN: -O -wmo -enable-library-evolution
// RUN: %target-sil-opt -sil-print-types %t/Lib.swiftmodule -sil-verify-all -o %t/Lib.sil

// RUN: %target-build-swift -module-name=Main -package-name Pkg -I%t -Xllvm -sil-print-types -emit-sil -O %t/main.swift -o %t/Main.sil

// RUN: %FileCheck %s --check-prefixes=CHECK < %t/Lib.sil
// RUN: %FileCheck %s --check-prefixes=CHECK-MAIN < %t/Main.sil

// RUN: rm -rf %t/Lib.swiftmodule
// RUN: %target-build-swift %t/Lib.swift \
// RUN: -module-name=Lib -package-name Pkg \
// RUN: -parse-as-library -emit-module -emit-module-path %t/Lib.swiftmodule -I%t \
// RUN: -Xfrontend -package-cmo -Xfrontend -allow-non-resilient-access \
// RUN: -O -wmo -enable-library-evolution
// RUN: %target-sil-opt -sil-print-types %t/Lib.swiftmodule -sil-verify-all -o %t/Lib2.sil
// RUN: %FileCheck %s --check-prefixes=CHECK < %t/Lib2.sil


//--- main.swift

import Lib

/// There should be no linker error on a public function 
/// that contains symbols internal to Lib module.
///

// CHECK-MAIN-NOT: s3Lib14createPubClassySixlF
// CHECK-MAIN-NOT: s3Lib19usePubStructKeypathySixlF

// CHECK-MAIN: function_ref @$s3Lib11useInternalySiAA4BaseCF : $@convention(thin) (@guaranteed Base) -> Int
let x = useInternal(Base())

/// Since Base is not serialized, accessing its field should go
/// through `class_method`.
// CHECK-MAIN: class_method {{.*}} : $Base, #Base.baseVarPkg!getter : (Base) -> () -> Int, $@convention(method) (@guaranteed Base) -> Int
let y = usePkg(Base())

/// Since PubKlass is serialized, can access its field directly.
// CHECK-MAIN: struct $Int
// CHECK-MAIN-NEXT: store
let z = usePub(PubKlass())

/// PubKlassWithInternalMember is serialized but its initializer contains
/// an internal field.
// CHECK-MAIN: function_ref @$s3Lib26PubKlassWithInternalMemberCyACSicfc
let w = usePubWithInternalField(PubKlassWithInternalMember(1))

// useInternal(_:)
// CHECK-MAIN: sil @$s3Lib11useInternalySiAA4BaseCF : $@convention(thin) (@guaranteed Base) -> Int

// PubKlassWithInternalMember.__allocating_init(_:)
// CHECK-MAIN-DAG: sil public_external @$s3Lib26PubKlassWithInternalMemberCyACSicfC : $@convention(method) (Int, @thick PubKlassWithInternalMember.Type) -> @owned PubKlassWithInternalMember {

func checkNested() {
  // PubContainer initializer is inlined.
  // CHECK-MAIN: struct $PubContainer ()
  let c = PubContainer()
  // CHECK-MAIN: function_ref @$s3Lib12PubContainerV9pubMemberyxxlF
  print(c.pubMember(27))
}


func checkClasses() {
  /// Inlined
  print(createPubClass(0))

  /// Not inlined as functions below contain private/internal symbols
  // CHECK-MAIN: function_ref @$s3Lib20createPubClass_neverySixlF
  print(createPubClass_never(0))

  // CHECK-MAIN: function_ref @$s3Lib14createPrvClassySixlF
  print(createPrvClass(0))

  // CHECK-MAIN: function_ref @$s3Lib20createPrvClass_neverySixlF
  print(createPrvClass_never(0))
}

func checkKeyPaths() {
  /// Inlined
  // CHECK-MAIN: function_ref @$s3Lib19getPubStructKeypathys7KeyPathCyAA0cD0VSiGxlF
  print(usePubStructKeypath(0))

  /// Not inlined as functions below contain private/internal symbols
  // CHECK-MAIN: function_ref @$s3Lib24useInternalStructKeypathySixlF
  print(useInternalStructKeypath(0))

  // CHECK-MAIN: function_ref @$s3Lib15useClassKeypathySixlF
  print(useClassKeypath(0))
}

checkNested()
checkClasses()
checkKeyPaths()

// CHECK-MAIN-DAG: sil public_external [_semantics "optimize.sil.specialize.generic.never"] @$s3Lib20createPubClass_neverySixlF : $@convention(thin) <T> (@in_guaranteed T) -> Int {
// CHECK-MAIN-DAG: sil @$s3Lib14createPrvClassySixlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> Int
// CHECK-MAIN-DAG: sil [_semantics "optimize.sil.specialize.generic.never"] @$s3Lib20createPrvClass_neverySixlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> Int
// CHECK-MAIN-DAG: sil @$s3Lib12PubContainerV9pubMemberyxxlF : $@convention(method) <τ_0_0> (@in_guaranteed τ_0_0, @in_guaranteed PubContainer) -> @out τ_0_0
// CHECK-MAIN-DAG: sil @$s3Lib24useInternalStructKeypathySixlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> Int
// CHECK-MAIN-DAG: sil @$s3Lib15useClassKeypathySixlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> Int


//--- Lib.swift

/// Package CMO does not serialize this function since
/// it references an internal symbol `baseVarInternal`.
/// If it were [serialized_for_package], it would leak
/// into client and client won't be able to find the
/// symbol `baseVarInternal` since its dispatch thunk
/// was not generated in the first place (due to it
/// being internal).
// CHECK-NOT: s3Lib11useInternalySiAA4BaseCF
public func useInternal(_ arg: Base) -> Int {
  return PubKlass().pkgVar + arg.baseVarInternal.data
}

// CHECK-DAG: sil package [serialized_for_package] [canonical] [ossa] @$s3Lib6usePkgySiAA4BaseCF : $@convention(thin) (@guaranteed Base) -> Int {
package func usePkg(_ arg: Base) -> Int {
  return arg.baseVarPkg
}

// CHECK-DAG: sil [serialized_for_package] [canonical] [ossa] @$s3Lib6usePubySiAA0C5KlassCF : $@convention(thin) (@guaranteed PubKlass) -> Int {
public func usePub(_ arg: PubKlass) -> Int {
  return arg.pubVar + arg.pkgVar
}

// CHECK-DAG: sil [serialized_for_package] [canonical] [ossa] @$s3Lib23usePubWithInternalFieldySiAA0c5KlassdE6MemberCF : $@convention(thin) (@guaranteed PubKlassWithInternalMember) -> Int {
public func usePubWithInternalField(_ arg: PubKlassWithInternalMember) -> Int {
  return arg.pubVar + arg.pkgVar
}

struct InternalStruct {
  var data: Int
  init(_ arg: Int) {
    data = arg
  }
}

/// This class is not serialized since it contains
/// a field of an internal type.
public class Base {
  public init() {}
  var baseVarInternal: InternalStruct {
    return InternalStruct(1)
  }
  package var baseVarPkg: Int {
    return 0
  }
}

@usableFromInline 
class UFIKlass: Base {
  override init() {}

  var varInternal = 11

  @usableFromInline
  var varUfi = 12

  override var baseVarInternal: InternalStruct { return InternalStruct(3) }
  override var baseVarPkg: Int { return 2 }
}

/// This class only contains package or public symbols, thus serialized.
public class PubKlass {
  public init() {}
  public var pubVar: Int {
    return 1
  }
  package var pkgVar: Int {
    return 2
  }
}

/// This class contains an internal field but its type is a public literal type, thus serialized.
public class PubKlassWithInternalMember {
  var internalVar: Int
  public init(_ arg: Int) {
    internalVar = arg
  }
  public var pubVar: Int {
    return 1
  }
  package var pkgVar: Int {
    return 2
  }
}

// createPubClass<A>(_:)
// CHECK-DAG: sil [serialized_for_package] [canonical] [ossa] @$s3Lib14createPubClassySixlF : $@convention(thin) <T> (@in_guaranteed T) -> Int {
public func createPubClass<T>(_ t: T) -> Int {
  return getPubClass(t).foo()
}

// CHECK-DAG: sil [serialized_for_package] [_semantics "optimize.sil.specialize.generic.never"] [canonical] [ossa] @$s3Lib20createPubClass_neverySixlF : $@convention(thin) <T> (@in_guaranteed T) -> Int {
@_semantics("optimize.sil.specialize.generic.never")
public func createPubClass_never<T>(_ t: T) -> Int {
  return getPubClass(t).foo()
}

// CHECK-DAG: sil [serialized_for_package] [canonical] [ossa] @$s3Lib11getPubClassyAA13PublicDerivedCyxGxlF : $@convention(thin) <T> (@in_guaranteed T) -> @owned PublicDerived<T> {
public func getPubClass<T>(_ t : T) -> PublicDerived<T> {
  return PublicDerived<T>(t)
}

// Not serialized
public func createPrvClass<T>(_ t: T) -> Int {
  return getPrvClass(t).foo()
}

// Not serialized
@_semantics("optimize.sil.specialize.generic.never")
public func createPrvClass_never<T>(_ t: T) -> Int {
  return getPrvClass(t).foo()
}

// Not serialized
private func getPrvClass<T>(_ t : T) -> PrivateBase<T> {
  return PrivateDerived<T>(t)
}

public class PublicBase<T> {
  public var t: T
  public func foo() -> Int { return 27 }
  public init(_ t: T) { self.t = t }
}

public class PublicDerived<T> : PublicBase<T> {
  override public func foo() -> Int { return 28 }
}

private class PrivateBase<T> {
  var t: T
  func foo() -> Int { return 27 }
  init(_ t: T) { self.t = t }
}

private class PrivateDerived<T> : PrivateBase<T> {
  override func foo() -> Int { return 28 }
}

public struct PubContainer {
  private final class PrvBase {}
  public init() {}

  // Not serialized; contains exported func
  // but references a private class.
  public func pubMember<T>(_ t: T) -> T {
    var arr = Array<PrvBase>()
    arr.append(PrvBase())
    print(arr)
    exportedFunc(arr)
    return t
  }
}

@_specialize(exported: true, where T == Int)
@inlinable
public func exportedFunc<T>(_ t: T) {
  print(t)
}

public func pubFunc<T>(_ t: T) -> Int {
  return getPubClass(t).foo()
}

@_semantics("optimize.sil.specialize.generic.never")
public func pubFuncNoSpecialize<T>(_ t: T) -> Int {
  return getPubClass(t).foo()
}

struct MyInternalStruct {
  var x: Int { return 27 }
  var y: Int { return 28 }
}

public struct PubStruct {
  public var x: Int { return 27 }
  public var y: Int { return 28 }
}

class Myclass {
  var x: Int { return 27 }
  var y: Int { return 28 }
}

class Derived : Myclass {
  override var x: Int { return 29 }
  override var y: Int { return 30 }
}


func getInternalStructKeypath<T>(_ t: T) -> KeyPath<MyInternalStruct, Int> {
  return \MyInternalStruct.x
}

// CHECK-DAG: sil [canonical] @$s3Lib19getPubStructKeypathys7KeyPathCyAA0cD0VSiGxlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @owned KeyPath<PubStruct, Int>
public func getPubStructKeypath<T>(_ t: T) -> KeyPath<PubStruct, Int> {
  return \PubStruct.x
}

public func useInternalStructKeypath<T>(_ t: T) -> Int {
  let s = MyInternalStruct()
  return s[keyPath: getInternalStructKeypath(t)]
}

// CHECK-DAG: sil [serialized_for_package] [canonical] [ossa] @$s3Lib19usePubStructKeypathySixlF : $@convention(thin) <T> (@in_guaranteed T) -> Int {
public func usePubStructKeypath<T>(_ t: T) -> Int {
  let p = PubStruct()
  return p[keyPath: getPubStructKeypath(t)]
}

func getClassKeypath<T>(_ t: T) -> KeyPath<Myclass, Int> {
  return \Myclass.x
}

public func useClassKeypath<T>(_ t: T) -> Int {
  let c = Derived()
  return c[keyPath: getClassKeypath(t)]
}

/// PubKlass doesn't contain internal symbols so its vtable is serialized.
// CHECK-LABEL: sil_vtable [serialized_for_package] PubKlass {
// CHECK-NEXT: #PubKlass.init!allocator: (PubKlass.Type) -> () -> PubKlass : @$s3Lib8PubKlassCACycfC
// CHECK-NEXT: #PubKlass.pubVar!getter: (PubKlass) -> () -> Int : @$s3Lib8PubKlassC6pubVarSivg
// CHECK-NEXT: #PubKlass.pkgVar!getter: (PubKlass) -> () -> Int : @$s3Lib8PubKlassC6pkgVarSivg
// CHECK-NEXT: #PubKlass.deinit!deallocator: @$s3Lib8PubKlassCfD

/// PubKlassWithInternalMember contains an internal field but its type is a literal public type, so the class is serialized.
// CHECK-LABEL: sil_vtable [serialized_for_package] PubKlassWithInternalMember {
// CHECK-NEXT:   #PubKlassWithInternalMember.init!allocator: (PubKlassWithInternalMember.Type) -> (Int) -> PubKlassWithInternalMember : @$s3Lib26PubKlassWithInternalMemberCyACSicfC  // PubKlassWithInternalMember.__allocating_init(_:)
// CHECK-NEXT:   #PubKlassWithInternalMember.pubVar!getter: (PubKlassWithInternalMember) -> () -> Int : @$s3Lib26PubKlassWithInternalMemberC6pubVarSivg  // PubKlassWithInternalMember.pubVar.getter
// CHECK-NEXT:   #PubKlassWithInternalMember.pkgVar!getter: (PubKlassWithInternalMember) -> () -> Int : @$s3Lib26PubKlassWithInternalMemberC6pkgVarSivg  // PubKlassWithInternalMember.pkgVar.getter
// CHECK-NEXT:   #PubKlassWithInternalMember.deinit!deallocator: @$s3Lib26PubKlassWithInternalMemberCfD  // PubKlassWithInternalMember.__deallocating_deinit
