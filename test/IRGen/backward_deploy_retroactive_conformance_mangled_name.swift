// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -target %target-cpu-apple-macosx14 -module-name Lib -o %t/Lib.swiftmodule %t/Lib.swift
// RUN: %target-swift-frontend -emit-ir -target %target-cpu-apple-macosx26 -I %t %t/main.swift | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -target %target-cpu-apple-macosx15 -I %t %t/main.swift | %FileCheck %s --check-prefix=SWIFT60 --implicit-check-not=HOHXg_GMD --implicit-check-not=HOHXg_GMd --implicit-check-not=HO_HCg_GMD --implicit-check-not=HO_HCg_GMd
// RUN: %target-swift-frontend -emit-ir -target %target-cpu-apple-macosx14 -I %t %t/main.swift | %FileCheck %s --check-prefix=SWIFT59 --implicit-check-not=HXg_GMD --implicit-check-not=HXg_GMd --implicit-check-not=HXg_Qo_MD --implicit-check-not=HXg_Qo_Md

// REQUIRES: OS=macosx

// Retroactive pack conformances ("HX") need the Swift 6.0 runtime to demangle,
// and conformances of opaque archetypes ("HO") need the Swift 6.2 runtime.

//--- Lib.swift
public struct Pack<each V: Error> {
  let v: (repeat each V)
  public init(_ v: repeat each V) { self.v = (repeat each v) }
}

struct E: Error {}
public func opaque() -> some Error { E() }

public protocol Q {}
public struct S<T: Q> {
  let t: T
  public init(_ t: T) { self.t = t }
}

struct W<each T> {}
public func opaquePack<each T: Error>(_ t: repeat each T) -> some Any {
  W<repeat each T>()
}

//--- main.swift
import Lib

extension Int: @retroactive Error {}

_ = Pack(0, opaque())
_ = [Pack(0)]

// "HO" without a pack: the opaque archetype's conformance satisfies the
// condition of a retroactive conditional conformance.
extension Array: @retroactive Q where Element: Error {}
_ = [S([opaque()])]

// "HX" in the substitutions of an opaque type rather than a nominal type.
_ = [opaquePack(0)]

// A field type with a type parameter needs the generic signature to mangle.
struct Holder<T: Error> { var x: Pack<T, Int> }

// CHECK-DAG: call ptr @__swift_instantiateConcreteTypeFromMangledName{{.*}}(ptr @"{{.*}}HOHXg_GM{{[Dd]}}"
// CHECK-DAG: call ptr @__swift_instantiateConcreteTypeFromMangledName{{.*}}(ptr @"$s3Lib4PackVySi_QP{{.*}}HXg_GM{{[Dd]}}"
// CHECK-DAG: @"$s4main6HolderVMF" = {{.*}}(ptr @"symbolic {{.*}}HXg_G {{.*}}"

// CHECK-DAG: call ptr @__swift_instantiateConcreteTypeFromMangledName{{.*}}(ptr @"$s3Lib1SVy{{.*}}HO_HCg_GM{{[Dd]}}"
// CHECK-DAG: call ptr @__swift_instantiateConcreteTypeFromMangledName{{.*}}(ptr @"$s3Lib10opaquePack{{.*}}HXg_Qo_M{{[Dd]}}"

// SWIFT60-DAG: call swiftcc %swift.metadata_response @"{{.*}}HOHXg_GMa"
// SWIFT60-DAG: call swiftcc %swift.metadata_response @"$s3Lib1SVy{{.*}}HO_HCg_GMa"
// SWIFT60-DAG: call ptr @__swift_instantiateConcreteTypeFromMangledName{{.*}}(ptr @"$s3Lib10opaquePack{{.*}}HXg_Qo_M{{[Dd]}}"
// SWIFT60-DAG: call ptr @__swift_instantiateConcreteTypeFromMangledName{{.*}}(ptr @"$s3Lib4PackVySi_QP{{.*}}HXg_GM{{[Dd]}}"
// SWIFT60-DAG: @"$s4main6HolderVMF" = {{.*}}(ptr @"symbolic {{.*}}HXg_G {{.*}}"

// SWIFT59-DAG: call swiftcc %swift.metadata_response @"{{.*}}HOHXg_GMa"
// SWIFT59-DAG: call swiftcc %swift.metadata_response @"$s3Lib1SVy{{.*}}HO_HCg_GMa"
// SWIFT59-DAG: call swiftcc %swift.metadata_response @"$s3Lib10opaquePack{{.*}}HXg_Qo_Ma"
// SWIFT59-DAG: call swiftcc %swift.metadata_response @"$s3Lib4PackVySi_QP{{.*}}HXg_GMa"
// SWIFT59-DAG: @"$s4main6HolderVMF" = {{.*}}(ptr @"get_type_metadata {{.*}}HXg_G{{.*}}"
