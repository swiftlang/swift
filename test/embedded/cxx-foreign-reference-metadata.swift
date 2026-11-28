// RUN: %target-swift-frontend -emit-ir -I %S/Inputs %s -enable-experimental-feature Embedded -cxx-interoperability-mode=default -wmo -module-name Main | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -I %S/Inputs %s -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -cxx-interoperability-mode=default -wmo -module-name Main | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -I %S/Inputs %s -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=inlinable -cxx-interoperability-mode=default -wmo -module-name Main | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

// Embedded Swift emits minimal type metadata for C++ foreign reference types:
// a value witness table followed by the metadata kind, without a type context
// descriptor or a metadata completion function.

import CxxForeignReference

public protocol HasAssoc {
  associatedtype A
}

public struct UsesBase: HasAssoc {
  public typealias A = BaseRef
}

public struct UsesDerived: HasAssoc {
  public typealias A = DerivedRef
}

public protocol Describable {
  func describe() -> Int
}

extension BaseRef: Describable {
  public func describe() -> Int { Int(value) }
}

public func makeExistential(_ b: BaseRef) -> any Describable { b }

func assocMetatype<T: HasAssoc>(_: T.Type) -> Any.Type { T.A.self }

public func derivedAssocMetatype() -> Any.Type {
  assocMetatype(UsesDerived.self)
}

public func derivedMetatype() -> Any.Type { DerivedRef.self }

// CHECK: @"$eSo10DerivedRefVWV" = linkonce_odr hidden constant %swift.vwtable
// CHECK: @"$eSo10DerivedRefVMf" = linkonce_odr hidden constant <{ ptr, i64 }> <{ ptr @"$eSo10DerivedRefVWV{{(\.ptrauth)?}}", i64 516 }>
// CHECK: @"$eSo7BaseRefVWV" = linkonce_odr hidden constant %swift.vwtable
// CHECK: @"$eSo7BaseRefVMf" = linkonce_odr hidden constant <{ ptr, i64 }> <{ ptr @"$eSo7BaseRefVWV{{(\.ptrauth)?}}", i64 516 }>

// The value witnesses retain and release the object with the C++ functions.
// CHECK-LABEL: define linkonce_odr hidden void @"$eSo10DerivedRefVwxx"(
// CHECK: call void @{{.*}}releaseShared{{.*}}(ptr
// CHECK-LABEL: define linkonce_odr hidden ptr @"$eSo10DerivedRefVwcp"(
// CHECK: call void @{{.*}}retainShared{{.*}}(ptr
