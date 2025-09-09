// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/%target-library-name(associated_type_by_mangled_name)) -target %target-cpu-apple-macosx15.0 -enable-library-evolution %S/Inputs/associated_type_by_mangled_name.swift -emit-module -emit-module-path %t/associated_type_by_mangled_name.swiftmodule -module-name associated_type_by_mangled_name
// RUN: %target-codesign %t/%target-library-name(associated_type_by_mangled_name)

// RUN: %target-swift-frontend -target %target-cpu-apple-macosx15.0 -I %t %s -emit-ir -o - | %FileCheck %s
// RUN: %target-swift-frontend -target %target-cpu-apple-macosx26.0 -I %t %s -emit-ir -o - | %FileCheck %s -check-prefix CHECK-SUPPORTED

// RUN: %target-build-swift -target %target-cpu-apple-macosx15.0 %s -lassociated_type_by_mangled_name -I %t -L %t -o %t/main15 %target-rpath(%t)
// RUN: %target-codesign %t/main15

// RUN: %target-run %t/main15 %t/%target-library-name(associated_type_by_mangled_name)

// RUN: %target-build-swift -target %target-cpu-apple-macosx26.0 %s -lassociated_type_by_mangled_name -I %t -L %t -o %t/main26 %target-rpath(%t)
// RUN: %target-codesign %t/main26

// RUN: %target-run %t/main26 %t/%target-library-name(associated_type_by_mangled_name)

// REQUIRES: executable_test
// REQUIRES: OS=macosx
// UNSUPPORTED: back_deployment_runtime || use_os_stdlib

import associated_type_by_mangled_name

protocol P: Sendable {
    associatedtype T: Proto
    func foo() -> T
}

struct PImpl: P {
  func foo() -> some Proto {
    return ProtoImpl(2)
  }
}

// CHECK: @"get_type_metadata 30nonisolated_nonsending_closure1PRzl31associated_type_by_mangled_name9ProtoImplVyySiYaKYCcG.3" = linkonce_odr hidden constant <{ i8, i32, i8 }> <{ i8 9, i32 trunc (i64 sub (i64 ptrtoint (ptr @"get_type_metadata 30nonisolated_nonsending_closure1PRzl31associated_type_by_mangled_name9ProtoImplVyySiYaKYCcG" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, i8 }>, ptr @"get_type_metadata 30nonisolated_nonsending_closure1PRzl31associated_type_by_mangled_name9ProtoImplVyySiYaKYCcG.3", i32 0, i32 1) to i64)) to i32), i8 0 }>
// CHECK: define linkonce_odr hidden ptr @"$s31associated_type_by_mangled_name9ProtoImplVyySiYaKYCcGACyxGAA0F0AAWl"() #1 {
// CHECK:   call swiftcc %swift.metadata_response @"$s31associated_type_by_mangled_name9ProtoImplVyySiYaKYCcGMa"(i64 255)
// CHECK: }

// CHECK-SUPPORTED-NOT: @"get_type_metadata 30nonisolated_nonsending_closure1PRzl31associated_type_by_mangled_name9ProtoImplVyySiYaKYCcG.3"
// CHECK-SUPPORTED: define linkonce_odr hidden ptr @"$s31associated_type_by_mangled_name9ProtoImplVyySiYaKYCcGACyxGAA0F0AAWl"() #1 {
// CHECK-SUPPORTED:   call ptr @__swift_instantiateConcreteTypeFromMangledNameAbstract(ptr @"$s31associated_type_by_mangled_name9ProtoImplVyySiYaKYCcGMD")
// CHECK-SUPPORTED: }
struct MyStruct<T: P>: Proto {
  typealias Closure = nonisolated(nonsending) (Int) async throws -> Void

  let x: T

  public var value: some Proto {
    return ProtoImpl<Closure> { _ in }
  }
}

print(makeThing(MyStruct(x: PImpl())))
