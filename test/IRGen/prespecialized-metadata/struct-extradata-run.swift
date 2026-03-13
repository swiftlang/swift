// RUN: %empty-directory(%t)
// RUN: %clang -c -std=c++17 -v %target-cc-options %target-threading-opt -g -O0 -isysroot %sdk %S/Inputs/extraDataFields.cpp -o %t/extraDataFields.o -I %swift-include-dir -I %swift_src_root/include/ -I %swift_src_root/stdlib/public/SwiftShims/ -I %llvm_src_root/include -I %llvm_obj_root/include -L %swift-include-dir/../lib/swift/macosx -I %swift_obj_root/include

// RUN: %target-build-swift -c %S/Inputs/struct-extra-data-fields.swift -emit-library -emit-module -module-name ExtraDataFieldsNoTrailingFlags -target %module-target-future -Xfrontend -disable-generic-metadata-prespecialization -emit-module-path %t/ExtraDataFieldsNoTrailingFlags.swiftmodule -o %t/%target-library-name(ExtraDataFieldsNoTrailingFlags)
// RUN: %target-build-swift -c %S/Inputs/struct-extra-data-fields.swift -emit-library -emit-module -module-name ExtraDataFieldsTrailingFlags -target %module-target-future -Xfrontend -prespecialize-generic-metadata -emit-module-path %t/ExtraDataFieldsTrailingFlags.swiftmodule -o %t/%target-library-name(ExtraDataFieldsTrailingFlags)
// RUN: %target-build-swift -v %mcp_opt %s %t/extraDataFields.o -import-objc-header %S/Inputs/extraDataFields.h -Xfrontend -disable-generic-metadata-prespecialization -target %module-target-future -lc++ -L %swift-include-dir/../lib/swift/macosx -sdk %sdk -o %t/main -I %t -L %t -lExtraDataFieldsTrailingFlags -lExtraDataFieldsNoTrailingFlags -module-name main

// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: swift_test_mode_optimize
// UNSUPPORTED: swift_test_mode_optimize_size

func ptr<T>(to ty: T.Type) -> UnsafeMutableRawPointer {
    UnsafeMutableRawPointer(mutating: unsafePointerToMetadata(of: ty))!
}

func unsafePointerToMetadata<T>(of ty: T.Type) -> UnsafePointer<T.Type> {
  unsafeBitCast(ty, to: UnsafePointer<T.Type>.self)
}

import ExtraDataFieldsNoTrailingFlags


let one = completeMetadata(
  ptr(
    to: ExtraDataFieldsNoTrailingFlags.FixedFieldOffsets<Void>.self
  )
)

// CHECK: nil
print(
  trailingFlagsForStructMetadata(
    one
  )
)

guard let oneOffsets = fieldOffsetsForStructMetadata(one) else {
  fatalError("no field offsets")
}

// CHECK-NEXT: 0
print(oneOffsets.advanced(by: 0).pointee)
// CHECK-NEXT: 8
print(oneOffsets.advanced(by: 1).pointee)

let two = completeMetadata(
  ptr(
    to: ExtraDataFieldsNoTrailingFlags.DynamicFieldOffsets<Int32>.self
  )
)

// CHECK-NEXT: nil
print(
  trailingFlagsForStructMetadata(
    two
  )
)

guard let twoOffsets = fieldOffsetsForStructMetadata(two) else {
  fatalError("no field offsets")
}

// CHECK-NEXT: 0
print(twoOffsets.advanced(by: 0).pointee)
// CHECK-NEXT: 4
print(twoOffsets.advanced(by: 1).pointee)


import ExtraDataFieldsTrailingFlags


let three = completeMetadata(
  ptr(
    to: ExtraDataFieldsTrailingFlags.FixedFieldOffsets<Void>.self
  )
)

// CHECK-NEXT: 0
print(
  trailingFlagsForStructMetadata(
    three
  )!.pointee
)

guard let threeOffsets = fieldOffsetsForStructMetadata(three) else {
  fatalError("no field offsets")
}

// CHECK-NEXT: 0
print(threeOffsets.advanced(by: 0).pointee)
// CHECK-NEXT: 8
print(threeOffsets.advanced(by: 1).pointee)

let four = completeMetadata(
  ptr(
    to: ExtraDataFieldsTrailingFlags.DynamicFieldOffsets<Int32>.self
  )
)

// CHECK-NEXT: 0
print(
  trailingFlagsForStructMetadata(
    four
  )!.pointee
)

guard let fourOffsets = fieldOffsetsForStructMetadata(four) else {
  fatalError("no field offsets")
}

// CHECK-NEXT: 0
print(fourOffsets.advanced(by: 0).pointee)
// CHECK-NEXT: 4
print(fourOffsets.advanced(by: 1).pointee)

