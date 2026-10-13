// Two modules that share an ABI name may declare types with the same name.
// Those types have identical debugger manglings, so reconstructing the type
// from its mangled name may legitimately find the other declaration.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module %t/Lib.swift -module-name Lib \
// RUN:   -o %t/Lib.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/Lib_SPI.swift \
// RUN:   -module-name Lib_SPI -module-abi-name Lib -o %t/Lib_SPI.swiftmodule
// RUN: %target-swift-frontend -primary-file %t/main.swift -I %t -emit-ir -g \
// RUN:   -module-name Main -o - | %FileCheck %s

//--- Lib.swift
public struct CollidingStruct {}
public struct CollidingGeneric<T> {}

//--- Lib_SPI.swift
public struct CollidingStruct {}
public struct CollidingGeneric<T> {}

//--- main.swift
// Lib must be imported first so that the demangler resolves the shared ABI
// name to Lib's declarations rather than Lib_SPI's.
import Lib
import Lib_SPI

public func f(_ s: Lib_SPI.CollidingStruct,
              _ g: Lib_SPI.CollidingGeneric<Int>,
              _ n: Lib_SPI.CollidingGeneric<Lib_SPI.CollidingStruct>) {}

// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "CollidingStruct",{{.*}}identifier: "$s3Lib15CollidingStructVD"
// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "$s3Lib16CollidingGenericVySiGD"
// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "$s3Lib16CollidingGenericVyAA0B6StructVGD"
