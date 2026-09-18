// REQUIRES: swift_feature_SerializeAbstractTypeLayoutForHiddenTypes

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend \
// RUN:   -internal-import-bridging-header %t/Hidden.h \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -dump-abstract-type-layout-info \
// RUN:   -parse-as-library -emit-module -module-name Library \
// RUN:   -emit-module-path %t/Library.swiftmodule %t/Library.swift \
// RUN:   > %t/serialization.txt
// RUN: %FileCheck --check-prefix=SERIALIZATION %s < %t/serialization.txt

// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -dump-abstract-type-layout-info \
// RUN:   -I %t -emit-ir -o %t/Client.ll -module-name Client %t/Client.swift \
// RUN:   > %t/recovery.txt
// RUN: %FileCheck --check-prefix=RECOVERY %s < %t/recovery.txt

//--- Hidden.h
typedef struct {
  int count;
  double value;
} HiddenCStruct;

//--- Library.swift
public struct Wrapper {
  private var hidden: HiddenCStruct

  public init(count: Int32, value: Double) {
    hidden = HiddenCStruct(count: count, value: value)
  }
}

//--- Client.swift
import Library

public func passThrough(_ value: Wrapper) -> Wrapper {
  value
}

// SERIALIZATION: === Abstract type layout information ===
// SERIALIZATION: origin: serialization
// SERIALIZATION: CanType:
// SERIALIZATION: Minimal Type Lowering for lowered type:
// SERIALIZATION: IRGen Type Lowering for lowered type:
// SERIALIZATION: TypeInfo:
// SERIALIZATION: fixedSize: 16
// SERIALIZATION: fixedAlignment: 8
// SERIALIZATION: fixedStride: 16
// SERIALIZATION: nativeParameterSchema:
// SERIALIZATION: nativeReturnSchema:

// RECOVERY: === Abstract type layout information ===
// RECOVERY: origin: recovery
// RECOVERY: CanType:
// RECOVERY: Minimal Type Lowering for lowered type:
// RECOVERY: IRGen Type Lowering for lowered type:
// RECOVERY: TypeInfo:
// RECOVERY: fixedSize: 16
// RECOVERY: fixedAlignment: 8
// RECOVERY: fixedStride: 16
// RECOVERY: nativeParameterSchema:
// RECOVERY: nativeReturnSchema:
