// XFAIL: *

// REQUIRES: swift_feature_SerializeAbstractTypeLayoutForHiddenTypes

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend \
// RUN:   -internal-import-bridging-header %S/Inputs/abstract-type-layout-info/Hidden.h \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -parse-as-library -emit-module -module-name Library \
// RUN:   -emit-module-path %t/Library.swiftmodule \
// RUN:   %S/Inputs/abstract-type-layout-info/Library.swift
// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -dump-abstract-type-layout-info=sil-type \
// RUN:   -I %t -emit-ir -o %t/Client.ll -module-name Client \
// RUN:   %S/Inputs/abstract-type-layout-info/Client.swift \
// RUN:   > %t/recovery.txt
// RUN: %FileCheck %s --input-file %t/recovery.txt --match-full-lines

// A recovered hidden struct should retain its aggregate and nominal
// classification.
// CHECK: SILType:
// CHECK:   isMetatype: false
// CHECK-NEXT:   isAggregate: true
// CHECK-NEXT:   isOrHasEnum: false
// CHECK-NEXT:   nominalKind: struct
