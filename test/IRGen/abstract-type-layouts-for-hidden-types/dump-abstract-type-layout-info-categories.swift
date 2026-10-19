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
// RUN:   > %t/sil-type.txt
// RUN: %FileCheck %s --check-prefix=SIL-TYPE --input-file=%t/sil-type.txt \
// RUN:   --implicit-check-not='TypeLowering:' --implicit-check-not='TypeInfo:'

// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -dump-abstract-type-layout-info=type-lowering \
// RUN:   -I %t -emit-ir -o %t/Client.ll -module-name Client \
// RUN:   %S/Inputs/abstract-type-layout-info/Client.swift \
// RUN:   > %t/type-lowering.txt
// RUN: %FileCheck %s --check-prefix=TYPE-LOWERING \
// RUN:   --input-file=%t/type-lowering.txt --implicit-check-not='SILType:' \
// RUN:   --implicit-check-not='TypeInfo:'

// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -dump-abstract-type-layout-info=type-info \
// RUN:   -I %t -emit-ir -o %t/Client.ll -module-name Client \
// RUN:   %S/Inputs/abstract-type-layout-info/Client.swift \
// RUN:   > %t/type-info.txt
// RUN: %FileCheck %s --check-prefix=TYPE-INFO --input-file=%t/type-info.txt \
// RUN:   --implicit-check-not='SILType:' --implicit-check-not='TypeLowering:'

// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -dump-abstract-type-layout-info=all \
// RUN:   -I %t -emit-ir -o %t/Client.ll -module-name Client \
// RUN:   %S/Inputs/abstract-type-layout-info/Client.swift \
// RUN:   > %t/all.txt
// RUN: %FileCheck %s --check-prefix=ALL --input-file=%t/all.txt

// SIL-TYPE: === Abstract type layout information ===
// SIL-TYPE: SILType:

// TYPE-LOWERING: === Abstract type layout information ===
// TYPE-LOWERING: TypeLowering:

// TYPE-INFO: === Abstract type layout information ===
// TYPE-INFO: TypeInfo:

// ALL: === Abstract type layout information ===
// ALL: TypeLowering:
// ALL: SILType:
// ALL: TypeInfo:
