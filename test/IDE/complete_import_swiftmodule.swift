// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: CPU=x86_64

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/Frameworks
// RUN: mkdir -p %t/Modules

// RUN: mkdir -p %t/Frameworks/MacAndLinuxInterfaceFW.framework/Modules/MacAndLinuxInterfaceFW.swiftmodule/
// RUN: touch    %t/Frameworks/MacAndLinuxInterfaceFW.framework/Modules/MacAndLinuxInterfaceFW.swiftmodule/x86_64-apple-macos.swiftinterface
// RUN: touch    %t/Frameworks/MacAndLinuxInterfaceFW.framework/Modules/MacAndLinuxInterfaceFW.swiftmodule/x86_64-unknown-linux-gnu.swiftinterface

// RUN: mkdir -p %t/Frameworks/MacAndLinuxSerializedFW.framework/Modules/MacAndLinuxSerializedFW.swiftmodule/
// RUN: touch    %t/Frameworks/MacAndLinuxSerializedFW.framework/Modules/MacAndLinuxSerializedFW.swiftmodule/x86_64-apple-macos.swiftmodule
// RUN: touch    %t/Frameworks/MacAndLinuxSerializedFW.framework/Modules/MacAndLinuxSerializedFW.swiftmodule/x86_64-unknown-linux-gnu.swiftmodule

// Not-matching target.
// RUN: mkdir -p %t/Frameworks/IosInterfaceFW.framework/Modules/IosInterfaceFW.swiftmodule/
// RUN: touch    %t/Frameworks/IosInterfaceFW.framework/Modules/IosInterfaceFW.swiftmodule/arm64-apple-ios.swiftinterface

// Invalid - Framework must be target specific.
// RUN: mkdir -p %t/Frameworks/NonTargetInterfaceFW.framework/Modules/
// RUN: touch    %t/Frameworks/NonTargetInterfaceFW.framework/Modules/NonTargetInterfaceFW.swiftinterface

// Invalid - Framework must be target specific.
// RUN: mkdir -p %t/Frameworks/NonTargetSerializedFW.framework/Modules/
// RUN: touch    %t/Frameworks/NonTargetSerializedFW.framework/Modules/NonTargetSerializedFW.swiftmodule

// RUN: mkdir -p %t/Modules/MacAndLinuxInterfaceMod.swiftmodule/
// RUN: touch    %t/Modules/MacAndLinuxInterfaceMod.swiftmodule/x86_64-apple-macos.swiftinterface
// RUN: touch    %t/Modules/MacAndLinuxInterfaceMod.swiftmodule/x86_64-unknown-linux-gnu.swiftinterface

// RUN: mkdir -p %t/Modules/MacAndLinuxSerializedMod.swiftmodule/
// RUN: touch    %t/Modules/MacAndLinuxSerializedMod.swiftmodule/x86_64-apple-macos.swiftmodule
// RUN: touch    %t/Modules/MacAndLinuxSerializedMod.swiftmodule/x86_64-unknown-linux-gnu.swiftmodule

// Non-matching target.
// RUN: mkdir -p %t/Modules/IosInterfaceMod.swiftmodule
// RUN: touch    %t/Modules/IosInterfaceMod.swiftmodule/arm64-apple-ios.swiftinterface

// RUN: touch %t/Modules/NonTargetInterfaceMod.swiftinterface
// RUN: touch %t/Modules/NonTargetSerializedMod.swiftmodule

// Invalid - '.swiftinterface' directory.
// RUN: mkdir -p %t/Modules/DirInterfaceMod.swiftinterface
// RUN: touch    %t/Modules/DirInterfaceMod.swiftinterface/x86_64-apple-macos.swiftinterface
// RUN: touch    %t/Modules/DirInterfaceMod.swiftinterface/x86_64-unknown-linux-gnu.swiftinterface

// Invalid - Empty directory.
// RUN: mkdir -p %t/Modules/EmptyDirInterfaceMod.swiftinterface
// RUN: mkdir -p %t/Modules/EmptyDirSerializedMod.swiftmodule

// Invalid - Not a module.
// RUN: touch %t/Modules/UnrelatedFile.dat

// Invalid - Serialized in -F directory.
// RUN: touch %t/Frameworks/ModuleInFrameworkDir.swiftmodule

// Invalid - Framework in -I directory.
// RUN: mkdir -p %t/Modules/FrameworkInModuleDir.framework/Modules/FrameworkInModuleDir.swiftmodule/
// RUN: touch    %t/Modules/FrameworkInModuleDir.framework/Modules/FrameworkInModuleDir.swiftmodule/x86_64-apple-macos.swiftinterface
// RUN: touch    %t/Modules/FrameworkInModuleDir.framework/Modules/FrameworkInModuleDir.swiftmodule/x86_64-unknown-linux-gnu.swiftinterface

// Invalid - FrameworkName/SerializedName mismatch
// RUN: mkdir -p %t/Frameworks/NameMismatchFW.framework/Modules/MismatchNameFW.swiftmodule/
// RUN: touch    %t/Frameworks/NameMismatchFW.framework/Modules/MismatchNameFW.swiftmodule/x86_64-apple-macos.swiftinterface
// RUN: touch    %t/Frameworks/NameMismatchFW.framework/Modules/MismatchNameFW.swiftmodule/x86_64-unknown-linux-gnu.swiftinterface

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COMPLETE -F %t/Frameworks -sdk %t -I %t/Modules | %FileCheck %s

// CHECK-NOT: IosInterfaceFW
// CHECK-NOT: NonTargetInterfaceFW
// CHECK-NOT: NonTargetSerializedFW
// CHECK-NOT: IosInterfaceMod
// CHECK-NOT: DirInterfaceMod
// CHECK-NOT: EmptyDirInterfaceMod
// CHECK-NOT: EmptyDirSerializedMod
// CHECK-NOT: UnrelatedFile
// CHECK-NOT: ModuleInFrameworkDir
// CHECK-NOT: FrameworkInModuleDir
// CHECK-NOT: NameMismatchFW
// CHECK-NOT: MismatchNameFW

// CHECK-DAG: MacAndLinuxInterfaceFW[#Module#]
// CHECK-DAG: MacAndLinuxSerializedFW[#Module#]
// CHECK-DAG: MacAndLinuxInterfaceMod[#Module#]
// CHECK-DAG: MacAndLinuxSerializedMod[#Module#]
// CHECK-DAG: NonTargetInterfaceMod[#Module#]
// CHECK-DAG: NonTargetSerializedMod[#Module#]

// CHECK-DAG: Swift[#Module#]

import #^COMPLETE^#
