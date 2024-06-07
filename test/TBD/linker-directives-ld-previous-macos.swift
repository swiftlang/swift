// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck %S/Inputs/linker-directive.swift -tbd-is-installapi -emit-tbd -emit-tbd-path %t/linker_directives_installapi.tbd -previous-module-installname-map-file %S/Inputs/install-name-map-toasterkit.json -tbd-install_name toasterkit
// RUN: %llvm-nm %t/linker_directives_installapi.tbd | %FileCheck %s --check-prefixes=CHECK,CHECK-MAC --implicit-check-not "System/Previous/macCatalyst"
// RUN: %target-swift-frontend -typecheck %S/Inputs/linker-directive.swift -emit-tbd -emit-tbd-path %t/linker_directives_macos.tbd -previous-module-installname-map-file %S/Inputs/install-name-map-toasterkit.json -tbd-install_name toasterkit
// RUN: %llvm-nm %t/linker_directives_macos.tbd | %FileCheck %s --check-prefixes=CHECK,CHECK-MAC --implicit-check-not "System/Previous/macCatalyst"

// RUN: %target-swift-frontend -target-variant x86_64-apple-ios13.1-macabi -typecheck %S/Inputs/linker-directive.swift -emit-tbd -emit-tbd-path %t/linker_directives_macos_macabi.tbd -previous-module-installname-map-file %S/Inputs/install-name-map-toasterkit.json -tbd-install_name toasterkit
// RUN: %llvm-nm %t/linker_directives_macos_macabi.tbd | %FileCheck -check-prefixes=CHECK,CHECK-MAC,CHECK-MACCATALYST %s

// RUN: %target-swift-frontend -target x86_64-apple-ios13.1-macabi -typecheck %S/Inputs/linker-directive.swift -emit-tbd -emit-tbd-path %t/linker_directives_macabi.tbd -previous-module-installname-map-file %S/Inputs/install-name-map-toasterkit.json -tbd-install_name toasterkit
// R/UN: %llvm-nm %t/linker_directives_macabi.tbd | %FileCheck -check-prefixes=CHECK,CHECK-MACCATALYST %s --implicit-check-not "System/Previous/macOS"

// CHECK-MACCATALYST: D $ld$previous$/System/Previous/macCatalyst/ToasterKit.dylib$$6$10.2$13.0$_$s10ToasterKit5toastyyF$
// CHECK-MACCATALYST: D $ld$previous$/System/Previous/macCatalyst/ToasterKit.dylib$$6$10.2$13.0$_$s10ToasterKit7VehicleV4moveyyF$
// CHECK-MACCATALYST: D $ld$previous$/System/Previous/macCatalyst/ToasterKit.dylib$$6$10.2$13.0$_$s10ToasterKit7VehicleVMa$
// CHECK-MACCATALYST: D $ld$previous$/System/Previous/macCatalyst/ToasterKit.dylib$$6$10.2$13.0$_$s10ToasterKit7VehicleVMn$
// CHECK-MACCATALYST: D $ld$previous$/System/Previous/macCatalyst/ToasterKit.dylib$$6$10.2$13.0$_$s10ToasterKit7VehicleVN$

// CHECK-MAC: D $ld$previous$/System/Previous/macOS/ToasterKit.dylib$$1$10.8$10.15$_$s10ToasterKit5toastyyF$
// CHECK-MAC: D $ld$previous$/System/Previous/macOS/ToasterKit.dylib$$1$10.8$10.15$_$s10ToasterKit7VehicleV4moveyyF$
// CHECK-MAC: D $ld$previous$/System/Previous/macOS/ToasterKit.dylib$$1$10.8$10.15$_$s10ToasterKit7VehicleVMa$
// CHECK-MAC: D $ld$previous$/System/Previous/macOS/ToasterKit.dylib$$1$10.8$10.15$_$s10ToasterKit7VehicleVMn$
// CHECK-MAC: D $ld$previous$/System/Previous/macOS/ToasterKit.dylib$$1$10.8$10.15$_$s10ToasterKit7VehicleVN$

// CHECK-NOT: $_$s10ToasterKit7VehicleV32originallyDefinedInCurrentModuleyyF
