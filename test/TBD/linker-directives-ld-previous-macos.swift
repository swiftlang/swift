// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck %S/Inputs/linker-directive.swift -tbd-is-installapi -emit-tbd -emit-tbd-path %t/linker_directives.tbd -previous-module-installname-map-file %S/Inputs/install-name-map-toasterkit.json
// RUN: %FileCheck %s < %t/linker_directives.tbd
// RUN: %FileCheck -check-prefix=CHECK-NO-NEW-SYMBOL %s < %t/linker_directives.tbd
// RUN: %target-swift-frontend -typecheck %S/Inputs/linker-directive.swift -emit-tbd -emit-tbd-path %t/linker_directives.tbd -previous-module-installname-map-file %S/Inputs/install-name-map-toasterkit.json
// RUN: %FileCheck %s < %t/linker_directives.tbd
// RUN: %FileCheck -check-prefix=CHECK-NO-NEW-SYMBOL %s < %t/linker_directives.tbd

// RUN: %target-swift-frontend -target-variant x86_64-apple-ios13.0-macabi -typecheck %S/Inputs/linker-directive.swift -emit-tbd -emit-tbd-path %t/linker_directives.tbd -previous-module-installname-map-file %S/Inputs/install-name-map-toasterkit.json
// RUN: %FileCheck -check-prefix=CHECK-ZIPPERED %s < %t/linker_directives.tbd
// RUN: %FileCheck -check-prefix=CHECK-NO-NEW-SYMBOL %s < %t/linker_directives.tbd

// CHECK: $ld$previous$/System/Previous/macOS/ToasterKit.dylib$$1$10.8$10.15$_$s10ToasterKit5toastyyF$
// CHECK: $ld$previous$/System/Previous/macOS/ToasterKit.dylib$$1$10.8$10.15$_$s10ToasterKit7VehicleV4moveyyF$
// CHECK: $ld$previous$/System/Previous/macOS/ToasterKit.dylib$$1$10.8$10.15$_$s10ToasterKit7VehicleVMa$
// CHECK: $ld$previous$/System/Previous/macOS/ToasterKit.dylib$$1$10.8$10.15$_$s10ToasterKit7VehicleVMn$
// CHECK: $ld$previous$/System/Previous/macOS/ToasterKit.dylib$$1$10.8$10.15$_$s10ToasterKit7VehicleVN$

// CHECK-ZIPPERED: $ld$previous$/System/Previous/iOS/ToasterKit.dylib$$2$10.2$13.0$_$s10ToasterKit5toastyyF$
// CHECK-ZIPPERED: $ld$previous$/System/Previous/macOS/ToasterKit.dylib$$1$10.8$10.15$_$s10ToasterKit5toastyyF$

// CHECK-NO-NEW-SYMBOL-NOT: $_$s10ToasterKit7VehicleV32originallyDefinedInCurrentModuleyyF
