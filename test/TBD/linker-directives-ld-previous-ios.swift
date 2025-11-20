// REQUIRES: OS=ios

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck %S/Inputs/linker-directive.swift -tbd-is-installapi -emit-tbd -emit-tbd-path %t/linker_directives.tbd -previous-module-installname-map-file %S/Inputs/install-name-map-toasterkit.json -tbd-install_name toasterkit
// RUN: %llvm-nm %t/linker_directives.tbd | %FileCheck --check-prefix=CHECK-objc-simulator-%target-is-simulator %s
// RUN: %target-swift-frontend -typecheck %S/Inputs/linker-directive.swift -emit-tbd -emit-tbd-path %t/linker_directives.tbd -previous-module-installname-map-file %S/Inputs/install-name-map-toasterkit.json -tbd-install_name toasterkit
// RUN: %llvm-nm %t/linker_directives.tbd | %FileCheck --check-prefix=CHECK-objc-simulator-%target-is-simulator %s

// CHECK-objc-simulator-false: D $ld$previous$/System/Previous/iOS/ToasterKit.dylib$$2$10.2$13.0$_$s10ToasterKit5toastyyF$
// CHECK-objc-simulator-false: D $ld$previous$/System/Previous/iOS/ToasterKit.dylib$$2$10.2$13.0$_$s10ToasterKit7VehicleV4moveyyF$
// CHECK-objc-simulator-false: D $ld$previous$/System/Previous/iOS/ToasterKit.dylib$$2$10.2$13.0$_$s10ToasterKit7VehicleVMa$
// CHECK-objc-simulator-false: D $ld$previous$/System/Previous/iOS/ToasterKit.dylib$$2$10.2$13.0$_$s10ToasterKit7VehicleVMn$
// CHECK-objc-simulator-false: D $ld$previous$/System/Previous/iOS/ToasterKit.dylib$$2$10.2$13.0$_$s10ToasterKit7VehicleVN$
// CHECK-objc-simulator-false: D $ld$previous$/System/Previous/iOS/ToasterKit.dylib$$2$10.2$18.0$_$s10ToasterKit14movedPriorTo26yyF
// CHECK-objc-simulator-false: D $ld$previous$/System/Previous/iOS/ToasterKit.dylib$$2$10.2$26.0$_$s10ToasterKit16movedInVersion26yyF$
// CHECK-objc-simulator-false: D $ld$previous$/System/Previous/iOS/ToasterKit.dylib$$2$10.2$26.0$_$s10ToasterKit26movedInVersionsMappingTo26yyF$
// CHECK-objc-simulator-false: D $ld$previous$/System/Previous/iOS/ToasterKit.dylib$$2$10.2$27.0$_$s10ToasterKit26movedInVersionsMappingTo27yyF$

// CHECK-objc-simulator-true: D $ld$previous$/System/Previous/iOS/ToasterKit.dylib$$7$10.2$13.0$_$s10ToasterKit5toastyyF$
// CHECK-objc-simulator-true: D $ld$previous$/System/Previous/iOS/ToasterKit.dylib$$7$10.2$13.0$_$s10ToasterKit7VehicleV4moveyyF$
// CHECK-objc-simulator-true: D $ld$previous$/System/Previous/iOS/ToasterKit.dylib$$7$10.2$13.0$_$s10ToasterKit7VehicleVMa$
// CHECK-objc-simulator-true: D $ld$previous$/System/Previous/iOS/ToasterKit.dylib$$7$10.2$13.0$_$s10ToasterKit7VehicleVMn$
// CHECK-objc-simulator-true: D $ld$previous$/System/Previous/iOS/ToasterKit.dylib$$7$10.2$13.0$_$s10ToasterKit7VehicleVN$
// CHECK-objc-simulator-true: D $ld$previous$/System/Previous/iOS/ToasterKit.dylib$$7$10.2$18.0$_$s10ToasterKit14movedPriorTo26yyF
// CHECK-objc-simulator-true: D $ld$previous$/System/Previous/iOS/ToasterKit.dylib$$7$10.2$26.0$_$s10ToasterKit16movedInVersion26yyF$
// CHECK-objc-simulator-true: D $ld$previous$/System/Previous/iOS/ToasterKit.dylib$$7$10.2$26.0$_$s10ToasterKit26movedInVersionsMappingTo26yyF$
// CHECK-objc-simulator-true: D $ld$previous$/System/Previous/iOS/ToasterKit.dylib$$7$10.2$27.0$_$s10ToasterKit26movedInVersionsMappingTo27yyF$
