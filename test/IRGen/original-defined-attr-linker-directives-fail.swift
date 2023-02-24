// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)
// RUN: echo '[ { "module": "OriginalModule", "platforms": ["macOS"], "install_name": "/System/OriginalModule.dylib"} ]' > %t/install-name.json
// RUN: %target-swift-frontend -swift-version 4 -enforce-exclusivity=checked %s -emit-ir -module-name CurrentModule -D CURRENT_MODULE -previous-module-installname-map-file %t/install-name.json | %FileCheck %s -check-prefix=CHECK-SYMBOLS

// RUN: not %target-swift-frontend -swift-version 4 -enforce-exclusivity=checked %s -emit-ir -module-name CurrentModule -D CURRENT_MODULE -previous-module-installname-map-file %t/nil.json >& %t/error.txt
// RUN: %FileCheck %s -check-prefix=CHECK-ERROR < %t/error.txt

// RUN: %target-swift-frontend -target x86_64-apple-macosx10.6 -swift-version 4 -enforce-exclusivity=checked %s -emit-ir -disable-legacy-type-info -module-name CurrentModule -D CURRENT_MODULE -previous-module-installname-map-file %t/install-name.json | %FileCheck %s -check-prefix=CHECK-SYMBOLS-LOW-TARGET

@available(OSX 10.8, *)
@_originallyDefinedIn(module: "OriginalModule", macOS 10.10)
public struct Entity {
  public func addEntity(_ e: Entity) {}
  public func removeEntity(_ e: Entity) {}
}

// CHECK-SYMBOLS: $ld$previous$/System/OriginalModule.dylib$$1$10.8$10.10$_$s14OriginalModule6EntityVMn$
// CHECK-SYMBOLS: $ld$previous$/System/OriginalModule.dylib$$1$10.8$10.10$_$s14OriginalModule6EntityV03addC0yyACF$
// CHECK-SYMBOLS: $ld$previous$/System/OriginalModule.dylib$$1$10.8$10.10$_$s14OriginalModule6EntityVN$
// CHECK-SYMBOLS: $ld$previous$/System/OriginalModule.dylib$$1$10.8$10.10$_$s14OriginalModule6EntityVMa$
// CHECK-SYMBOLS: $ld$previous$/System/OriginalModule.dylib$$1$10.8$10.10$_$s14OriginalModule6EntityV06removeC0yyACF$

// CHECK-SYMBOLS-LOW-TARGET: $ld$previous$/System/OriginalModule.dylib$$1$1.0$10.10$_$s14OriginalModule6EntityVMn$
// CHECK-SYMBOLS-LOW-TARGET: $ld$previous$/System/OriginalModule.dylib$$1$1.0$10.10$_$s14OriginalModule6EntityV03addC0yyACF$
// CHECK-SYMBOLS-LOW-TARGET: $ld$previous$/System/OriginalModule.dylib$$1$1.0$10.10$_$s14OriginalModule6EntityVN$
// CHECK-SYMBOLS-LOW-TARGET: $ld$previous$/System/OriginalModule.dylib$$1$1.0$10.10$_$s14OriginalModule6EntityVMa$
// CHECK-SYMBOLS-LOW-TARGET: $ld$previous$/System/OriginalModule.dylib$$1$1.0$10.10$_$s14OriginalModule6EntityV06removeC0yyACF$

// CHECK-ERROR: cannot open previous install name map
