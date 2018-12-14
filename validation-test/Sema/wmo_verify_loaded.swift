// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend -swift-version 4 -emit-module -o %t/rdar36801676.swiftmodule %S/Inputs/rdar36801676.swift
// RUN: %target-swift-frontend -swift-version 4 -emit-silgen -enable-objc-interop -I %t -emit-silgen %S/Inputs/rdar36801676_empty.swift %s | %FileCheck %s
// REQUIRES: OS=macosx

// If AST loaded module verification is run after type checking the empty source
// file (rdar36801676_empty.swift), but before type checking this source file,
// then the importer caches the declaration for PasteboardType's constructor
// without synthesizing it's body. Eventually, the SILVerifier will raise a linkage error:
// SIL verification failed: external declarations of SILFunctions with shared
// visibility is not allowed: SingleFunction ||
// !hasSharedVisibility(RefF->getLinkage()) || RefF->hasForeignBody()

import Cocoa
import rdar36801676

let objCSynthesizedEnum = NSPasteboard.PasteboardType(rawValue: "MyPboardType")
extension Notification.Name {}

// NSPasteboardType.init(rawValue:)
// - just make sure it has a body.
// CHECK-LABEL: sil shared [transparent] [serializable] @$sSo16NSPasteboardTypea8rawValueABSS_tcfC : $@convention(method) (@owned String, @thin NSPasteboard.PasteboardType.Type) -> @owned NSPasteboard.PasteboardType {
// CHECK: bb0(%0 : @owned $String, %1 : $@thin NSPasteboard.PasteboardType.Type):
// CHECK: return %{{.*}} : $NSPasteboard.PasteboardType
// CHECK-LABEL: } // end sil function '$sSo16NSPasteboardTypea8rawValueABSS_tcfC'
