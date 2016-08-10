// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -I %t -I %S/Inputs/custom-modules -print-module -module-to-print=ImportedProtocols.SubModule -source-filename %s > %t.printed.txt
// RUN: %FileCheck %s < %t.printed.txt 

// REQUIRES: objc_interop

// CHECK: protocol ImportedProtocolBase : NSObjectProtocol {
// CHECK: }
// CHECK: typealias ImportedProtocolBase_t = ImportedProtocolBase
// CHECK: protocol IPSub : ImportedProtocolBase {
// CHECK: }
// CHECK: typealias IPSub_t = IPSub
// CHECK: typealias Dummy = IPSub
// CHECK: func takesIPSub(_ in: IPSub_t!)

import ImportedProtocols
func noop(_ p: ImportedProtocolSub) {}
