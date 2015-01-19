// RUN: rm -rf %t
// RUN: mkdir %t
//
// Build swift modules this test depends on.
// RUN: %target-swift-frontend -emit-module %S/Inputs/foo_swift_module.swift -emit-module-path %t/foo_swift_module.swiftmodule -emit-module-doc-path %t/foo_swift_module.swiftdoc
//
// RUN: %target-swift-ide-test -print-module -source-filename %s -I %t -module-to-print=foo_swift_module > %t.printed.txt
// RUN: diff %t.printed.txt %S/Inputs/foo_swift_module.printed.comments.txt

