// RUN: rm -rf %t
// RUN: mkdir %t
//
// Build swift modules this test depends on.
// RUN: %swift -emit-module -target x86_64-apple-darwin13 %S/Inputs/foo_swift_module.swift -emit-module-path %t/foo_swift_module.swiftmodule -emit-module-doc-path %t/foo_swift_module.swiftdoc
//
// RUN: %swift-ide-test -print-module -source-filename %s -I %t -module-cache-path %t/clang-module-cache -module-to-print=foo_swift_module > %t.printed.txt
// RUN: diff %t.printed.txt %S/Inputs/foo_swift_module.printed.comments.txt

