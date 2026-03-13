// RUN: %target-swift-ide-test -print-header -header-to-print %S/Inputs/bridging-header.h -import-objc-header %S/Inputs/bridging-header.h -source-filename=%s -cxx-interoperability-mode=upcoming-swift | %FileCheck %s

// CHECK: struct InBridgingHeader1
// CHECK: struct InBridgingHeader2
