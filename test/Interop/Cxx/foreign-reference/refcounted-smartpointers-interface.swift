// RUN: %target-swift-ide-test -print-module -module-to-print=RefCountedSmartPtrs -I %S/Inputs -I %swift_src_root/lib/ClangImporter/SwiftBridging -source-filename=x -cxx-interoperability-mode=upcoming-swift | %FileCheck %s

// CHECK: func bridgedFunction(_ ptr: RefCountedBase)
// CHECK: func bridgedFunction2(_ ptr: RefCountedBase?)
// CHECK: func bridgedFunction3() -> RefCountedBase
// CHECK: func bridgedFunction4() -> RefCountedBase?
