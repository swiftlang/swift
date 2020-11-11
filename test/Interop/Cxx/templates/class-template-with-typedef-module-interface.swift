// RUN: %target-swift-ide-test -print-module -module-to-print=ClassTemplateWithTypedef -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: struct __CxxTemplateInst6LanderIcE {
// CHECK:   typealias size_type = {{UInt|UInt32}}
// CHECK:   init()
// CHECK:   mutating func test(_: {{UInt|UInt32}})
// CHECK: }
// CHECK: typealias Surveyor = __CxxTemplateInst6LanderIcE
