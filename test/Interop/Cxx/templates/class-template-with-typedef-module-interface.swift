// RUN: %target-swift-ide-test -print-module -module-to-print=ClassTemplateWithTypedef -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: struct Lander<CChar> {
// CHECK:   init()
// CHECK:   typealias size_type = {{UInt|UInt32}}
// CHECK:   mutating func test(_: {{UInt|UInt32}})
// CHECK: }
// CHECK: typealias Surveyor = Lander<CChar>
