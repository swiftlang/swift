// RUN: %target-swift-ide-test -print-module -module-to-print=ClassTemplateWithTypedef -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: struct Lander<CChar> {
// CHECK:   init()
// CHECK:   typealias size_type = {{CUnsignedLong|CUnsignedInt}}
// CHECK:   mutating func test(_: {{CUnsignedLong|CUnsignedInt}})
// CHECK: }
// CHECK: typealias Surveyor = Lander<CChar>
