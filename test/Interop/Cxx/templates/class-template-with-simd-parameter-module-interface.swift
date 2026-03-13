// RUN: %target-swift-ide-test -print-module -module-to-print=ClassTemplateWithSIMDParameter -I %S/Inputs -source-filename=x -cxx-interoperability-mode=upcoming-swift | %FileCheck %s

// REQUIRES: OS=macosx || OS=ios

// CHECK: typealias TemplatedSIMDUInt1 = Templated<CUnsignedInt>
// CHECK: typealias TemplatedSIMDUInt16 = Templated<SIMD16<CUnsignedInt>>
// CHECK: typealias TemplatedSIMDFloat3 = Templated<SIMD3<CFloat>>
// CHECK: typealias TemplatedSIMDFloat4 = Templated<SIMD4<CFloat>>
// CHECK: typealias TemplatedSIMDDouble8 = Templated<SIMD8<CDouble>>
