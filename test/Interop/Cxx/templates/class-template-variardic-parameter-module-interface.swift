// RUN: %target-swift-ide-test -print-module -module-to-print=ClassTemplateVariadic -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=ClassTemplateVariadic -I %S/Inputs -source-filename=x -cxx-interoperability-mode=swift-6 | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=ClassTemplateVariadic -I %S/Inputs -source-filename=x -cxx-interoperability-mode=upcoming-swift | %FileCheck %s

// CHECK: @available(*, unavailable
// CHECK: struct Tuple<Ts> {
// CHECK: }

// CHECK: typealias Single = Tuple<IntWrapper>
// CHECK: typealias Pair = Tuple<IntWrapper, IntWrapper>
// CHECK: typealias Triple = Tuple<IntWrapper, IntWrapper, IntWrapper>
// CHECK: typealias Nested = Tuple<Tuple<IntWrapper, IntWrapper>, Tuple<IntWrapper, IntWrapper>>
