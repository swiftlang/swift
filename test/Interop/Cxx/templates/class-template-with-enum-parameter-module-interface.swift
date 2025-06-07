// RUN: %target-swift-ide-test -print-module -module-to-print=ClassTemplateWithEnumParameter -I %S/Inputs -source-filename=x -cxx-interoperability-mode=upcoming-swift | %FileCheck %s

// CHECK: typealias WrappedEnum = Wrapper<MyEnum>
// CHECK: typealias WrappedEnumClass = Wrapper<MyEnumClass>
// CHECK: typealias WrappedTypedefEnum = Wrapper<MyTypedefEnum>
