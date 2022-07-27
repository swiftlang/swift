// RUN: %target-swift-ide-test -print-module -module-to-print=MemberTemplates -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: struct HasMemberTemplates {
// CHECK:   mutating func addSameTypeParams<T>(_ a: T, _ b: T) -> T
// CHECK:   mutating func addMixedTypeParams<T, U>(_ a: T, _ b: U) -> T
// CHECK:   mutating func addAll<T, U>(_ a: Int32, _ b: T, _ c: U) -> Int32
// CHECK:   mutating func passThrough<T>(_ val: T) -> T
// CHECK:   mutating func passThroughConst<T>(_ val: T) -> T
// CHECK:   func passThroughOnConst<T>(_ val: T) -> T
// CHECK:   func passThroughConstOnConst<T>(_ val: T) -> T
// CHECK:   mutating func doNothingConstRef<T>(_ val: T)
// CHECK:   mutating func make42Ref<T>(_ val: inout T)
// CHECK: }

// CHECK: struct __CxxTemplateInst32TemplateClassWithMemberTemplatesIiE {
// CHECK:   init(_ val: Int32)
// CHECK:   var value: Int32
// CHECK:   mutating func setValue<U>(_ val: U)
// CHECK: }

// CHECK: typealias IntWrapper = __CxxTemplateInst32TemplateClassWithMemberTemplatesIiE

// CHECK: struct HasStaticMemberTemplates {
// CHECK:   init()
// CHECK:   static func add<T>(_ a: T, _ b: T) -> T
// CHECK:   static func addTwoTemplates<T, U>(_ a: T, _ b: U) -> T
// CHECK:   static func removeReference<T>(_ a: inout T) -> T
// CHECK: }

// CHECK: struct __CxxTemplateInst17MyTemplatedStructIiE {
// CHECK:   init()
// CHECK: }

// CHECK: struct HasTemplatedField {
// CHECK:   init(x: __CxxTemplateInst17MyTemplatedStructIiE)
// CHECK:   var x: __CxxTemplateInst17MyTemplatedStructIiE
// CHECK: }
