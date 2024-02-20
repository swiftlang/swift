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

// CHECK: struct TemplateClassWithMemberTemplates<CInt> {
// CHECK:   init(_ val: Int32)
// CHECK:   var value: Int32
// CHECK:   mutating func setValue<U>(_ val: U)
// CHECK: }

// CHECK: typealias IntWrapper = TemplateClassWithMemberTemplates<CInt>

// CHECK: struct HasStaticMemberTemplates {
// CHECK:   init()
// CHECK:   static func add<T>(_ a: T, _ b: T) -> T
// CHECK:   static func addTwoTemplates<T, U>(_ a: T, _ b: U) -> T
// CHECK:   static func removeReference<T>(_ a: inout T) -> T
// CHECK: }

// CHECK: struct MyTemplatedStruct<CInt> {
// CHECK:   init()
// CHECK: }

// CHECK: struct HasTemplatedField {
// CHECK:   init(x: MyTemplatedStruct<CInt>)
// CHECK:   var x: MyTemplatedStruct<CInt>
// CHECK: }

// CHECK: struct HasUninstantiatableTemplateMember<NoDefinition, TemplateClassWithMemberTemplates<NoDefinition>> {
// CHECK:   init(pointer: OpaquePointer!)
// CHECK:   var pointer: OpaquePointer!
// CHECK: }

// CHECK: struct HasTemplateInstantiationWithForwardDecl {
// CHECK:   init(noDefMember: HasUninstantiatableTemplateMember<NoDefinition, TemplateClassWithMemberTemplates<NoDefinition>>)
// CHECK:   var noDefMember: HasUninstantiatableTemplateMember<NoDefinition, TemplateClassWithMemberTemplates<NoDefinition>>
// CHECK: }
