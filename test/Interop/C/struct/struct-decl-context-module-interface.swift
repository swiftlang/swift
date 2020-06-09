// RUN: %target-swift-ide-test -print-module -module-to-print=StructDeclContext -I %S/Inputs -source-filename=x | %FileCheck %s

// This test checks that structs that are imported from a C module are imported
// into the top-level scope in Swift, even when they are lexically declared
// nested in other C structs.

// CHECK: struct StructRegular {
// CHECK-NEXT:   var complete_immediately: StructNestedComplete1
// CHECK-NEXT:   var completed_later: UnsafeMutablePointer<StructNestedCompletedLater1>!
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(complete_immediately: StructNestedComplete1, completed_later: UnsafeMutablePointer<StructNestedCompletedLater1>!)
// CHECK-NEXT: }
// CHECK-NEXT: struct StructNestedComplete1 {
// CHECK-NEXT:   var complete_immediately_nested: StructNestedNestedComplete1
// CHECK-NEXT:   var completed_later_nested: UnsafeMutablePointer<StructNestedNestedCompletedLater1>!
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(complete_immediately_nested: StructNestedNestedComplete1, completed_later_nested: UnsafeMutablePointer<StructNestedNestedCompletedLater1>!)
// CHECK-NEXT: }
// CHECK-NEXT: struct StructNestedNestedComplete1 {
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: struct StructNestedNestedCompletedLater1 {
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: struct StructNestedCompletedLater1 {
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: struct StructTypedefTag2 {
// CHECK-NEXT:   var complete_immediately: StructNestedComplete2
// CHECK-NEXT:   var completed_later: UnsafeMutablePointer<StructNestedCompletedLater2>!
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(complete_immediately: StructNestedComplete2, completed_later: UnsafeMutablePointer<StructNestedCompletedLater2>!)
// CHECK-NEXT: }
// CHECK-NEXT: struct StructNestedComplete2 {
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: typealias StructTypedefName2 = StructTypedefTag2
// CHECK-NEXT: struct StructNestedCompletedLater2 {
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: struct StructTypedefName3 {
// CHECK-NEXT:   var complete_immediately: StructNestedComplete3
// CHECK-NEXT:   var completed_later: UnsafeMutablePointer<StructNestedCompletedLater3>!
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(complete_immediately: StructNestedComplete3, completed_later: UnsafeMutablePointer<StructNestedCompletedLater3>!)
// CHECK-NEXT: }
// CHECK-NEXT: struct StructNestedComplete3 {
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: struct StructNestedCompletedLater3 {
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: struct StructTypedefTag4 {
// CHECK-NEXT:   var complete_immediately: StructNestedComplete4
// CHECK-NEXT:   var completed_later: UnsafeMutablePointer<StructNestedCompletedLater4>!
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(complete_immediately: StructNestedComplete4, completed_later: UnsafeMutablePointer<StructNestedCompletedLater4>!)
// CHECK-NEXT: }
// CHECK-NEXT: struct StructNestedComplete4 {
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: typealias StructTypedefName4 = UnsafeMutablePointer<StructTypedefTag4>
// CHECK-NEXT: struct StructNestedCompletedLater4 {
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: struct StructNestedComplete5 {
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: typealias StructTypedefName5 = OpaquePointer
// CHECK-NEXT: struct StructNestedCompletedLater5 {
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: struct StructTypedefName6 {
// CHECK-NEXT:   var complete_immediately: StructNestedComplete6
// CHECK-NEXT:   var completed_later: UnsafeMutablePointer<StructNestedCompletedLater6>!
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(complete_immediately: StructNestedComplete6, completed_later: UnsafeMutablePointer<StructNestedCompletedLater6>!)
// CHECK-NEXT: }
// CHECK-NEXT: struct StructNestedComplete6 {
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: typealias StructTypedefName6Ptr = UnsafeMutablePointer<StructTypedefName6>
// CHECK-NEXT: struct StructNestedCompletedLater6 {
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: struct StructTypedefName7 {
// CHECK-NEXT:   var complete_immediately: StructNestedComplete7
// CHECK-NEXT:   var completed_later: UnsafeMutablePointer<StructNestedCompletedLater7>!
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(complete_immediately: StructNestedComplete7, completed_later: UnsafeMutablePointer<StructNestedCompletedLater7>!)
// CHECK-NEXT: }
// CHECK-NEXT: struct StructNestedComplete7 {
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: typealias StructTypedefName7Ptr = UnsafeMutablePointer<StructTypedefName7>
// CHECK-NEXT: struct StructNestedCompletedLater7 {
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: struct StructTypedefName8 {
// CHECK-NEXT:   var complete_immediately: StructNestedComplete8
// CHECK-NEXT:   var completed_later: UnsafeMutablePointer<StructNestedCompletedLater8>!
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(complete_immediately: StructNestedComplete8, completed_later: UnsafeMutablePointer<StructNestedCompletedLater8>!)
// CHECK-NEXT: }
// CHECK-NEXT: struct StructNestedComplete8 {
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: typealias StructTypedefName8Ptr = UnsafeMutablePointer<StructTypedefName8>
// CHECK-NEXT: typealias StructTypedefName8PtrPtr = UnsafeMutablePointer<UnsafeMutablePointer<StructTypedefName8>?>
// CHECK-NEXT: struct StructNestedCompletedLater8 {
// CHECK-NEXT:   init()
// CHECK-NEXT: }
