// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-module-interface(%t/Test.swiftinterface) %s \
// RUN:   -define-availability "_iOS8Aligned:macOS 10.10, iOS 8.0" \
// RUN:   -define-availability "_iOS9Aligned:macOS 10.11, iOS 9.0" \
// RUN:   -define-availability "_iOS9:iOS 9.0" \
// RUN:   -define-availability "_macOS10_11:macOS 10.11" \
// RUN:   -define-availability "_myProject 1.0:macOS 10.11" \
// RUN:   -define-availability "_myProject 2.5:macOS 10.12" \
// RUN:   -define-availability "_emptyMacro:*" \
// RUN:   -define-availability "MyLib_v1_0:iOS 13.0, macOS 10.15, tvOS 13.0, watchOS 6.0"

// RUN: %target-swift-typecheck-module-from-interface(%t/Test.swiftinterface)
// RUN: %FileCheck %s < %t/Test.swiftinterface

@available(_iOS8Aligned, *)
public func onMacOS10_10() {}
// CHECK: @available(macOS 10.10, iOS 8.0, *)
// CHECK-NEXT: public func onMacOS10_10

@available(_iOS9Aligned, *)
public func onMacOS10_11() {}
// CHECK: @available(macOS 10.11, iOS 9.0, *)
// CHECK-NEXT: public func onMacOS10_11()

@available(_iOS9, _macOS10_11, tvOS 11.0, *)
public func composed() {}
// CHECK: @available(iOS 9.0, macOS 10.11, tvOS 11.0, *)
// CHECK-NEXT: public func composed()

@available(_myProject 1.0, *)
public func onMyProjectV1() {}
// CHECK: @available(macOS 10.11, *)
// CHECK-NEXT: public func onMyProjectV1

@available(_myProject 2.5, *)
public func onMyProjectV2_5() {}
// CHECK: @available(macOS 10.12, *)
// CHECK-NEXT: public func onMyProjectV2_5

@available(_emptyMacro, *)
public func emptyMacro() {}
// CHECK-NOT: @available
// CHECK-NEXT: public func emptyMacro()

@_specialize(exported: true, availability: SwiftStdlib 5.1, *; where T == Int)
public func testSemanticsAvailability<T>(_ t: T) {}
// CHECK: @_specialize(exported: true, kind: full, availability: macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *; where T == Swift::Int)
// CHECK-NEXT: public func testSemanticsAvailability

// A library's availability macro can overlap with an explicit platform
// requirement, regardless of which attribute appears first.
@available(macOS 15.0, *)
@available(MyLib_v1_0, *)
public func api() {}
// CHECK: @available(macOS 15.0, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
// CHECK-NEXT: public func api()

@available(MyLib_v1_0, *)
@available(macOS 15.0, *)
public func apiWithMacroFirst() {}
// CHECK-NEXT: @available(iOS 13.0, macOS 15.0, tvOS 13.0, watchOS 6.0, *)
// CHECK-NEXT: public func apiWithMacroFirst()
