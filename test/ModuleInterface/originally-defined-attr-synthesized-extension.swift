// RUN: %empty-directory(%t)

// Ensure the originallyDefinedIn attribute is printed in swiftinterface files for synthesized extensions
// RUN: %target-swift-emit-module-interface(%t/Foo.swiftinterface) %s -module-name Foo
// RUN: %target-swift-typecheck-module-from-interface(%t/Foo.swiftinterface) -module-name Foo
// RUN: %FileCheck %s < %t/Foo.swiftinterface

@available(OSX 10.7, iOS 7.0, *)
@_originallyDefinedIn(module: "Bar", OSX 10.9, iOS 13.0)
public enum MyCase: Int {
	case first
	case second
}

// CHECK: @_originallyDefinedIn(module: "Bar", macOS 10.9)
// CHECK: @_originallyDefinedIn(module: "Bar", iOS 13.0)
// CHECK: public enum MyCase : Swift.Int

// CHECK: @_originallyDefinedIn(module: "Bar", macOS 10.9)
// CHECK: @_originallyDefinedIn(module: "Bar", iOS 13.0)
// CHECK: extension Foo.MyCase : Swift.Equatable {}

// CHECK: @_originallyDefinedIn(module: "Bar", macOS 10.9)
// CHECK: @_originallyDefinedIn(module: "Bar", iOS 13.0)
// CHECK: extension Foo.MyCase : Swift.Hashable {}

