// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/inputs)
// RUN: %empty-directory(%t/modulecache)
// RUN: echo "// swift-interface-format-version: 1.0" > %t/inputs/Foo.swiftinterface
// RUN: echo "// swift-module-flags: -module-name Foo" >> %t/inputs/Foo.swiftinterface
// RUN: echo "@available(macOS 11, *)" >> %t/inputs/Foo.swiftinterface
// RUN: echo "public class Foo {}" >> %t/inputs/Foo.swiftinterface
// RUN: echo "public extension Foo { public func f() {} }" >> %t/inputs/Foo.swiftinterface

// RUN: not %target-swift-frontend -emit-module-path %t/Bar.swiftmodule -enable-library-evolution -enable-objc-interop -disable-objc-attr-requires-foundation-module -module-name Bar %s -I %t/inputs -target x86_64-apple-macos10.9 -module-cache-path %t/modulecache

// RUN: %target-swift-frontend -emit-module-path %t/Bar.swiftmodule -enable-library-evolution -enable-objc-interop -disable-objc-attr-requires-foundation-module -module-name Bar %s -I %t/inputs -target x86_64-apple-macos10.9 -disable-availability-checking -module-cache-path %t/modulecache

import Foo

