// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/inputs)
// RUN: %empty-directory(%t/modulecache)
// RUN: echo "public class HideyHole { @_spi(Private) public init() {} }" > %t/Foo.swift
// RUN: echo "public class StashyCache: HideyHole {}" >> %t/Foo.swift

// RUN: %target-swift-frontend -emit-module -emit-module-interface-path %t/inputs/Foo.swiftinterface %t/Foo.swift -module-name Foo

// RUN: %target-swift-frontend -emit-module-path %t/Bar.swiftmodule -enable-library-evolution -enable-objc-interop -disable-objc-attr-requires-foundation-module -module-name Bar %s -I %t/inputs -disable-availability-checking -module-cache-path %t/modulecache

import Foo