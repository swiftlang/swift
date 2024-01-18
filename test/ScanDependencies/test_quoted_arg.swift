// RUN: %empty-directory(%t)	
// RUN: %empty-directory(%t/module-cache)		
// RUN: %empty-directory(%t/inputs)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/module-cache %t/test.swift -o %t/deps.json -I %t/inputs -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import
// RUN: %validate-json %t/deps.json > %t/validated_deps.json
// RUN: %FileCheck %s < %t/validated_deps.json

//--- inputs/Foo.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name Foo
// swift-module-flags-ignorable-private: -package-name "\"ManyFoos\""
public func foo() {}

//--- test.swift
import Foo

// CHECK:      "-package-name"
// CHECK-NEXT: "\"ManyFoos\""
