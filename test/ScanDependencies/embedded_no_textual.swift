// REQUIRES: swift_feature_Embedded
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Foo.swift -emit-module-path %t/Foo.swiftmodule/%target-swiftmodule-name -module-name Foo -emit-module-interface-path %t/Foo.swiftmodule/%target-swiftinterface-name -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -enable-library-evolution

// RUN: %target-swift-frontend -scan-dependencies \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/main.swift -I %t -enable-experimental-feature Embedded -o - | %FileCheck %s --check-prefix=SERIALIZED

// SERIALIZED: "swiftPrebuiltExternal": "Foo"

//--- main.swift
import Foo

//--- Foo.swift
public func foo() {}

