// RUN: %empty-directory(%t)

// RUN: echo '@_typeEraser(AnyView) public protocol View {}' > %t/Module.swift
// RUN: echo 'public struct AnyView : View {' >> %t/Module.swift
// RUN: echo ' public init<T: View>(erasing: T) {}' >> %t/Module.swift
// RUN: echo '}' >> %t/Module.swift
// RUN: %swift -emit-module -o %t/Module.swiftmodule %t/Module.swift -parse-as-library

// RUN: %sourcekitd-test -req=cursor -pos=13:13 %s -- -I %t %s | %FileCheck %s

import Module

struct Foo: View {}

// CHECK: <Declaration>@_typeEraser(<Type usr="s:6Module7AnyViewV">AnyView</Type>) protocol View</Declaration>
