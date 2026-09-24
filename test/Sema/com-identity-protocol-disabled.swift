// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM %t/COM.swift
// RUN: %target-swift-frontend -typecheck -verify -I %t %t/client.swift

//--- COM.swift
public protocol COMInterface {}
public protocol COMActivatable {}

//--- client.swift
import COM

// These names impose no identity restrictions with COM interop disabled.
protocol RefinesInterface: COMInterface & Sendable {}
protocol RefinesActivatable: COMActivatable & Sendable {}
struct ExplicitInterface: COMInterface {}
struct ExplicitActivatable: COMActivatable {}
extension COMInterface {
  var member: Int { 0 }
}
extension COMActivatable {
  var member: Int { 0 }
}
func interface(_: any COMInterface) {}
func activatable(_: any COMActivatable) {}
