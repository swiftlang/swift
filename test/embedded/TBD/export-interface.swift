// Under CodeGenerationModel=implementation, only @export(interface)
// declarations have strong definitions, so they are all that the TBD contains.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir -o /dev/null %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation -validate-tbd-against-ir=all
// RUN: %target-swift-frontend -emit-ir -o /dev/null %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation -validate-tbd-against-ir=all -O

// RUN: %target-swift-frontend -typecheck %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation -emit-tbd-path %t/Lib.tbd -tbd-install_name Lib
// RUN: %FileCheck %s < %t/Lib.tbd
// RUN: %FileCheck -check-prefix NEGATIVE %s < %t/Lib.tbd

// REQUIRES: swift_feature_Embedded
// REQUIRES: VENDOR=apple

public protocol Greeter {
  func greet()
}

// The metadata and conformances of an @export(interface) type are strongly
// defined. Its members are not, unless they are @export(interface)
// themselves.
// CHECK-DAG: "_$e3Lib8ExportedVMf"
// CHECK-DAG: "_$e3Lib8ExportedVN"
// CHECK-DAG: "_$e3Lib8ExportedVAA7GreeterAAWP"
// CHECK-DAG: "_$e3Lib8ExportedV5greetyyF"
// NEGATIVE-NOT: 8ExportedVACycfC
@export(interface)
public struct Exported: Greeter {
  public init() {}
  @export(interface)
  public func greet() {}
}

// Other types have their metadata, conformances, and members emitted on
// demand into each module that uses them.
// NEGATIVE-NOT: 5PlainV
public struct Plain: Greeter {
  public init() {}
  public func greet() {}
}

// CHECK-DAG: "_$e3Lib9exportedFyyF"
@export(interface)
public func exportedF() {}

// NEGATIVE-NOT: plainF
public func plainF() {}
