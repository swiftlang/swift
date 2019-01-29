// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s

// REQUIRES: objc_interop

// Check that type metadata defined inside extensions of files imported from
// other modules is emitted with the right linkage.
//
// In particular, it should be possible to define types inside extensions of
// types imported from Foundation (rdar://problem/27245620).

import Foundation

// CHECK-LABEL: @"$sSo8NSNumberC31extension_type_metadata_linkingE4BaseCMm" = global
// CHECK-LABEL: @"$sSo8NSNumberC31extension_type_metadata_linkingE4BaseCMn" = constant
// CHECK-LABEL: @"$sSo8NSNumberC31extension_type_metadata_linkingE4BaseCMf" = internal global

// CHECK-LABEL: @"$sSo8NSNumberC31extension_type_metadata_linkingE7DerivedCMm" = global
// CHECK-LABEL: @"$sSo8NSNumberC31extension_type_metadata_linkingE7DerivedCMn" = constant
// CHECK-LABEL: @"$sSo8NSNumberC31extension_type_metadata_linkingE7DerivedCMf" = internal global

// CHECK-LABEL: @"$sSo8NSNumberC31extension_type_metadata_linkingE6StructVMn" = constant
// CHECK-LABEL: @"$sSo8NSNumberC31extension_type_metadata_linkingE6StructVMf" = internal constant

// CHECK-LABEL: "$sSo18NSComparisonResultVMn" = linkonce_odr hidden

// CHECK-LABEL: @"$sSo8NSNumberC31extension_type_metadata_linkingE4BaseCN" = alias
// CHECK-LABEL: @"$sSo8NSNumberC31extension_type_metadata_linkingE7DerivedCN" = alias
// CHECK-LABEL: @"$sSo8NSNumberC31extension_type_metadata_linkingE6StructVN" = alias

// CHECK-LABEL: define swiftcc %swift.metadata_response @"$sSo8NSNumberC31extension_type_metadata_linkingE4BaseCMa"
// CHECK-LABEL: define swiftcc %swift.metadata_response @"$sSo8NSNumberC31extension_type_metadata_linkingE7DerivedCMa"

// FIXME: Not needed
// CHECK-LABEL: define swiftcc %swift.metadata_response @"$sSo8NSNumberC31extension_type_metadata_linkingE6StructVMa"

extension NSNumber {
  public class Base : CustomStringConvertible {
    public var description: String {
      return "Base"
    }
  }

  public class Derived : Base {
    override public var description: String {
      return "Derived"
    }
  }

  public struct Struct {}
}

// SR-9397: not emitting metadata for NSComparisonResult
protocol CommandTypes {
    associatedtype Result
    associatedtype Message
}

struct EnumCommand: CommandTypes {
    typealias Result = ComparisonResult
    typealias Message = String
}

struct Command<T: CommandTypes> {
    var result: T.Result?
    var message: T.Message?
}

func createCommandArray() -> Any {
  return [Command<EnumCommand>]()
}
