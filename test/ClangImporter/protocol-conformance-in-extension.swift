// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-objc-interop -emit-module -o %t/a~partial.swiftmodule -I %S/Inputs/custom-modules -module-name TEST -primary-file %s
// RUN: %target-swift-frontend -enable-objc-interop -emit-module -o %t/test.swiftmodule -I %S/Inputs/custom-modules -module-name TEST %t/a~partial.swiftmodule

import TestProtocols

// The protocol in the extension has to refine something that the base class
// conforms to to trigger the error in rdar://problem/32346184.
protocol SomeSwiftProto: Equatable {}
extension ProtocolTestingBase: Equatable {
  public static func ==(left: ProtocolTestingBase, right: ProtocolTestingBase) -> Bool {
    return left === right
  }
}

// The extension going through the typealias also makes a difference.
typealias SpecialObject = SubProtoImpl
extension SpecialObject: SomeSwiftProto {
}
