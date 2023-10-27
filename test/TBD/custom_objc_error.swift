// REQUIRES: VENDOR=apple
// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -I %S/Inputs/ -disable-objc-attr-requires-foundation-module -emit-tbd -emit-tbd-path %t/test.tbd -tbd-install_name objc_err

import Foundation
@_exported import CustomError

extension CustomError : CustomStringConvertible {
  public var description: String {
    let nsError = self as NSError
    return nsError.description
  }
}
