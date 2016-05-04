// RUN// RUN: %target-swift-frontend -I %S/../IDE/Inputs/custom-modules %s -emit-ir -enable-swift-newtype | FileCheck %s
import Newtype

// REQUIRES: objc_interop

// CHECK-LABEL: define %CSo8NSString* @_TF7newtype14getErrorDomainFT_VSC11ErrorDomain()
public func getErrorDomain() -> ErrorDomain {
  // CHECK: load %CSo8NSString*, %CSo8NSString** getelementptr inbounds (%VSC11ErrorDomain, %VSC11ErrorDomain* @SNTErrOne
  return .one
}

