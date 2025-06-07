// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-symbolgraph-extract -sdk %sdk -module-name MyError -I %t/MyError -output-dir %t -pretty-print -v -skip-protocol-implementations
// RUN: %FileCheck %s --input-file %t/MyError.symbols.json

// CHECK-NOT: "sourceOrigin"

// CHECK-NOT: "displayName": "_BridgedStoredNSError.Code"

// == is implemented as part of the hidden _BridgedStoredNSError protocol,
// but it should be hidden since it ultimately comes from Equatable
// CHECK-NOT: "precise": "s:10Foundation21_BridgedStoredNSErrorPAAE2eeoiySbx_xtFZ::SYNTHESIZED::s:SC11MyErrorCodeLeV"

// hash(into:) is implemented as part of an extension on that same protocol,
// but it should be hidden for a similar reason
// CHECK-NOT: "precise": "s:SYsSHRzSH8RawValueSYRpzrlE4hash4intoys6HasherVz_tF::SYNTHESIZED::c:@E@MyErrorCode"

// MyError
// CHECK-DAG: "precise": "s:SC11MyErrorCodeLeV"

// MyError.errFirst
// CHECK-DAG: "precise": "s:SC11MyErrorCodeLeV8errFirstSoAAVvpZ"

// MyErrorCode.Code
// CHECK-DAG: "precise": "c:@E@MyErrorCode"

// MyErrorCode.Code.init(rawValue:)
// CHECK-DAG: "precise": "s:So11MyErrorCodeV8rawValueABSgs5Int32V_tcfc"

// the following header and module map were copied from test/SourceKit/DocSupport/Inputs
//--- MyError/module.modulemap
module "MyError" {
  header "MyError.h"
  export *
}

//--- MyError/MyError.h
@import Foundation;

#define NS_ERROR_ENUM(_type, _name, _domain)  \
  enum _name : _type _name; enum __attribute__((ns_error_domain(_domain))) _name : _type

@class NSString;
extern const NSString *const MyErrorDomain;
/// This is my cool error code.
typedef NS_ERROR_ENUM(int, MyErrorCode, MyErrorDomain) {
  /// This is first error.
  MyErrFirst,
  /// This is second error.
  MyErrSecond,
};
