// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/lib.swift \
// RUN:   -emit-module-path %t/lib.swiftmodule -module-name lib \
// RUN:   -parse-as-library -enable-library-evolution \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -enable-experimental-feature CustomAvailability

// RUN: %target-swift-frontend %t/client.swift \
// RUN:   -emit-ir -module-name client \
// RUN:   -parse-as-library -enable-library-evolution \
// RUN:   -I %t -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -enable-experimental-feature CustomAvailability | %FileCheck %s

// REQUIRES: swift_feature_CustomAvailability

// https://github.com/swiftlang/swift/issues/80058
// UNSUPPORTED: OS=linux-android

//--- lib.swift

import Oceans

@export(implementation)
public func call_available_in_arctic() {
  if #available(Arctic) {
    available_in_arctic()
  }
}

//--- client.swift

import lib

// CHECK-LABEL: define linkonce_odr hidden swiftcc void @"$s3lib24call_available_in_arcticyyF"()
// CHECK:         call swiftcc i1 @"$sSC26__swift_Arctic_isAvailableBi1_yF"()
// CHECK:         call void @available_in_arctic()

// CHECK-LABEL: define linkonce_odr hidden swiftcc i1 @"$sSC26__swift_Arctic_isAvailableBi1_yF"()
// CHECK:         call i32 @arctic_pred()
public func test() {
  call_available_in_arctic()
}
