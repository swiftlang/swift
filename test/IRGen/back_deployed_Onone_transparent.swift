// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/back_deployed.swift -o %t/ -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -emit-ir %s -I %t -Onone | %FileCheck %s

// _stdlib_isOSVersionAtLeast() is @_transparent on iOS
// REQUIRES: OS=ios

import back_deployed

public func test() {
  backDeployedFunc()
}

// CHECK: define{{.*}} hidden swiftcc void @"$s13back_deployed0A12DeployedFuncyyFTwb"
// CHECK:   call swiftcc i1 @"$ss31_stdlib_isOSVersionAtLeast_AEICyBi1_Bw_BwBwtF"
// CHECK:   call swiftcc void @"$s13back_deployed0A12DeployedFuncyyFTwB"
// CHECK:   call swiftcc void @"$s13back_deployed0A12DeployedFuncyyF"

// CHECK: define{{.*}} hidden swiftcc i1 @"$ss31_stdlib_isOSVersionAtLeast_AEICyBi1_Bw_BwBwtF"
// CHECK:   call i32 @__isPlatformVersionAtLeast
