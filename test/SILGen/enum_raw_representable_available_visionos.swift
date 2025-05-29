// Mock SDK will contain an appropriate SDKSettings and a loadable stdlib
// RUN: %empty-directory(%t/mock-sdk)
// RUN: %empty-directory(%t/mock-sdk/usr/lib/swift)
// RUN: cp -r %test-resource-dir/xros/Swift.swiftmodule %t/mock-sdk/usr/lib/swift/Swift.swiftmodule
// RUN: cp %S/Inputs/mock-visionos-sdk/SDKSettings.json %t/mock-sdk/SDKSettings.json
// RUN: %swift -emit-sil -parse-as-library %s -target arm64-apple-xros1.0 -sdk %t/mock-sdk -I %t/mock-sdk/usr/lib/swift/ -verify
// RUN: %swift -emit-silgen -parse-as-library %s -target arm64-apple-xros1.0 -sdk %t/mock-sdk -I %t/mock-sdk/usr/lib/swift/ -o %t/output.sil
// RUN: %FileCheck %s < %t/output.sil

// REQUIRES: OS=xros

// CHECK-LABEL: // Metasyntactics.init(rawValue:)
// Ensure that OS version check is generated for the current platform
// CHECK: function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
public enum Metasyntactics: String, RawRepresentable {
    case foo
    @available(visionOS 1.1, *)
    case bar
}
