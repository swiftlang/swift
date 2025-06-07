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
// Ensure that no OS version check is generated because the current platform falls under the wildcard '*'
// CHECK-NOT: function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
public enum Metasyntactics: String, RawRepresentable {
    case foo
    @available(iOS 17.0, *)
    case bar
    @available(iOS 99.0, *)
    case baz
    @available(iOS 1.0, *)
    case qux
    @available(iOS 16.0, *)
    case quux
    @available(iOS 999.9.9, *)    
    case corge
    @available(iOS 18.0, *)    
    case grault
    case garply
    case waldo
    case fred
    case plugh
    case xyzzy
    case thud
}
