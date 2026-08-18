// Embedded Swift promotes -g to -gdwarf-types and describes types in DWARF,
// so there is no need to warn about disabling reflection metadata.

// RUN: %target-swift-frontend -emit-ir -o /dev/null -g -parse-stdlib -enable-experimental-feature Embedded -target arm64e-apple-none -wmo %s 2>&1 | %FileCheck %s --allow-empty
// RUN: %target-swift-frontend -emit-ir -o /dev/null -g -disable-reflection-metadata -parse-stdlib -enable-experimental-feature Embedded -target arm64e-apple-none -wmo %s 2>&1 | %FileCheck %s --allow-empty

// REQUIRES: swift_feature_Embedded

// CHECK-NOT: prevent variable inspection

public struct S {}
public func f(s: S) {}
