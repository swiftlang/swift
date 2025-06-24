// RUN: %target-swift-frontend -target %target-triple -module-name main -parse-as-library -emit-ir %s -enable-experimental-feature Embedded | %FileCheck %s
// REQUIRES: swift_in_compiler
// REQUIRES: CODEGENERATOR=ARM
// REQUIRES: embedded_stdlib_cross_compiling
// REQUIRES: swift_feature_Embedded

@main
public struct Application {
    public static func main() {
        var x: UInt64 = 0
        x <<= 8
    }
}

enum MyEnum: UInt8 {
    case a = 0
}

// CHECK: define {{.*}}@{{_*}}main{{.*}}(

