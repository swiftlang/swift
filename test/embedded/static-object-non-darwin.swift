// RUN: %target-swift-frontend -target armv7em-none-none-eabi -parse-as-library -module-name main -O -emit-ir %s -enable-experimental-feature Embedded -Xllvm -link-embedded-runtime=0 | %FileCheck %s
// RUN: %target-swift-frontend -target armv7em-none-none-eabi -parse-as-library -module-name main -Osize -emit-ir %s -enable-experimental-feature Embedded -Xllvm -link-embedded-runtime=0 | %FileCheck %s

// UNSUPPORTED: CPU=wasm32
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: CODEGENERATOR=ARM
// REQUIRES: embedded_stdlib_cross_compiling
// REQUIRES: swift_feature_Embedded

let pots = [41,52,99,1,4,4]

public enum ValidState {
    case yes
    case no
    case undefined
}

public func checkPot(i: UInt8, testValue: UInt8) -> ValidState {
    guard i < pots.count else { return .undefined }
    return pots[Int(i)] < testValue ? .yes : .no
}

// CHECK: @"$e4main4pots_Wz" = {{.*}}global i32 0
// CHECK: @"$es20__StaticArrayStorageCN" = {{.*}}global ptr null
// CHECK: @"$e4main4pots_WZTv_r" = {{.*}}constant %Ts23_ContiguousArrayStorageCySiG_tailelems0 {{.*}}@"$es20__StaticArrayStorageCN"
// CHECK: @"$e4main4potsSaySiGvp" = {{.*}}constant %TSa {{.*}}@"$e4main4pots_WZTv_r"
