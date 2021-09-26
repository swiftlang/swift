// RUN: %swift -target arm64-apple-macos11.0 -parse-stdlib %s -module-name Swift -emit-ir -o - | %FileCheck %s

public protocol Simple {}

public struct Other : Simple {}

// CHECK:      @"\01l_protocols" = private constant
// CHECK-SAME: @"$ss6SimpleMp"
// CHECK-SAME: , section "__TEXT, __swift5_protos, regular"

// CHECK:      @"\01l_protocol_conformances" = private constant
// CHECK-SAME: @"$ss5OtherVs6SimplesMc"
// CHECK-SAME: , section "__TEXT, __swift5_proto, regular"

// CHECK:      @"\01l_type_metadata_table" = private constant
// CHECK-SAME: @"$ss5OtherVMn"
// CHECK-SAME: , section "__TEXT, __swift5_types, regular"

// CHECK:      @llvm.used = appending global [{{.*}} x i8*] [
// CHECK-SAME: @"\01l_protocols"
// CHECK-SAME: @"\01l_protocol_conformances"
// CHECK-SAME: @"\01l_type_metadata_table"
// CHECK-SAME: ], section "llvm.metadata"
