// RUN: %target-build-swift -Xfrontend -safe-conditional-runtime-records -emit-ir %s -o - | %FileCheck %s

// REQUIRES: PTRSIZE=64

public protocol Protocol {}

// Reflection metadata field descriptor for protocol live if protocol descriptor is live
// CHECK-DAG:  @"__$s4main8Protocol_pMF_cl" = private constant { ptr, i64, ptr } { ptr @"$s4main8Protocol_pMF", i64 1, ptr @"$s4main8ProtocolMp" }, section "__DATA,__llvm_condlive", align 8

// Protocol descriptor runtime record live if protocol descriptor is live
// CHECK-DAG:  @"__$s4main8ProtocolHr_cl" = private constant { ptr, i64, ptr } { ptr @"$s4main8ProtocolHr", i64 1, ptr @"$s4main8ProtocolMp" }, section "__DATA,__llvm_condlive", align 8

public class Class : Protocol {}

// Reflection metadata field descriptor for class live if nominal type descriptor is live
// CHECK-DAG:  @"__$s4main5ClassCMF_cl" = private constant { ptr, i64, ptr } { ptr @"$s4main5ClassCMF", i64 1, ptr @"$s4main5ClassCMn" }, section "__DATA,__llvm_condlive", align 8

// Protocol conformance descriptor runtime record class : protocol live if both protocol descriptor and nominal type descriptor are live
// CHECK-DAG:  @"__$s4main5ClassCAA8ProtocolAAHc_cl" = private constant { ptr, i64, ptr, ptr } { ptr @"$s4main5ClassCAA8ProtocolAAHc", i64 2, ptr @"$s4main8ProtocolMp", ptr @"$s4main5ClassCMn" }, section "__DATA,__llvm_condlive", align 8

// Nominal type descriptor runtime record for class live if nominal type descriptor is live
// CHECK-DAG:  @"__$s4main5ClassCHn_cl" = private constant { ptr, i64, ptr } { ptr @"$s4main5ClassCHn", i64 1, ptr @"$s4main5ClassCMn" }, section "__DATA,__llvm_condlive", align 8

public struct Struct : Protocol {}

// Reflection metadata field descriptor for struct live if nominal type descriptor is live
// CHECK-DAG:  @"__$s4main6StructVMF_cl" = private constant { ptr, i64, ptr } { ptr @"$s4main6StructVMF", i64 1, ptr @"$s4main6StructVMn" }, section "__DATA,__llvm_condlive", align 8

// Protocol conformance descriptor runtime record struct : protocol live if both protocol descriptor and nominal type descriptor are live
// CHECK-DAG:  @"__$s4main6StructVAA8ProtocolAAHc_cl" = private constant { ptr, i64, ptr, ptr } { ptr @"$s4main6StructVAA8ProtocolAAHc", i64 2, ptr @"$s4main8ProtocolMp", ptr @"$s4main6StructVMn" }, section "__DATA,__llvm_condlive", align 8

// Nominal type descriptor runtime record for struct if nominal type descriptor is live
// CHECK-DAG:  @"__$s4main6StructVHn_cl" = private constant { ptr, i64, ptr } { ptr @"$s4main6StructVHn", i64 1, ptr @"$s4main6StructVMn" }, section "__DATA,__llvm_condlive", align 8

public enum Enum {}

// Reflection metadata field descriptor for enum live if nominal type descriptor is live
// CHECK-DAG:  @"__$s4main4EnumOMF_cl" = private constant { ptr, i64, ptr } { ptr @"$s4main4EnumOMF", i64 1, ptr @"$s4main4EnumOMn" }, section "__DATA,__llvm_condlive", align 8

// Nominal type descriptor runtime record for enum live if nominal type descriptor is live
// CHECK-DAG:  @"__$s4main4EnumOHn_cl" = private constant { ptr, i64, ptr } { ptr @"$s4main4EnumOHn", i64 1, ptr @"$s4main4EnumOMn" }, section "__DATA,__llvm_condlive", align 8

// When targeting ELF, llvm.compiler.used is used instead of llvm.used: https://github.com/apple/swift/pull/39150
// CHECK-DAG:@llvm.{{(compiler.)?}}used = appending global{{.*}}ptr @"__$s4main8Protocol_pMF_cl"{{.*}}ptr @"__$s4main5ClassCMF_cl"{{.*}}ptr @"__$s4main6StructVMF_cl"{{.*}}ptr @"__$s4main4EnumOMF_cl"{{.*}}ptr @"__$s4main8ProtocolHr_cl"{{.*}}ptr @"__$s4main5ClassCAA8ProtocolAAHc_cl"{{.*}}ptr @"__$s4main6StructVAA8ProtocolAAHc_cl"{{.*}}ptr @"__$s4main5ClassCHn_cl"{{.*}}ptr @"__$s4main6StructVHn_cl"{{.*}}ptr @"__$s4main4EnumOHn_cl"
