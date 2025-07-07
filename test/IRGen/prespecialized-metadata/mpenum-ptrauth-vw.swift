// RUN: %swift -prespecialize-generic-metadata -target %module-target-future -emit-irgen %s | %IRGenFileCheck %s

// REQUIRES: CPU=arm64e

public enum R<X> {
case a(X)
case b(Any)
}

@inline(never)
func consume<T>(_ t: T) {}

// CHECK:      @"$s4main1ROySiGWV" = {{.*}}%swift.enum_vwtable {
//           :   ptr @"$s4main1ROwCP.ptrauth.1",
//           :   ptr @"$s4main1ROwxx.ptrauth.2",
//           :   ptr @"$s4main1ROwcp.ptrauth.3",
//           :   ptr @"$s4main1ROwca.ptrauth.4",
//           :   ptr @"$s4main1ROwtk.ptrauth.5",
//           :   ptr @"$s4main1ROwta.ptrauth.6",
// CHECK-SAME:   ptr @swift_getMultiPayloadEnumTagSinglePayload.ptrauth,
// CHECK-SAME:   ptr @swift_storeMultiPayloadEnumTagSinglePayload.ptrauth,
//           :   i64 33,
//           :   i64 40,
//           :   i32 2293767,
//           :   i32 254,
//           :   ptr @"$s4main1ROwug.ptrauth.7",
//           :   ptr @"$s4main1ROwup.ptrauth.8",
//           :   ptr @"$s4main1ROwui.ptrauth.9"
//           : }
consume(R<Int>.self)
