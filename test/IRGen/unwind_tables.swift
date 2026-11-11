// RUN: %swift_frontend_plain -target aarch64-unknown-linux-gnu -parse-stdlib -module-name main -emit-ir %s | %FileCheck %s --check-prefix=ASYNC
// RUN: %swift_frontend_plain -target arm64-apple-macos14 -parse-stdlib -module-name main -emit-ir %s | %FileCheck %s --check-prefix=SYNC
// RUN: %swift_frontend_plain -target aarch64-unknown-linux-gnu -parse-stdlib -module-name main -emit-ir %s -Xcc -fno-asynchronous-unwind-tables -Xcc -fno-unwind-tables | %FileCheck %s --check-prefix=NONE
// RUN: %swift_frontend_plain -target aarch64-unknown-linux-gnu -parse-stdlib -module-name main -S %s | %FileCheck %s --check-prefix=ASM
// RUN: %swift_frontend_plain -target aarch64-unknown-linux-gnu -parse-stdlib -module-name main -enable-experimental-feature Embedded -wmo -emit-ir %s | %FileCheck %s --check-prefix=EMBEDDED

// REQUIRES: CODEGENERATOR=AArch64
// REQUIRES: swift_feature_Embedded

// Swift functions get Clang's unwind table kind. On AArch64, only
// uwtable(async) produces epilogue CFI.

@inline(never)
public func g() {}

public func f() {
  g()
  g()
}

// ASYNC: define {{.*}} @"$s4main1fyyF"() #[[ATTR:[0-9]+]]
// ASYNC: attributes #[[ATTR]] = { {{.*}}uwtable{{[^(]}}

// SYNC: define {{.*}} @"$s4main1fyyF"() #[[ATTR:[0-9]+]]
// SYNC: attributes #[[ATTR]] = { {{.*}}uwtable(sync)

// NONE-NOT: uwtable

// EMBEDDED: define {{.*}} @"$e4main1fyyF"() #[[ATTR:[0-9]+]]
// EMBEDDED: attributes #[[ATTR]] = {
// EMBEDDED-NOT: uwtable
// EMBEDDED-SAME: }

// ASM-LABEL: $s4main1fyyF:
// ASM: bl $s4main1gyyF
// ASM: bl $s4main1gyyF
// ASM: ldp x29, x30
// ASM: .cfi_restore w30
// ASM: ret
