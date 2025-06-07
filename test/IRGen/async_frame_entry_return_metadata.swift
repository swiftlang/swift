// RUN: %target-swift-frontend -primary-file %s -emit-ir  -module-name async -target %target-swift-5.1-abi-triple -enable-async-frame-push-pop-metadata | %FileCheck %s --check-prefix=ENABLED
// RUN: %target-swift-frontend -primary-file %s -emit-ir  -module-name async -target %target-swift-5.1-abi-triple -O -enable-async-frame-push-pop-metadata | %FileCheck %s --check-prefix=ENABLED
// RUN: %target-swift-frontend -primary-file %s -emit-ir  -module-name async -target %target-swift-5.1-abi-triple -disable-async-frame-push-pop-metadata | %FileCheck %s --check-prefix=DISABLED
// RUN: %target-swift-frontend -primary-file %s -emit-ir  -module-name async -target %target-swift-5.1-abi-triple | %FileCheck %s --check-prefix=ENABLED

// REQUIRES: OS=macosx || OS=iphoneos
// REQUIRES: PTRSIZE=64

// ENABLED: @__swift_async_entry_functlets = internal constant [2 x i32] [i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s5async6calleeyyYaF" to i64), i64 ptrtoint (ptr @__swift_async_entry_functlets to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s5async6callerySiSbYaF" to i64), i64 ptrtoint (ptr getelementptr inbounds ([2 x i32], ptr @__swift_async_entry_functlets, i32 0, i32 1) to i64)) to i32)], section "__TEXT,__swift_as_entry, coalesced, no_dead_strip", no_sanitize_address, align 4
// ENABLED: @__swift_async_ret_functlets = internal constant [1 x i32] [i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s5async6callerySiSbYaFTQ1_" to i64), i64 ptrtoint (ptr @__swift_async_ret_functlets to i64)) to i32)], section "__TEXT,__swift_as_ret, coalesced, no_dead_strip", no_sanitize_address, align 4

// ENABLED: define{{.*}} swifttailcc void @"$s5async6callerySiSbYaF"{{.*}} [[CALLER_FUNCLET_ATTRS:#[0-9]+]]
// ENABLED: define{{.*}} internal swifttailcc void @"$s5async6callerySiSbYaFTY0_"{{.*}} [[CALLER_FUNCLET_ATTRS2:#[0-9]+]]

// ENABLED: attributes [[CALLER_FUNCLET_ATTRS2]] = { {{.*}}noinline
// ENABLED: attributes [[CALLER_FUNCLET_ATTRS]] = { {{.*}}noinline

// DISABLED-NOT: @__swift_async_entry_functlets
// DISABLED-NOT: @__swift_async_ret_functlets
// DISABLED-NOT: s5async6calleeyyYaF.0
// DISABLED-NOT: s5async6callerySiSbYaF.0

@inline(never)
public func plusOne() {
    print("+1")
}

@inline(never)
public func minusOne() {
}

@inline(never)
public func callee() async {
  print("callee")
}

public func caller(_ b: Bool) async -> Int {
  plusOne()

  if b {
      await callee()
  }

  minusOne()
  return 1
}
