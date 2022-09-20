// RUN: %target-swift-frontend -disable-availability-checking -target arm64_32-apple-watchos7 %s -S | %FileCheck  -check-prefix=NEVER %s
// RUN: %target-swift-frontend -disable-availability-checking -target arm64_32-apple-watchos8 %s -S | %FileCheck  -check-prefix=ALWAYS %s
// RUN: %target-swift-frontend -disable-availability-checking -target arm64_32-apple-watchos7  -swift-async-frame-pointer=always %s -S | %FileCheck  -check-prefix=ALWAYS %s
// RUN: %target-swift-frontend -disable-availability-checking -target arm64_32-apple-watchos7  -swift-async-frame-pointer=never %s -S | %FileCheck  -check-prefix=NEVER %s
// RUN: %target-swift-frontend -disable-availability-checking -target arm64_32-apple-watchos7  -swift-async-frame-pointer=auto %s -S | %FileCheck  -check-prefix=AUTO %s

// REQUIRES: OS=watchos
// REQUIRES: CPU=armv7k || CPU=arm64_32
// REQUIRES: rdar97790231

public func someAsyncFunction() async {
}

// AUTO: swift_async_extendedFramePointerFlags

// ALWAYS-NOT: swift_async_extendedFramePointerFlags
// ALWAYS: 0x1000000000000000
// ALWAYS-NOT: swift_async_extendedFramePointerFlags

// NEVER-NOT: swift_async_extendedFramePointerFlags
// NEVER-NOT: 0x1000000000000000
