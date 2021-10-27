// RUN: %target-swift-frontend -disable-availability-checking -target arm64_32-apple-watchos7 %s -S | %FileCheck  -check-prefix=ALWAYS %s
// RUN: %target-swift-frontend -disable-availability-checking -target arm64_32-apple-watchos8 %s -S | %FileCheck  -check-prefix=ALWAYS %s
// RUN: %target-swift-frontend -disable-availability-checking -target arm64_32-apple-watchos7  -swift-async-frame-pointer=auto %s -S | %FileCheck  -check-prefix=ALWAYS %s
// RUN: %target-swift-frontend -disable-availability-checking -target arm64_32-apple-watchos7  -swift-async-frame-pointer=never %s -S | %FileCheck  -check-prefix=NEVER %s

// REQUIRES: OS=watchos

public func someAsyncFunction() async {
}

// ALWAYS-NOT: swift_async_extendedFramePointerFlags
// NEVER-NOT: swift_async_extendedFramePointerFlags
