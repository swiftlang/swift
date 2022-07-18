// RUN: %target-swift-frontend -disable-availability-checking -target x86_64-apple-macosx11 %s -S | %FileCheck  -check-prefix=AUTO %s
// RUN: %target-swift-frontend -disable-availability-checking -target x86_64-apple-macosx12 %s -S | %FileCheck  -check-prefix=ALWAYS %s
// RUN: %target-swift-frontend -disable-availability-checking -swift-async-frame-pointer=auto -target x86_64-apple-macosx11 %s -S | %FileCheck  -check-prefix=AUTO %s
// RUN: %target-swift-frontend -disable-availability-checking -swift-async-frame-pointer=auto -target x86_64-apple-macosx12 %s -S | %FileCheck  -check-prefix=ALWAYS %s
// RUN: %target-swift-frontend -disable-availability-checking -swift-async-frame-pointer=never -target x86_64-apple-macosx11 %s -S | %FileCheck  -check-prefix=NEVER %s
// RUN: %target-swift-frontend -disable-availability-checking -swift-async-frame-pointer=never -target x86_64-apple-macosx12 %s -S | %FileCheck  -check-prefix=NEVER %s
// RUN: %target-swift-frontend -disable-availability-checking -swift-async-frame-pointer=always -target x86_64-apple-macosx11 %s -S | %FileCheck  -check-prefix=ALWAYS %s
// RUN: %target-swift-frontend -disable-availability-checking -swift-async-frame-pointer=always -target x86_64-apple-macosx12 %s -S | %FileCheck  -check-prefix=ALWAYS %s

// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64

public func someAsyncFunction() async {
}

// AUTO: s31swift_async_extended_frame_info17someAsyncFunctionyyYaF:
// AUTO:	_swift_async_extendedFramePointerFlags

// ALWAYS: s31swift_async_extended_frame_info17someAsyncFunctionyyYaF:
// ALWAYS: btsq	$60

// NEVER: s31swift_async_extended_frame_info17someAsyncFunctionyyYaF:
// NEVER-NOT:	_swift_async_extendedFramePointerFlags
// NEVER-NOT: btsq	$60

// AUTO: .weak_reference _swift_async_extendedFramePointerFlags
// NEVER-NOT: .weak_reference _swift_async_extendedFramePointerFlags
// ALWAYS-NOT: .weak_reference _swift_async_extendedFramePointerFlags
