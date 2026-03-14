// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -target x86_64-apple-macosx11 %s -S | %FileCheck  -check-prefix=AUTO %s
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -target x86_64-apple-macosx12 %s -S | %FileCheck  -check-prefix=ALWAYS %s
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -swift-async-frame-pointer=auto -target x86_64-apple-macosx11 %s -S | %FileCheck  -check-prefix=AUTO %s
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -swift-async-frame-pointer=auto -target x86_64-apple-macosx12 %s -S | %FileCheck  -check-prefix=ALWAYS %s
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -swift-async-frame-pointer=never -target x86_64-apple-macosx11 %s -S | %FileCheck  -check-prefix=NEVER %s
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -swift-async-frame-pointer=never -target x86_64-apple-macosx12 %s -S | %FileCheck  -check-prefix=NEVER %s
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -swift-async-frame-pointer=always -target x86_64-apple-macosx11 %s -S | %FileCheck  -check-prefix=ALWAYS %s
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -swift-async-frame-pointer=always -target x86_64-apple-macosx12 %s -S | %FileCheck  -check-prefix=ALWAYS %s
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -swift-async-frame-pointer=never -target arm64-apple-macos12 %s -S | %FileCheck -check-prefix=NEVER-ARM64 %s
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -swift-async-frame-pointer=always -target arm64-apple-macos12 %s -S | %FileCheck -check-prefix=ALWAYS-ARM64 %s

// REQUIRES: OS=macosx
// REQUIRES: CODEGENERATOR=X86 && CODEGENERATOR=AArch64

@_silgen_name("opaque_function")
func blarpy()

public func someAsyncFunction() async {
    blarpy()
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

// NEVER-ARM64: s31swift_async_extended_frame_info17someAsyncFunctionyyYaFTY0
// NEVER-ARM64-NOT: swift_async_extendedFramePointerFlags
// NEVER-ARM64-NOT: #0x1000000000000000

// ALWAYS-ARM64: s31swift_async_extended_frame_info17someAsyncFunctionyyYaFTY0
// ALWAYS-ARM64-NOT: swift_async_extendedFramePointerFlags
// ALWAYS-ARM64: #0x1000000000000000
