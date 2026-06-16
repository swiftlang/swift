// REQUIRES: OS=linux

// RUN: %swift_frontend_plain -typecheck -target x86_64-unknown-linux-gnu -disable-objc-attr-requires-foundation-module -gnustep-objc-interop %s
// RUN: not %swift_frontend_plain -typecheck -target x86_64-unknown-linux-gnu -disable-objc-attr-requires-foundation-module %s 2>&1 | %FileCheck %s --check-prefix=OBJC-DISABLED
// RUN: not %swift_frontend_plain -typecheck -target wasm32-unknown-wasi -gnustep-objc-interop %s 2>&1 | %FileCheck %s --check-prefix=UNSUPPORTED-TARGET
// RUN: not %swift_frontend_plain -typecheck -target x86_64-unknown-linux-gnu -disable-objc-attr-requires-foundation-module -Xcc -### %s 2>&1 | %FileCheck %s --check-prefix=LINUX-CLANG
// RUN: not %swift_frontend_plain -typecheck -target x86_64-unknown-linux-gnu -disable-objc-attr-requires-foundation-module -gnustep-objc-interop -Xcc -### %s 2>&1 | %FileCheck %s --check-prefix=GNUSTEP-CLANG

// OBJC-DISABLED: error: Objective-C interoperability is disabled
// UNSUPPORTED-TARGET: error: unsupported option '-gnustep-objc-interop' for target 'wasm32-unknown-wasi'
// LINUX-CLANG: "-fobjc-runtime=ios-7.0"
// GNUSTEP-CLANG: "-fobjc-runtime=gnustep-2.0"

@objc protocol GNUstepObjCInteropProtocol {}
