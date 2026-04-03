// REQUIRES: OS=linux

// RUN: %swift_frontend_plain -typecheck -target x86_64-unknown-linux-gnu -disable-objc-attr-requires-foundation-module -gnustep-objc-interop %s
// RUN: not %swift_frontend_plain -typecheck -target x86_64-unknown-linux-gnu -disable-objc-attr-requires-foundation-module %s 2>&1 | %FileCheck %s --check-prefix=OBJC-DISABLED
// RUN: not %swift_frontend_plain -typecheck -target wasm32-unknown-wasi -gnustep-objc-interop %s 2>&1 | %FileCheck %s --check-prefix=UNSUPPORTED-TARGET

// OBJC-DISABLED: error: Objective-C interoperability is disabled
// UNSUPPORTED-TARGET: error: unsupported option '-gnustep-objc-interop' for target 'wasm32-unknown-wasi'

@objc protocol GNUstepObjCInteropProtocol {}
