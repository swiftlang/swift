/// Don't crash when there's no NSObject protocol.
/// rdar://problem/34597302

// RUN: %target-swift-frontend -typecheck %s -import-objc-header %S/Inputs/no-nsobject-protocol.h -enable-objc-interop

var a = Bar()!
var b = a.barFunc()!;
b.nsobjectFunc()
b.fooFunc()
