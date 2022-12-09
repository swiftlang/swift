// REQUIRES: objc_interop, concurrency

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-indexed-symbols -source-filename %s | %FileCheck %s

import AppKit

@MainActor
class AppDelegate {
  let window = NSWindow()
  // CHECK: [[@LINE-1]]:16 | class/Swift | NSWindow | c:objc(cs)NSWindow | Ref,RelCont
  // CHECK: [[@LINE-2]]:16 | constructor/Swift | init() | c:objc(cs)NSObject(im)init | Ref,Call,RelCont
}
