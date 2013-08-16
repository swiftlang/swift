// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%sdk -i %s | FileCheck %s
// REQUIRES: sdk

import Cocoa

// Make sure the ObjectiveC adapter module gets imported, including ObjCSel.
func rdar14759044(button : NSButton) -> Bool {
  return button.action() == "abc" // no-warning
}
