// RUN: %target-build-swift %s
// REQUIRES: executable_test

// FIXME: iOS does not have Cocoa.framework
// REQUIRES: OS=macosx

import Cocoa

// Make sure the ObjectiveC adapter module gets imported, including ObjCSel.
func rdar14759044(obj: NSObject) -> Bool {
  return obj.respondsToSelector("abc") // no-warning
}
