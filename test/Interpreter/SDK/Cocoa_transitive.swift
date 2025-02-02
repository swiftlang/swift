// RUN: %empty-directory(%t/cache)
// RUN: %target-build-swift %s -module-cache-path %t/cache
// REQUIRES: executable_test

// FIXME: iOS does not have Cocoa.framework
// REQUIRES: OS=macosx

import Cocoa

// Make sure the ObjectiveC overlay gets imported, including ObjCSel.
func rdar14759044(obj: NSObject) -> Bool {
  return obj.responds(to: "abc") // no-warning
}
