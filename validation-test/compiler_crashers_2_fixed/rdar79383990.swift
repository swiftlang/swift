// RUN: %target-swift-frontend %s -emit-silgen -disable-availability-checking
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import Foundation

func test(s: NSBackgroundActivityScheduler) async {
    _ = await s.schedule()
}
