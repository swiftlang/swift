// RUN: %target-swift-frontend %s -emit-silgen -disable-availability-checking -import-objc-header %S/Inputs/rdar79383990.h
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import Foundation

func test(s: BackgroundActivityScheduler) async {
    _ = await s.schedule()
}
