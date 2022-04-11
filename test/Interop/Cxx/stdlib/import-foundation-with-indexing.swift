// RUN: %target-swift-frontend %s -c -index-system-modules -index-store-path %t -enable-experimental-cxx-interop
//
// REQUIRES: OS=macosx

import Foundation

func test(d: Date) {}