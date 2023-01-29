// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -c -index-system-modules -index-store-path %t -enable-experimental-cxx-interop
// RUN: ls %t/v5/units | %FileCheck %s

// REQUIRES: OS=macosx

import Foundation

func test(d: Date) {}

// CHECK: Foundation-{{.*}}.pcm
