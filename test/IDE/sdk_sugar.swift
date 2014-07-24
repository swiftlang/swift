// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %swift-ide-test -sdk=%sdk -module-cache-path %t/clang-module-cache -print-module -module-print-submodules -module-to-print=Foundation -function-definitions=false -source-filename %s > %t/out 2>&1
// RUN: FileCheck -input-file=%t/out %s
// REQUIRES: sdk
// REQUIRES: OS=macosx

// FIXME: rdar://17791048
// XFAIL: *

import Foundation

// CHECK: func loadAndReturnError(error: NSErrorPointer) -> Bool

