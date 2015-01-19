// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-ide-test -print-module -module-print-submodules -module-to-print=Foundation -function-definitions=false -source-filename %s > %t/out 2>&1
// RUN: FileCheck -input-file=%t/out %s
// REQUIRES: sdk

import Foundation

// CHECK: func loadAndReturnError(error: NSErrorPointer) -> Bool

