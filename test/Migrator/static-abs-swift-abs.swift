// REQUIRES: objc_interop
// RUN: %target-swift-frontend -typecheck -swift-version 3 %s
// RUN: rm -rf %t && mkdir -p %t && %target-swift-frontend -c -update-code -primary-file %s -emit-migrated-file-path %t/static-abs-swift-abs.swift.result -emit-remap-file-path %t/static-abs-swift-abs.swift.remap -o /dev/null
// RUN: diff -u %S/static-abs-swift-abs.swift.expected %t/static-abs-swift-abs.swift.result
// RUN: %target-swift-frontend -typecheck -swift-version 4 %t/static-abs-swift-abs.swift.result

import CoreGraphics

_ = CGFloat.abs(0)
_ = Float.abs(0)
_ = Double.abs(0)
