// REQUIRES: objc_interop
// RUN: %empty-directory(%t) && not %swift -c -update-code -primary-file %s -emit-migrated-file-path %t/api-special-cases.swift.result -emit-remap-file-path %t/api-special-cases.swift.remap -o /dev/null

// A failed import due to a missing -sdk frontend argument produces a
// diagnostic with an invalid sourceloc.
import CoreGraphics
