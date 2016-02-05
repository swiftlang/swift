// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

//
// Test OS version parsing
//

// CHECK: (10, 0, 0)
print(_parseDottedVersionTriple("10"))

// CHECK: (10, 9, 0)
print(_parseDottedVersionTriple("10.9"))

// CHECK: (10, 9, 3)
print(_parseDottedVersionTriple("10.9.3"))

