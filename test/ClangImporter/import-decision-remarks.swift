// Positive checks for all expected diagnostics. The actual expectations are in
// Foundation.h, not here.
//
// TODO: Actually add all of the expectations to Foundation.h.
// RUN: %target-typecheck-verify-swift %clang-importer-sdk -verify-additional-file %clang-importer-sdk-path/usr/include/Foundation.h -Robjc-imports Foundation

import Foundation
import ctypes
import enums_using_attributes

// The %t.txt file should contain failures (but not successes) in Foundation,
// successes and failures in user_objc, and nothing in enums_using_attributes.
//
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -Robjc-import-failures Foundation -Robjc-imports ctypes 2>%t.txt
// RUN: %FileCheck --check-prefix POSITIVE %s <%t.txt
// RUN: %FileCheck --check-prefix NEGATIVE %s <%t.txt

// POSITIVE-DAG: ctypes.h:{{[0-9]+:[0-9]+}}: remark: could not import
// POSITIVE-DAG: ctypes.h:{{[0-9]+:[0-9]+}}: remark: {{.* (also )?}}imported as
// POSITIVE-DAG: Foundation.h:{{[0-9]+:[0-9]+}}: remark: could not import

// NEGATIVE-NOT: Foundation.h:{{[0-9]+:[0-9]+}}: remark: {{.* (also )?}}imported as
// NEGATIVE-NOT: enums_using_attributes.h:{{[0-9]+:[0-9]+}}: remark: {{.* (also )?}}imported as
// NEGATIVE-NOT: enums_using_attributes.h:{{[0-9]+:[0-9]+}}: remark: could not import

// Not mentioned in any of the flags, but imported by Foundation.
// NEGATIVE-NOT: CoreGraphics.h:{{[0-9]+:[0-9]+}}: remark: {{.* (also )?}}imported as
// NEGATIVE-NOT: CoreGraphics.h:{{[0-9]+:[0-9]+}}: remark: could not import

// TODO: Test -Robjc-imports-natural-only
// TODO: Test specifying a submodule
