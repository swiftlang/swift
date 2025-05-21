// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/SwiftToolchain.swiftmodule -module-name SwiftToolchain %S/Inputs/SwiftToolchain.swift
// RUN: not %target-swift-frontend -typecheck -I %t %s 2>&1 | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print SwiftToolchain -source-filename x -I %t | %FileCheck %S/Inputs/SwiftToolchain.swift

import SwiftToolchain

// CHECK: 'fourOnly()' is unavailable
// CHECK: 'fourOnly()' was obsoleted in Swift Toolchain 5.0
let _ = fourOnly()

