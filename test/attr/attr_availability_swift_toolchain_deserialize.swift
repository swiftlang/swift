// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/SwiftToolchain.swiftmodule -module-name SwiftToolchain %S/Inputs/SwiftToolchain.swift
// RUN: %target-swift-frontend -typecheck -verify -I %t %s 2>&1
// RUN: %target-swift-ide-test -print-module -module-to-print SwiftToolchain -source-filename x -I %t | %FileCheck %S/Inputs/SwiftToolchain.swift

import SwiftToolchain

// This won't emit a diagnostic because _SwiftToolchain availability is always inactive.
let _ = fourOnly()

