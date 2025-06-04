// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/SwiftToolchain.swiftmodule -module-name SwiftToolchain %S/Inputs/SwiftToolchain.swift -enable-experimental-feature SwiftToolchainAvailability
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature SwiftToolchainAvailability -I %t %s 2>&1
// RUN: %target-swift-ide-test -print-module -module-to-print SwiftToolchain -enable-experimental-feature SwiftToolchainAvailability -source-filename x -I %t | %FileCheck %S/Inputs/SwiftToolchain.swift

import SwiftToolchain

// This won't emit a diagnostic because _SwiftToolchain availability is always inactive.
let _ = fourOnly()

