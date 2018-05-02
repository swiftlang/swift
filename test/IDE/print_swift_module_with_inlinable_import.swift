// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -o %t/InlinableImport.swiftmodule %S/Inputs/InlinableImport.swift
// RUN: %target-swift-frontend -I %t -emit-module -o %t/HasInlinableImport.swiftmodule %S/Inputs/HasInlinableImport.swift
// RUN: %target-swift-ide-test -I %t -print-module -source-filename %s -module-to-print=HasInlinableImport -function-definitions=false | %FileCheck %s

// REQUIRES: objc_interop

import HasInlinableImport

// For now, don't print anything special for inlinable imports.

// CHECK: {{^}}import InlinableImport
