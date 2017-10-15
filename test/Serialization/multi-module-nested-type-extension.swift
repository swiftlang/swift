// RUN: %empty-directory(%t)

// Build the other module, which consists of a single source file.

// RUN: %target-swift-frontend -emit-module -module-name other -o %t/multi-module-nested-type-1.swiftmodule -primary-file %S/Inputs/multi-module-nested-type-1.swift
// RUN: %target-swift-frontend -emit-module -module-name other -o %t/other.swiftmodule %t/multi-module-nested-type-1.swiftmodule

// Build this module, which consists of two source files.

// RUN: %target-swift-frontend -emit-module -module-name me -o %t/multi-module-nested-type-2.swiftmodule -primary-file %S/Inputs/multi-module-nested-type-2.swift %s -I %t
// RUN: %target-swift-frontend -emit-module -module-name me -o %t/multi-module-nested-type-3.swiftmodule %S/Inputs/multi-module-nested-type-2.swift -primary-file %s -I %t

// RUN: %target-swift-frontend -emit-module -module-name me -o %t/me.swiftmodule %t/multi-module-nested-type-2.swiftmodule %t/multi-module-nested-type-3.swiftmodule -I %t

import other

extension X {
  func takesB(_: B) {}
}
