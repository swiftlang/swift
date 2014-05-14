// RUN: rm -rf %t
// RUN: mkdir %t

// RUN: %swift -emit-dependencies-path - -parse %S/../Inputs/empty.swift | FileCheck -check-prefix=CHECK-BASIC %s

// CHECK-BASIC-LABEL: - :
// CHECK-BASIC: Inputs/empty.swift
// CHECK-BASIC: Swift.swiftmodule
// CHECK-BASIC: shims/shims.h
// CHECK-BASIC: shims/module.map
// CHECK-BASIC-NOT: :
// CHECK-BASIC-NOT: /

// RUN: %swift -emit-dependencies-path - -emit-module %S/../Inputs/empty.swift -o %t/empty.swiftmodule -emit-module-doc-path %t/empty.swiftdoc -emit-objc-header-path %t/empty.h | FileCheck -check-prefix=CHECK-MULTIPLE-OUTPUTS %s

// CHECK-MULTIPLE-OUTPUTS-LABEL: empty.swiftmodule :
// CHECK-MULTIPLE-OUTPUTS: Inputs/empty.swift
// CHECK-MULTIPLE-OUTPUTS: Swift.swiftmodule
// CHECK-MULTIPLE-OUTPUTS-LABEL: empty.swiftdoc :
// CHECK-MULTIPLE-OUTPUTS: Inputs/empty.swift
// CHECK-MULTIPLE-OUTPUTS: Swift.swiftmodule
// CHECK-MULTIPLE-OUTPUTS-LABEL: empty.h :
// CHECK-MULTIPLE-OUTPUTS: Inputs/empty.swift
// CHECK-MULTIPLE-OUTPUTS: Swift.swiftmodule
// CHECK-MULTIPLE-OUTPUTS-NOT: :

// RUN: %swift %clang-importer-sdk -I %S/Inputs/dependencies/ -import-objc-header extra-header.h -emit-dependencies-path - -parse %s | FileCheck -check-prefix=CHECK-IMPORT %s

// CHECK-IMPORT-LABEL: - :
// CHECK-IMPORT: dependencies.swift
// CHECK-IMPORT: Inputs/dependencies/extra-header.h
// CHECK-IMPORT: Swift.swiftmodule
// CHECK-IMPORT: shims/shims.h
// CHECK-IMPORT: shims/module.map
// CHECK-IMPORT: Foundation.swift
// CHECK-IMPORT: ObjectiveC.swift
// CHECK-IMPORT: CoreGraphics.swift
// CHECK-IMPORT: Inputs/dependencies/UserClangModule.h
// CHECK-IMPORT: Inputs/dependencies/module.modulemap
// CHECK-IMPORT-NOT: :

import Foundation
import UserClangModule

class Test: NSObject {}

_ = A()
_ = USER_VERSION
_ = EXTRA_VERSION
