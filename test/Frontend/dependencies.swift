// RUN: rm -rf %t
// RUN: mkdir %t

// RUN: %swift -emit-dependencies-path - -parse %S/../Inputs/empty.swift | FileCheck -check-prefix=CHECK-BASIC %s

// CHECK-BASIC-LABEL: - :
// CHECK-BASIC: Inputs/empty.swift
// CHECK-BASIC: Swift.swiftmodule
// CHECK-BASIC-DAG: shims/shims.h
// CHECK-BASIC-DAG: shims/module.map
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
// CHECK-IMPORT-DAG: Inputs/dependencies/extra-header.h
// CHECK-IMPORT-DAG: Swift.swiftmodule
// CHECK-IMPORT-DAG: shims/shims.h
// CHECK-IMPORT-DAG: shims/module.map
// CHECK-IMPORT-DAG: Foundation.swift
// CHECK-IMPORT-DAG: ObjectiveC.swift
// CHECK-IMPORT-DAG: CoreGraphics.swift
// CHECK-IMPORT-DAG: Inputs/dependencies/UserClangModule.h
// CHECK-IMPORT-DAG: Inputs/dependencies/module.modulemap
// CHECK-IMPORT-NOT: :

import Foundation
import UserClangModule

class Test: NSObject {}

_ = A()
_ = USER_VERSION
_ = EXTRA_VERSION
