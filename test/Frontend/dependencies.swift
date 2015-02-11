// RUN: rm -rf %t && mkdir %t

// RUN: %target-swift-frontend -emit-dependencies-path - -parse %S/../Inputs/empty.swift | FileCheck -check-prefix=CHECK-BASIC %s
// RUN: %target-swift-frontend -emit-reference-dependencies-path - -parse -primary-file %S/../Inputs/empty.swift | FileCheck -check-prefix=CHECK-BASIC-YAML %s

// RUN: %target-swift-frontend -emit-dependencies-path %t.d -emit-reference-dependencies-path %t.swiftdeps -parse -primary-file %S/../Inputs/empty.swift
// FileCheck -check-prefix=CHECK-BASIC < %t.d
// FileCheck -check-prefix=CHECK-BASIC-YAML < %t.swiftdeps

// XFAIL: linux

// CHECK-BASIC-LABEL: - :
// CHECK-BASIC: Inputs/empty.swift
// CHECK-BASIC: Swift.swiftmodule
// CHECK-BASIC-NOT: :

// CHECK-BASIC-YAML-LABEL: cross-module:
// CHECK-BASIC-YAML-NOT: empty.swift
// CHECK-BASIC-YAML: "{{.*}}/Swift.swiftmodule"
// CHECK-BASIC-YAML-NOT: {{:$}}


// RUN: %target-swift-frontend -emit-dependencies-path - -emit-module %S/../Inputs/empty.swift -o %t/empty.swiftmodule -emit-module-doc-path %t/empty.swiftdoc -emit-objc-header-path %t/empty.h | FileCheck -check-prefix=CHECK-MULTIPLE-OUTPUTS %s

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

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -import-objc-header %S/Inputs/dependencies/extra-header.h -emit-dependencies-path - -parse %s | FileCheck -check-prefix=CHECK-IMPORT %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -import-objc-header %S/Inputs/dependencies/extra-header.h -emit-reference-dependencies-path - -parse -primary-file %s | FileCheck -check-prefix=CHECK-IMPORT-YAML %s

// CHECK-IMPORT-LABEL: - :
// CHECK-IMPORT: dependencies.swift
// CHECK-IMPORT-DAG: Inputs/dependencies/$$$$$$$$$$.h
// CHECK-IMPORT-DAG: Inputs/dependencies/extra-header.h
// CHECK-IMPORT-DAG: Swift.swiftmodule
// CHECK-IMPORT-DAG: Foundation.swift
// CHECK-IMPORT-DAG: ObjectiveC.swift
// CHECK-IMPORT-DAG: CoreGraphics.swift
// CHECK-IMPORT-DAG: Inputs/dependencies/UserClangModule.h
// CHECK-IMPORT-DAG: Inputs/dependencies/module.modulemap
// CHECK-IMPORT-NOT: :

// CHECK-IMPORT-YAML-LABEL: cross-module:
// CHECK-IMPORT-YAML-NOT: dependencies.swift
// CHECK-IMPORT-YAML-DAG: "{{.*}}Inputs/dependencies/$$$$$.h"
// CHECK-IMPORT-YAML-DAG: "{{.*}}Inputs/dependencies/extra-header.h"
// CHECK-IMPORT-YAML-DAG: "{{.*}}/Swift.swiftmodule"
// CHECK-IMPORT-YAML-DAG: "{{.*}}/Foundation.swift"
// CHECK-IMPORT-YAML-DAG: "{{.*}}/ObjectiveC.swift"
// CHECK-IMPORT-YAML-DAG: "{{.*}}/CoreGraphics.swift"
// CHECK-IMPORT-YAML-DAG: "{{.*}}Inputs/dependencies/UserClangModule.h"
// CHECK-IMPORT-YAML-DAG: "{{.*}}Inputs/dependencies/module.modulemap"
// CHECK-IMPORT-YAML-NOT: {{^-}}
// CHECK-IMPORT-YAML-NOT: {{:$}}

// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -DERROR -import-objc-header %S/Inputs/dependencies/extra-header.h -emit-dependencies-path - -parse %s | FileCheck -check-prefix=CHECK-IMPORT %s
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -DERROR -import-objc-header %S/Inputs/dependencies/extra-header.h -emit-reference-dependencies-path - -parse -primary-file %s | FileCheck -check-prefix=CHECK-IMPORT-YAML %s


import Foundation
import UserClangModule

class Test: NSObject {}

_ = A()
_ = USER_VERSION
_ = EXTRA_VERSION
_ = MONEY

#if ERROR
_ = someRandomUndefinedName
#endif
