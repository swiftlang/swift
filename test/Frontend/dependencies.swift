// XFAIL: linux

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-dependencies-path - -resolve-imports %S/../Inputs/empty\ file.swift | %FileCheck -check-prefix=CHECK-BASIC %s
// RUN: %target-swift-frontend -emit-reference-dependencies-path - -typecheck -primary-file %S/../Inputs/empty\ file.swift | %FileCheck -check-prefix=CHECK-BASIC-YAML %s

// RUN: %target-swift-frontend -emit-dependencies-path %t.d -emit-reference-dependencies-path %t.swiftdeps -typecheck -primary-file %S/../Inputs/empty\ file.swift
// RUN: %FileCheck -check-prefix=CHECK-BASIC %s < %t.d
// RUN: %FileCheck -check-prefix=CHECK-BASIC-YAML %s < %t.swiftdeps

// CHECK-BASIC-LABEL: - :
// CHECK-BASIC: Inputs/empty\ file.swift
// CHECK-BASIC: Swift.swiftmodule
// CHECK-BASIC-NOT: :

// CHECK-BASIC-YAML-LABEL: depends-external:
// CHECK-BASIC-YAML-NOT: empty\ file.swift
// CHECK-BASIC-YAML: "{{.*}}/Swift.swiftmodule"
// CHECK-BASIC-YAML-NOT: {{:$}}


// RUN: %target-swift-frontend -emit-dependencies-path %t.d -emit-reference-dependencies-path %t.swiftdeps -typecheck %S/../Inputs/empty\ file.swift 2>&1 | %FileCheck -check-prefix=NO-PRIMARY-FILE %s

// NO-PRIMARY-FILE: warning: ignoring -emit-reference-dependencies (requires -primary-file)


// RUN: %target-swift-frontend -emit-dependencies-path - -emit-module %S/../Inputs/empty\ file.swift -o %t/empty\ file.swiftmodule -emit-module-doc-path %t/empty\ file.swiftdoc -emit-objc-header-path %t/empty\ file.h -emit-parseable-module-interface-path %t/empty\ file.swiftinterface | %FileCheck -check-prefix=CHECK-MULTIPLE-OUTPUTS %s

// CHECK-MULTIPLE-OUTPUTS-LABEL: empty\ file.swiftmodule :
// CHECK-MULTIPLE-OUTPUTS: Inputs/empty\ file.swift
// CHECK-MULTIPLE-OUTPUTS: Swift.swiftmodule
// CHECK-MULTIPLE-OUTPUTS-LABEL: empty\ file.swiftdoc :
// CHECK-MULTIPLE-OUTPUTS: Inputs/empty\ file.swift
// CHECK-MULTIPLE-OUTPUTS: Swift.swiftmodule
// CHECK-MULTIPLE-OUTPUTS-LABEL: empty\ file.swiftinterface :
// CHECK-MULTIPLE-OUTPUTS: Inputs/empty\ file.swift
// CHECK-MULTIPLE-OUTPUTS: Swift.swiftmodule
// CHECK-MULTIPLE-OUTPUTS-LABEL: empty\ file.h :
// CHECK-MULTIPLE-OUTPUTS: Inputs/empty\ file.swift
// CHECK-MULTIPLE-OUTPUTS: Swift.swiftmodule
// CHECK-MULTIPLE-OUTPUTS-NOT: :

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -import-objc-header %S/Inputs/dependencies/extra-header.h -emit-dependencies-path - -resolve-imports %s | %FileCheck -check-prefix=CHECK-IMPORT %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -import-objc-header %S/Inputs/dependencies/extra-header.h -track-system-dependencies -emit-dependencies-path - -resolve-imports %s | %FileCheck -check-prefix=CHECK-IMPORT-TRACK-SYSTEM %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -import-objc-header %S/Inputs/dependencies/extra-header.h -emit-reference-dependencies-path - -typecheck -primary-file %s | %FileCheck -check-prefix=CHECK-IMPORT-YAML %s

// CHECK-IMPORT-LABEL: - :
// CHECK-IMPORT: dependencies.swift
// CHECK-IMPORT-DAG: Swift.swiftmodule
// CHECK-IMPORT-DAG: Inputs/dependencies/$$$$$$$$$$.h
// CHECK-IMPORT-DAG: Inputs/dependencies/UserClangModule.h
// CHECK-IMPORT-DAG: Inputs/dependencies/extra-header.h
// CHECK-IMPORT-DAG: Inputs/dependencies/module.modulemap
// CHECK-IMPORT-DAG: ObjectiveC.swift
// CHECK-IMPORT-DAG: Foundation.swift
// CHECK-IMPORT-DAG: CoreGraphics.swift
// CHECK-IMPORT-NOT: :

// CHECK-IMPORT-TRACK-SYSTEM-LABEL: - :
// CHECK-IMPORT-TRACK-SYSTEM: dependencies.swift
// CHECK-IMPORT-TRACK-SYSTEM-DAG: Swift.swiftmodule
// CHECK-IMPORT-TRACK-SYSTEM-DAG: SwiftOnoneSupport.swiftmodule
// CHECK-IMPORT-TRACK-SYSTEM-DAG: CoreFoundation.swift
// CHECK-IMPORT-TRACK-SYSTEM-DAG: CoreGraphics.swift
// CHECK-IMPORT-TRACK-SYSTEM-DAG: Foundation.swift
// CHECK-IMPORT-TRACK-SYSTEM-DAG: ObjectiveC.swift
// CHECK-IMPORT-TRACK-SYSTEM-DAG: Inputs/dependencies/$$$$$$$$$$.h
// CHECK-IMPORT-TRACK-SYSTEM-DAG: Inputs/dependencies/UserClangModule.h
// CHECK-IMPORT-TRACK-SYSTEM-DAG: Inputs/dependencies/extra-header.h
// CHECK-IMPORT-TRACK-SYSTEM-DAG: Inputs/dependencies/module.modulemap
// CHECK-IMPORT-TRACK-SYSTEM-DAG: swift/shims/module.modulemap
// CHECK-IMPORT-TRACK-SYSTEM-DAG: usr/include/CoreFoundation.h
// CHECK-IMPORT-TRACK-SYSTEM-DAG: usr/include/CoreGraphics.apinotes
// CHECK-IMPORT-TRACK-SYSTEM-DAG: usr/include/CoreGraphics.h
// CHECK-IMPORT-TRACK-SYSTEM-DAG: usr/include/Foundation.h
// CHECK-IMPORT-TRACK-SYSTEM-DAG: usr/include/objc/NSObject.h
// CHECK-IMPORT-TRACK-SYSTEM-DAG: usr/include/objc/ObjectiveC.apinotes
// CHECK-IMPORT-TRACK-SYSTEM-DAG: usr/include/objc/module.map
// CHECK-IMPORT-TRACK-SYSTEM-DAG: usr/include/objc/objc.h
// CHECK-IMPORT-TRACK-SYSTEM-NOT: :

// CHECK-IMPORT-YAML-LABEL: depends-external:
// CHECK-IMPORT-YAML-NOT: dependencies.swift
// CHECK-IMPORT-YAML-DAG: "{{.*}}/Swift.swiftmodule"
// CHECK-IMPORT-YAML-DAG: "{{.*}}Inputs/dependencies/$$$$$.h"
// CHECK-IMPORT-YAML-DAG: "{{.*}}Inputs/dependencies/UserClangModule.h"
// CHECK-IMPORT-YAML-DAG: "{{.*}}Inputs/dependencies/extra-header.h"
// CHECK-IMPORT-YAML-DAG: "{{.*}}Inputs/dependencies/module.modulemap"
// CHECK-IMPORT-YAML-DAG: "{{.*}}/ObjectiveC.swift"
// CHECK-IMPORT-YAML-DAG: "{{.*}}/Foundation.swift"
// CHECK-IMPORT-YAML-DAG: "{{.*}}/CoreGraphics.swift"
// CHECK-IMPORT-YAML-NOT: {{^-}}
// CHECK-IMPORT-YAML-NOT: {{:$}}

// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -DERROR -import-objc-header %S/Inputs/dependencies/extra-header.h -emit-dependencies-path - -typecheck %s | %FileCheck -check-prefix=CHECK-IMPORT %s
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -DERROR -import-objc-header %S/Inputs/dependencies/extra-header.h -emit-reference-dependencies-path - -typecheck -primary-file %s | %FileCheck -check-prefix=CHECK-IMPORT-YAML %s


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
