// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Prepare the module imported as package, and two middle modules.
// RUN: %target-swift-frontend -emit-module %t/PackageDep.swift -o %t
// RUN: %target-swift-frontend -emit-module %t/ResilientDep.swift -o %t \
// RUN:   -package-name MyPackage -I %t \
// RUN:   -enable-library-evolution \
// RUN:   -enable-experimental-feature AccessLevelOnImport
// RUN: %target-swift-frontend -emit-module %t/NonResilientDep.swift -o %t \
// RUN:   -package-name MyPackage -I %t \
// RUN:   -enable-experimental-feature AccessLevelOnImport

//--- PackageDep.swift

//--- ResilientDep.swift
package import PackageDep

//--- NonResilientDep.swift
package import PackageDep

/// When the middle module is library-evolution enabled, the package import
/// is only visible to modules from the same package.
// RUN: %target-swift-frontend -typecheck %t/ResilientClient.swift \
// RUN:   -package-name MyPackage -I %t \
// RUN:   -enable-experimental-feature AccessLevelOnImport \
// RUN:   -Rmodule-loading 2>&1 | %FileCheck -check-prefix=VISIBLE-PACKAGE-DEP %s
// VISIBLE-PACKAGE-DEP: source: '{{.*}}PackageDep.swiftmodule'

// RUN: %target-swift-frontend -typecheck %t/ResilientClient.swift \
// RUN:   -package-name NotMyPackage -I %t \
// RUN:   -enable-experimental-feature AccessLevelOnImport \
// RUN:   -Rmodule-loading 2>&1 | %FileCheck -check-prefix=HIDDEN-PACKAGE-DEP %s
// HIDDEN-PACKAGE-DEP-NOT: PackageDep

//--- ResilientClient.swift
import ResilientDep

/// When the middle module isn't library-evolution enabled, all clients
/// see the package dependency.
// RUN: %target-swift-frontend -typecheck %t/NonResilientClient.swift \
// RUN:   -package-name NotMyPackage -I %t \
// RUN:   -enable-experimental-feature AccessLevelOnImport \
// RUN:   -Rmodule-loading 2>&1 | %FileCheck -check-prefix=VISIBLE-PACKAGE-DEP %s

//--- NonResilientClient.swift
import NonResilientDep
