/// Test the logic printing the export_as name in public swiftinterfaces
/// and the real source module name in the private swiftinterfaces.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Build lib with an export_as.
// RUN: %target-swift-frontend -emit-module %t/Exported.swift \
// RUN:   -module-name Exported -swift-version 5 -I %t \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module-path %t/Exported.swiftmodule \
// RUN:   -emit-module-interface-path %t/Exported.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Exported.private.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/Exported.private.swiftinterface) -module-name Exported -I %t
// RUN: cat %t/Exported.swiftinterface | %FileCheck -check-prefix=CHECK-USE-EXPORTER %s
// RUN: cat %t/Exported.private.swiftinterface | %FileCheck -check-prefix=CHECK-USE-EXPORTED %s

/// The public swiftinterface only builds under the name of Exporter.
// RUN: sed -e "s/module-name Exported/module-name Exporter/" -ibk %t/Exported.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/Exported.swiftinterface) -I %t -module-name Exporter

/// Build lib with an @exported import of the exported one.
// RUN: %target-swift-frontend -emit-module %t/Exporter.swift \
// RUN:   -module-name Exporter -swift-version 5 -I %t \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module-path %t/Exporter.swiftmodule \
// RUN:   -emit-module-interface-path %t/Exporter.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Exporter.private.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/Exporter.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/Exporter.private.swiftinterface) -module-name Exporter -I %t
// RUN: cat %t/Exporter.swiftinterface | %FileCheck -check-prefix=CHECK-USE-EXPORTER %s
// RUN: cat %t/Exporter.private.swiftinterface | %FileCheck -check-prefix=CHECK-USE-EXPORTED %s

/// Build lib with an @exported import of the exported one.
// RUN: %target-swift-frontend -emit-module %t/Exporter.swift \
// RUN:   -module-name Exporter -swift-version 5 -I %t \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module-path %t/Exporter.swiftmodule \
// RUN:   -emit-module-interface-path %t/Exporter.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Exporter.private.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/Exporter.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/Exporter.private.swiftinterface) -module-name Exporter -I %t
// RUN: cat %t/Exporter.swiftinterface | %FileCheck -check-prefix=CHECK-USE-EXPORTER %s
// RUN: cat %t/Exporter.private.swiftinterface | %FileCheck -check-prefix=CHECK-USE-EXPORTED %s

/// Build a client of the exporter lib.
// RUN: %target-swift-frontend -emit-module %t/Client.swift \
// RUN:   -module-name Client -swift-version 5 -I %t \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module-path %t/Client.swiftmodule \
// RUN:   -emit-module-interface-path %t/Client.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Client.private.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/Client.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/Client.private.swiftinterface) -module-name Client -I %t
// RUN: cat %t/Client.swiftinterface | %FileCheck -check-prefix=CHECK-USE-EXPORTER %s
// RUN: cat %t/Client.private.swiftinterface | %FileCheck -check-prefix=CHECK-USE-EXPORTED %s

/// Build a client of the exporter lib against the public swiftinterface.
// RUN: rm %t/Exporter.private.swiftinterface %t/Exporter.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/Client.swift \
// RUN:   -module-name Client -swift-version 5 -I %t \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module-path %t/Client.swiftmodule \
// RUN:   -emit-module-interface-path %t/Client.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Client.private.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/Client.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/Client.private.swiftinterface) -module-name Client -I %t
// RUN: cat %t/Client.swiftinterface | %FileCheck -check-prefix=CHECK-USE-EXPORTER %s
// RUN: cat %t/Client.private.swiftinterface | %FileCheck -check-prefix=CHECK-USE-EXPORTED %s

//--- module.modulemap
module Exported {
    export_as Exporter
    header "Exported.h"
}

//--- Exported.h
struct exportedClangType {};

//--- Exported.swift
@_exported import Exported

public func foo(a: exportedClangType) {}
// CHECK-USE-EXPORTED: Exported.exportedClangType
// CHECK-USE-EXPORTER: Exporter.exportedClangType

//--- Exporter.swift
@_exported import Exported

public func foo(a: exportedClangType) {}

//--- Client.swift
import Exporter

public func foo(a: exportedClangType) {}
