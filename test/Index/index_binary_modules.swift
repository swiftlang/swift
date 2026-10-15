// Binary frameworks ship a .swiftinterface and are never built from source by
// their clients, so without index data for them index-based navigation cannot
// find their declarations. -index-binary-modules indexes modules built from a
// textual interface outside the SDK, and records them as non-user (system)
// modules like SDK modules. Modules compiled from source are still left to be
// indexed by their own build.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/SDK)
// RUN: split-file %s %t

/// A binary framework outside the SDK: only the textual interface is shipped,
/// as in an xcframework.
// RUN: mkdir -p %t/Frameworks/BinaryFramework.framework/Modules/BinaryFramework.swiftmodule
// RUN: %target-swift-frontend -emit-module -module-name BinaryFramework \
// RUN:     -swift-version 5 -enable-library-evolution -parse-stdlib \
// RUN:     -o %t/BinaryFramework.swiftmodule \
// RUN:     -emit-module-interface-path %t/Frameworks/BinaryFramework.framework/Modules/BinaryFramework.swiftmodule/%module-target-triple.swiftinterface \
// RUN:     %t/BinaryFramework.swift

/// A module compiled from source by the client's own build: binary module only.
// RUN: mkdir -p %t/Modules
// RUN: %target-swift-frontend -emit-module -module-name SourceModule \
// RUN:     -swift-version 5 -parse-stdlib \
// RUN:     -o %t/Modules/SourceModule.swiftmodule \
// RUN:     %t/SourceModule.swift

/// By default neither module is indexed.
// RUN: %target-swift-frontend -typecheck -parse-stdlib -swift-version 5 \
// RUN:     -index-system-modules \
// RUN:     -index-store-path %t/idx-default \
// RUN:     -index-ignore-stdlib \
// RUN:     -sdk %t/SDK \
// RUN:     -F %t/Frameworks -I %t/Modules \
// RUN:     -module-cache-path %t/modulecache \
// RUN:     %t/Client.swift
// RUN: c-index-test core -print-unit %t/idx-default | %FileCheck -check-prefix=DEFAULT %s
/// The client only records the dependencies, without a unit to link to.
// DEFAULT-DAG: Unit | user | BinaryFramework | {{.*}}.swiftinterface{{$}}
// DEFAULT-DAG: Unit | user | SourceModule | {{.*}}SourceModule.swiftmodule{{$}}
// DEFAULT-NOT: module-name: BinaryFramework
// DEFAULT-NOT: module-name: SourceModule

/// With -index-binary-modules the binary framework is indexed as a system
/// module, but the module compiled from source is not.
// RUN: %target-swift-frontend -typecheck -parse-stdlib -swift-version 5 \
// RUN:     -index-system-modules -index-binary-modules \
// RUN:     -index-store-path %t/idx \
// RUN:     -index-ignore-stdlib \
// RUN:     -sdk %t/SDK \
// RUN:     -F %t/Frameworks -I %t/Modules \
// RUN:     -module-cache-path %t/modulecache \
// RUN:     %t/Client.swift
// RUN: c-index-test core -print-unit %t/idx | %FileCheck -check-prefix=CLIENT %s
/// The client now links to a unit for the binary framework, marked as system.
// CLIENT-DAG: Unit | system | BinaryFramework | {{.*}}.swiftinterface | {{.+}}
// CLIENT-DAG: Unit | user | SourceModule | {{.*}}SourceModule.swiftmodule{{$}}
// RUN: c-index-test core -print-unit %t/idx | %FileCheck -check-prefix=MODULE-UNIT %s
// MODULE-UNIT: is-system: 1
// MODULE-UNIT-NEXT: is-module: 1
// MODULE-UNIT-NEXT: module-name: BinaryFramework
// MODULE-UNIT: Record | system | BinaryFramework |
// RUN: c-index-test core -print-unit %t/idx | %FileCheck -check-prefix=NO-SOURCE-UNIT %s
// NO-SOURCE-UNIT-NOT: module-name: SourceModule
// RUN: c-index-test core -print-record %t/idx | %FileCheck -check-prefix=RECORD %s
// RECORD: class/Swift | BinaryType |
// RECORD: instance-method/Swift | binaryMethod(label:)

//--- BinaryFramework.swift
public class BinaryType {
  public init() {}
  public func binaryMethod(label: BinaryType) {}
}

//--- SourceModule.swift
public class SourceType {
  public init() {}
}

//--- Client.swift
import BinaryFramework
import SourceModule

func use(_ binary: BinaryType, _ source: SourceType) {
  binary.binaryMethod(label: binary)
}
