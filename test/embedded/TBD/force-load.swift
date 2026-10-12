// Embedded Swift does not use force-load symbols, even with
// -autolink-force-load.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir -o %t/Lib.ll %s -parse-as-library -module-name Lib -module-link-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -validate-tbd-against-ir=all
// RUN: %FileCheck -check-prefix NO-FORCE-LOAD %s < %t/Lib.ll
// RUN: %target-swift-frontend -emit-ir -o %t/Lib-force.ll %s -parse-as-library -module-name Lib -module-link-name Lib -autolink-force-load -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -validate-tbd-against-ir=all
// RUN: %FileCheck -check-prefix NO-FORCE-LOAD %s < %t/Lib-force.ll

// RUN: %target-swift-frontend -typecheck %s -parse-as-library -module-name Lib -module-link-name Lib -autolink-force-load -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -emit-tbd-path %t/Lib.tbd -tbd-install_name Lib
// RUN: %FileCheck -check-prefix NO-FORCE-LOAD %s < %t/Lib.tbd

// A client of a module built with -autolink-force-load doesn't refer to its
// force-load symbol either.
// RUN: %target-swift-frontend -emit-module -o %t/Lib.swiftmodule %s -parse-as-library -module-name Lib -module-link-name Lib -autolink-force-load -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface
// RUN: echo 'import Lib; public func g() { f() }' > %t/Client.swift
// RUN: %target-swift-frontend -emit-ir %t/Client.swift -I %t -parse-as-library -module-name Client -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface | %FileCheck -check-prefix NO-FORCE-LOAD %s

// REQUIRES: swift_feature_Embedded
// REQUIRES: VENDOR=apple

// NO-FORCE-LOAD-NOT: FORCE_LOAD

public func f() {}
