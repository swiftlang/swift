// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Reparenting -enable-library-evolution -emit-module %S/Inputs/reparentable_availability_lib.swift -o %t

// A conformance to P references the base conformance descriptor for P: RP,
// but because of the availability of the @reparented extension, that
// conformance descriptor is only available in macOS 14, so it should be
// weak-linked when targeting platforms older than that.

// OLD: @"$s{{.*}}1PPAA2RPTb" = extern_weak global
// RUN: %target-swift-frontend -module-name output -enable-experimental-feature Reparenting -target %target-cpu-apple-macosx13 -emit-ir -parse-as-library %s -I %t | %FileCheck %s --check-prefix=OLD

// NEW: @"$s{{.*}}1PPAA2RPTb" = external global
// RUN: %target-swift-frontend -module-name output -enable-experimental-feature Reparenting -target %target-cpu-apple-macosx14 -emit-ir -parse-as-library %s -I %t | %FileCheck %s --check-prefix=NEW

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Reparenting

import reparentable_availability_lib

public struct C: P {
  public func doP() {}
}
