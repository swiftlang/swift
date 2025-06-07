// REQUIRES: VENDOR=apple
// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck %s -tbd-install_name some_dylib -emit-tbd-path - -tbd-current-version 2.0 | %FileCheck %s --check-prefix TWOPOINTZERO
// RUN: %target-swift-frontend -typecheck %s -tbd-install_name some_dylib -emit-tbd-path - -tbd-current-version 2 | %FileCheck %s --check-prefix TWOPOINTZERO
// RUN: %target-swift-frontend -typecheck %s -tbd-install_name some_dylib -emit-tbd-path - -tbd-current-version 20.10 | %FileCheck %s --check-prefix TWENTYPOINTTEN

// RUN: %target-swift-frontend -typecheck %s -tbd-install_name some_dylib -emit-tbd-path - -tbd-compatibility-version 2.0 | %FileCheck %s --check-prefix TWOPOINTZEROCOMPAT
// RUN: %target-swift-frontend -typecheck %s -tbd-install_name some_dylib -emit-tbd-path - -tbd-compatibility-version 2 | %FileCheck %s --check-prefix TWOPOINTZEROCOMPAT
// RUN: %target-swift-frontend -typecheck %s -tbd-install_name some_dylib -emit-tbd-path - -tbd-compatibility-version 20.10 | %FileCheck %s --check-prefix TWENTYPOINTTENCOMPAT

// Make sure we correctly truncate a value over 255

// RUN: %target-swift-frontend -typecheck %s -tbd-install_name some_dylib -emit-tbd-path - -tbd-current-version 20.300 2>&1 | %FileCheck %s --check-prefix TWENTYPOINTTHREEHUNDRED
// RUN: %target-swift-frontend -typecheck %s -tbd-install_name some_dylib -emit-tbd-path - -tbd-compatibility-version 20.300 2>&1 | %FileCheck %s --check-prefix TWENTYPOINTTHREEHUNDREDCOMPAT

// TWOPOINTZERO: current_versions
// TWOPOINTZERO: "version": "2"

// TWENTYPOINTTEN: current_versions 
// TWENTYPOINTTEN: "version": "20.10"

// TWOPOINTZEROCOMPAT: compatibility_versions
// TWOPOINTZEROCOMPAT: "version": "2"

// TWENTYPOINTTENCOMPAT: compatibility_versions
// TWENTYPOINTTENCOMPAT: "version": "20.10"

// TWENTYPOINTTHREEHUNDRED: warning: truncating current version '20.300' in TBD file to fit in 32-bit space used by old mach-o format
// TWENTYPOINTTHREEHUNDRED: current_versions
// TWENTYPOINTTHREEHUNDRED: "version": "20.255"

// TWENTYPOINTTHREEHUNDREDCOMPAT: warning: truncating compatibility version '20.300' in TBD file to fit in 32-bit space used by old mach-o format
// TWENTYPOINTTHREEHUNDREDCOMPAT: compatibility_versions
// TWENTYPOINTTHREEHUNDREDCOMPAT: "version": "20.255"
