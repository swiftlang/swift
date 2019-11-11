// REQUIRES: VENDOR=apple
// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck %s -emit-tbd-path - -tbd-current-version 2.0 | %FileCheck %s --check-prefix TWOPOINTZERO
// RUN: %target-swift-frontend -typecheck %s -emit-tbd-path - -tbd-current-version 2 | %FileCheck %s --check-prefix TWOPOINTZERO
// RUN: %target-swift-frontend -typecheck %s -emit-tbd-path - -tbd-current-version 20.10 | %FileCheck %s --check-prefix TWENTYPOINTTEN

// RUN: %target-swift-frontend -typecheck %s -emit-tbd-path - -tbd-compatibility-version 2.0 | %FileCheck %s --check-prefix TWOPOINTZEROCOMPAT
// RUN: %target-swift-frontend -typecheck %s -emit-tbd-path - -tbd-compatibility-version 2 | %FileCheck %s --check-prefix TWOPOINTZEROCOMPAT
// RUN: %target-swift-frontend -typecheck %s -emit-tbd-path - -tbd-compatibility-version 20.10 | %FileCheck %s --check-prefix TWENTYPOINTTENCOMPAT

// Make sure we correctly truncate a value over 255

// RUN: %target-swift-frontend -typecheck %s -emit-tbd-path - -tbd-current-version 20.300 2>&1 | %FileCheck %s --check-prefix TWENTYPOINTTHREEHUNDRED
// RUN: %target-swift-frontend -typecheck %s -emit-tbd-path - -tbd-compatibility-version 20.300 2>&1 | %FileCheck %s --check-prefix TWENTYPOINTTHREEHUNDREDCOMPAT

// TWOPOINTZERO: current-version: 2
// TWENTYPOINTTEN: current-version: 20.10

// TWOPOINTZEROCOMPAT: compatibility-version: 2
// TWENTYPOINTTENCOMPAT: compatibility-version: 20.10

// TWENTYPOINTTHREEHUNDRED: warning: truncating current version '20.300' in TBD file to fit in 32-bit space used by old mach-o format
// TWENTYPOINTTHREEHUNDRED: current-version: 20.255

// TWENTYPOINTTHREEHUNDREDCOMPAT: warning: truncating compatibility version '20.300' in TBD file to fit in 32-bit space used by old mach-o format
// TWENTYPOINTTHREEHUNDREDCOMPAT: compatibility-version: 20.255
