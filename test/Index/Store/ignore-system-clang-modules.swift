// RUN: %empty-directory(%t)

// Make a basic clang framework to import
//
// RUN: %empty-directory(%t/sdk/MySystemFramework.framework/Headers)
// RUN: %empty-directory(%t/sdk/MySystemFramework.framework/Modules)
// RUN: echo 'void someSystemFunc(int arg);' > %t/sdk/MySystemFramework.framework/Headers/MySystemFramework.h
// RUN: echo 'framework module MySystemFramework { umbrella header "MySystemFramework.h" export * }' > %t/sdk/MySystemFramework.framework/Modules/module.modulemap

import MySystemFramework
someSystemFunc(2)

// Index this file with and without ignoring system frameworks
//
// RUN: %target-swiftc_driver -index-store-path %t/idx1 -o %t/file.o -Fsystem %t/sdk -sdk %t/sdk -typecheck %s
// RUN: %target-swiftc_driver -index-store-path %t/idx2 -o %t/file.o -index-ignore-system-modules -Fsystem %t/sdk -sdk %t/sdk -typecheck %s
// RUN: c-index-test core -print-unit %t/idx1 | %FileCheck --check-prefixes=ALLOWSYSTEM,BOTH %s
// RUN: c-index-test core -print-unit %t/idx2 | %FileCheck --check-prefixes=IGNORESYSTEM,BOTH %s

// We should always get a dependency on the system framework in the unit for this file's module.
//
// BOTH: DEPEND START
// BOTH: Unit | system | MySystemFramework |
// BOTH: DEPEND END

// We should get a unit for the system framework if not ignoring them.
//
// ALLOWSYSTEM: provider: clang
// ALLOWSYSTEM-NEXT: is-system: 1
// ALLOWSYSTEM-NEXT: is-module: 1
// ALLOWSYSTEM-NEXT: module-name: MySystemFramework

// But shouldn't if we are.
//
// IGNORESYSTEM-NOT: module-name: MySystemFramework
