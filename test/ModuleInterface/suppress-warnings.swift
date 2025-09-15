// RUN: %target-swift-frontend -compile-module-from-interface %S/Inputs/suppress-warnings/SuppressWarnings.swiftinterface -o /dev/null -module-name SuppressWarnings 2>&1 | %FileCheck %s --allow-empty
// RUN: %target-swift-frontend -typecheck %s -I %S/Inputs/suppress-warnings/ 2>&1 | %FileCheck %s --allow-empty

// Warnings should not be emitted when compiling SuppressWarnings from its interface
// CHECK-NOT: warning

import SuppressWarnings
