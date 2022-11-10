// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/Library.swiftmodule -module-name Library %S/Inputs/moveonly_klass.swift -enable-experimental-move-only
// RUN: not %target-swift-frontend -I %t %s -emit-sil -o /dev/null 2>&1 | %FileCheck %s

// This test makes sure that if we import a move only type and do not set the
// experimental move only flag, we get a nice error.

import Library

// CHECK: error: Can not import module 'Library' that uses move only features when experimental move only is disabled! Pass the frontend flag -enable-experimental-move-only to swift to enable the usage of this language feature

func f(_ k: Klass) {
}
