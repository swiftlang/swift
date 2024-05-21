// RUN: %empty-directory(%t)

// RUN: %target-build-swift \
// RUN:    -emit-module      \
// RUN:    -emit-module-path %t \
// RUN:    -enable-library-evolution \
// RUN:    -module-name Swiftskell \
// RUN:    -parse-as-library \
// RUN:    %S/../Inputs/Swiftskell.swift \
// RUN:   -enable-experimental-feature SuppressedAssociatedTypes \
// RUN:   -enable-experimental-feature NonescapableTypes

// RUN: %target-build-swift %s -I %t -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

import Swiftskell

print("hello, world")
// CHECK: hello, world
