// RUN: %empty-directory(%t)

// RUN: %target-build-swift \
// RUN:    -emit-module-path %t \
// RUN:    -enable-library-evolution \
// RUN:    -module-name Swiftskell \
// RUN:    -parse-as-library \
// RUN:    %S/../Inputs/Swiftskell.swift -c -o %t/Swiftskell.o \
// RUN:   -enable-experimental-feature NonescapableTypes

// RUN: %target-build-swift -o %t/a.out %s -I %t %t/Swiftskell.o
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

import Swiftskell

print("hello, world")
// CHECK: hello, world
