// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -emit-module -parse-stdlib -o %t -module-name someModule -module-link-name module %S/../Inputs/empty.swift
// RUN: not %swift -interpret -lmagic %s -I %t 2>&1 | FileCheck %s

// REQUIRES: swift_interpreter
// REQUIRES: disabled
// See rdar://problem/20274745

import someModule

// CHECK-DAG: error: could not load shared library 'magic'
// CHECK-DAG: error: could not load shared library 'module'
