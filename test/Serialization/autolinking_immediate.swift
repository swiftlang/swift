// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -parse-stdlib -o %t -module-name someModule -module-link-name module %S/../Inputs/empty.swift
// RUN: not %swift -i -lmagic %s -I=%t 2>&1 | FileCheck %s
// REQUIRES: swift_interpreter

import someModule

// CHECK-DAG: error: could not load shared library 'libmagic'
// CHECK-DAG: error: could not load shared library 'libmodule'
