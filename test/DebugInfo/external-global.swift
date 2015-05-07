// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -emit-module-path %t/Globals.swiftmodule %S/Globals.swift
// RUN: %target-swift-frontend -I %t %s -g -emit-ir -o - | FileCheck %s
import Globals
// No debug info should be emitted for an externally defined global variable.
// CHECK-NOT: !DIGlobalVariable({{.*}}global
global = 23
