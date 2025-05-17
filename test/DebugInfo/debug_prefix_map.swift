// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/Globals.swiftmodule %S/Globals.swift
// RUN: %target-swiftc_driver -g -debug-prefix-map %S=/var/empty -debug-prefix-map %t=/var/empty %/s -I %t -emit-ir -o - | %FileCheck %s

import Globals

func square(_ n: Int) -> Int {
  return n * n
}

// CHECK: !DIFile(filename: "/var/empty{{/|\\\\}}debug_prefix_map.swift"
// CHECK: !DIModule(scope: null, name: "Globals", {{.*}}includePath: "/var/empty{{(/|\\\\)}}Globals.swiftmodule"
