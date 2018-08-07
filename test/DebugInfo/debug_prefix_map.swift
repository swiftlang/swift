// RUN: %swiftc_driver -g -debug-prefix-map %S=/var/empty %s -emit-ir -o - | %FileCheck %s

func square(_ n: Int) -> Int {
  return n * n
}

// CHECK: !DIFile(filename: "/var/empty/debug_prefix_map.swift"
