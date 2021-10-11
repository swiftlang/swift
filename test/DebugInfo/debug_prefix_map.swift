// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/Globals.swiftmodule %S/Globals.swift
// RUN: %target-swiftc_driver -g -debug-prefix-map %/S=/var/empty -debug-prefix-map %t=/var/empty %/s -I %t -emit-ir -o - | %FileCheck %s

// RUN: %empty-directory(%t) && cd %t && cp %s .
// RUN: %target-swift-frontend -emit-module-path %t/Globals.swiftmodule %S/Globals.swift
// RUN: %target-swiftc_driver -g -debug-prefix-map %/S=. -debug-prefix-map %t=. debug_prefix_map.swift -I %t -emit-ir -o - | %FileCheck %s --check-prefix=CHECK-RELATIVE

// RUN: mkdir -p %t/nested/dir && cd %t/nested/dir && echo > foo.swift
// RUN: %target-swiftc_driver -g -debug-prefix-map %/t/nested=. %/t/nested/dir/foo.swift -emit-ir -o - | %FileCheck %s --check-prefix=CHECK-PARTIAL-REMAP

import Globals

func square(_ n: Int) -> Int {
  return n * n
}

// CHECK: !DIFile(filename: "/var/empty/debug_prefix_map.swift"
// CHECK: !DIModule(scope: null, name: "Globals", {{.*}}includePath: "/var/empty{{(/|\\\\)}}Globals.swiftmodule"

// CHECK-RELATIVE: !DIFile(filename: "debug_prefix_map.swift", directory: ".")
// CHECK-RELATIVE: !DIModule(scope: null, name: "Globals", {{.*}}includePath: ".{{(/|\\\\)}}Globals.swiftmodule"

// CHECK-PARTIAL-REMAP: !DIFile(filename: "foo.swift", directory: "dir")
