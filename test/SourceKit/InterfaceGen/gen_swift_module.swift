// RUN: rm -rf %t.mod
// RUN: mkdir %t.mod
// RUN: %swift -emit-module -o %t.mod/swift_mod.swiftmodule %S/Inputs/swift_mod.swift -parse-as-library
// RUN: %sourcekitd-test -req=interface-gen -module swift_mod -- -I %t.mod > %t.response
// RUN: diff -u %s.response %t.response

// RUN: %sourcekitd-test -req=module-groups -module swift_mod -- -I %t.mod | FileCheck -check-prefix=GROUP-EMPTY %s
// GROUP-EMPTY: <GROUPS>
// GROUP-EMPTY-NEXT: <\GROUPS>
