// RUN: %target-swiftc_driver -### %s %S/Inputs/libEmpty.so | %FileCheck %s

// REQUIRES: autolink-extract

// CHECK-NOT: swift-autolink-extract {{.+}}.o {{.+}}Inputs/libEmpty.so -o {{.+}}.autolink
