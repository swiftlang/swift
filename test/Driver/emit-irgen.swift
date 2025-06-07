// RUN: %swiftc_driver -### -emit-irgen %s -o - | %FileCheck %s

// CHECK: -emit-irgen
