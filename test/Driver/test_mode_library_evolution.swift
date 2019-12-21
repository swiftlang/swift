// RUN: %swiftc_driver -driver-print-jobs -enable-testing -enable-library-evolution %s 2>&1 | %FileCheck %s

// CHECK: warning: '-enable-library-evolution' and '-enable-testing' together can cause unexpected compatibility compilation and runtime errors

@testable import Module
