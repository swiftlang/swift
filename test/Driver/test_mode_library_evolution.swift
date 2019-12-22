// RUN: %swiftc_driver -driver-print-jobs -enable-testing -enable-library-evolution %s 2>&1 | %FileCheck %s

// CHECK: warning: specifying '-enable-library-evolution' and '-enable-testing' together will cause program instability

@testable import Module
