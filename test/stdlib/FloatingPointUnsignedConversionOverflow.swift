// RUN: not --crash %swift -i %s -- 1 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 2 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 3 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 4 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 5 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 6 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 7 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 8 2>&1 | FileCheck %s

// REQUIRES: swift_interpreter

// These tests should crash (until we change overflow trapping mechanizm in stdlib).
// FIXME: iOS needs crash handling

// Interpret the command line arguments.
let arg = Process.arguments[0]

let d: Double = -1.0
let f: Float  = -1.0

con.write("GOOD") // CHECK: GOOD

if arg == "1" { let x =  UInt8(d); con.write("BAD") } // CHECK-NOT: BAD
if arg == "2" { let x = UInt16(d); con.write("BAD") } // CHECK-NOT: BAD
if arg == "3" { let x = UInt32(d); con.write("BAD") } // CHECK-NOT: BAD
if arg == "4" { let x = UInt64(d); con.write("BAD") } // CHECK-NOT: BAD

if arg == "5" { let x =  UInt8(f); con.write("BAD") } // CHECK-NOT: BAD
if arg == "6" { let x = UInt16(f); con.write("BAD") } // CHECK-NOT: BAD
if arg == "7" { let x = UInt32(f); con.write("BAD") } // CHECK-NOT: BAD
if arg == "8" { let x = UInt64(f); con.write("BAD") } // CHECK-NOT: BAD
