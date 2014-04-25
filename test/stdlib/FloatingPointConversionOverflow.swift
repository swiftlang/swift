// RUN: not --crash %swift -i %s --  1 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s --  2 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s --  3 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s --  4 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s --  5 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s --  6 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s --  7 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s --  8 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s --  9 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 10 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 11 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 12 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 13 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 14 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 15 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 16 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 17 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 18 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 19 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 20 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 21 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 22 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 23 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 24 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 25 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 26 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 27 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 28 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 29 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 30 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 31 2>&1 | FileCheck %s
// RUN: not --crash %swift -i %s -- 32 2>&1 | FileCheck %s

// REQUIRES: swift_interpreter

// These tests should crash (until we change overflow trapping mechanizm in stdlib).
// FIXME: iOS needs crash handling

// Interpret the command line arguments.
let arg = Process.arguments[0]

let pd = +Double.inf()
let nd = -Double.inf()
let pf = +Float.inf()
let nf = -Float.inf()

con.write("GOOD") // CHECK: GOOD

if arg ==  "1" { let x =  UInt8(nd); con.write("BAD") } // CHECK-NOT: BAD
if arg ==  "2" { let x = UInt16(nd); con.write("BAD") } // CHECK-NOT: BAD
if arg ==  "3" { let x = UInt32(nd); con.write("BAD") } // CHECK-NOT: BAD
if arg ==  "4" { let x = UInt64(nd); con.write("BAD") } // CHECK-NOT: BAD

if arg ==  "5" { let x =  UInt8(nf); con.write("BAD") } // CHECK-NOT: BAD
if arg ==  "6" { let x = UInt16(nf); con.write("BAD") } // CHECK-NOT: BAD
if arg ==  "7" { let x = UInt32(nf); con.write("BAD") } // CHECK-NOT: BAD
if arg ==  "8" { let x = UInt64(nf); con.write("BAD") } // CHECK-NOT: BAD

if arg ==  "9" { let x =  UInt8(nd); con.write("BAD") } // CHECK-NOT: BAD
if arg == "10" { let x = UInt16(nd); con.write("BAD") } // CHECK-NOT: BAD
if arg == "11" { let x = UInt32(nd); con.write("BAD") } // CHECK-NOT: BAD
if arg == "12" { let x = UInt64(nd); con.write("BAD") } // CHECK-NOT: BAD

if arg == "13" { let x =  UInt8(nf); con.write("BAD") } // CHECK-NOT: BAD
if arg == "14" { let x = UInt16(nf); con.write("BAD") } // CHECK-NOT: BAD
if arg == "15" { let x = UInt32(nf); con.write("BAD") } // CHECK-NOT: BAD
if arg == "16" { let x = UInt64(nf); con.write("BAD") } // CHECK-NOT: BAD

if arg == "17" { let x =  Int8(nd); con.write("BAD") } // CHECK-NOT: BAD
if arg == "18" { let x = Int16(nd); con.write("BAD") } // CHECK-NOT: BAD
if arg == "19" { let x = Int32(nd); con.write("BAD") } // CHECK-NOT: BAD
if arg == "20" { let x = Int64(nd); con.write("BAD") } // CHECK-NOT: BAD

if arg == "21" { let x =  Int8(nf); con.write("BAD") } // CHECK-NOT: BAD
if arg == "22" { let x = Int16(nf); con.write("BAD") } // CHECK-NOT: BAD
if arg == "23" { let x = Int32(nf); con.write("BAD") } // CHECK-NOT: BAD
if arg == "24" { let x = Int64(nf); con.write("BAD") } // CHECK-NOT: BAD

if arg == "25" { let x =  Int8(pd); con.write("BAD") } // CHECK-NOT: BAD
if arg == "26" { let x = Int16(pd); con.write("BAD") } // CHECK-NOT: BAD
if arg == "27" { let x = Int32(pd); con.write("BAD") } // CHECK-NOT: BAD
if arg == "28" { let x = Int64(pd); con.write("BAD") } // CHECK-NOT: BAD

if arg == "29" { let x =  Int8(pf); con.write("BAD") } // CHECK-NOT: BAD
if arg == "30" { let x = Int16(pf); con.write("BAD") } // CHECK-NOT: BAD
if arg == "31" { let x = Int32(pf); con.write("BAD") } // CHECK-NOT: BAD
if arg == "32" { let x = Int64(pf); con.write("BAD") } // CHECK-NOT: BAD
