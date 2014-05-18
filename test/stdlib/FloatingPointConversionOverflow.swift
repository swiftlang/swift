// These tests should crash (until we change overflow trapping mechanism in stdlib).
// RUN: mkdir -p %t
// RUN: xcrun -sdk %target-sdk-name clang++ -arch %target-cpu %S/Inputs/CatchCrashes.cpp -c -o %t/CatchCrashes.o
// RUN: %target-build-swift %s -Xlinker %t/CatchCrashes.o -o %t/a.out
//
// RUN: %target-run %t/a.out  1 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out  2 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out  3 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out  4 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out  5 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out  6 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out  7 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out  8 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out  9 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out 10 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out 11 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out 12 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out 13 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out 14 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out 15 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out 16 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out 17 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out 18 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out 19 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out 20 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out 21 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out 22 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out 23 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out 24 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out 25 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out 26 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out 27 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out 28 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out 29 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out 30 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out 31 2>&1 | FileCheck %s
// RUN: %target-run %t/a.out 32 2>&1 | FileCheck %s

import Darwin

// Interpret the command line arguments.
let arg = Process.arguments[1]

let pd = +Double.infinite()
let nd = -Double.infinite()
let pf = +Float.infinite()
let nf = -Float.infinite()

println("GOOD") // CHECK: GOOD

switch (arg) {
case  "1": let x =  UInt8(nd)
case  "2": let x = UInt16(nd)
case  "3": let x = UInt32(nd)
case  "4": let x = UInt64(nd)

case  "5": let x =  UInt8(nf)
case  "6": let x = UInt16(nf)
case  "7": let x = UInt32(nf)
case  "8": let x = UInt64(nf)

case  "9": let x =  UInt8(nd)
case "10": let x = UInt16(nd)
case "11": let x = UInt32(nd)
case "12": let x = UInt64(nd)

case "13": let x =  UInt8(nf)
case "14": let x = UInt16(nf)
case "15": let x = UInt32(nf)
case "16": let x = UInt64(nf)

case "17": let x =  Int8(nd)
case "18": let x = Int16(nd)
case "19": let x = Int32(nd)
case "20": let x = Int64(nd)

case "21": let x =  Int8(nf)
case "22": let x = Int16(nf)
case "23": let x = Int32(nf)
case "24": let x = Int64(nf)

case "25": let x =  Int8(pd)
case "26": let x = Int16(pd)
case "27": let x = Int32(pd)
case "28": let x = Int64(pd)

case "29": let x =  Int8(pf)
case "30": let x = Int16(pf)
case "31": let x = Int32(pf)
case "32": let x = Int64(pf)
default:   break
}

// CHECK: assertion failed: file {{.*}}/FloatingPoint.swift, line {{[0-9]*}}
// CHECK-NEXT: CRASHED: SIG{{ILL|TRAP}}
println("BUSTED: should have crashed already")
exit(1)
