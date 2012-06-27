// RUN: %swift %s -i | FileCheck %s

println(demangle("_T4test1fFT3jklNSs5Int64_TS0_NSs6String_"))

// CHECK: test.swift : func f(jkl : Int64) -> (Int64, String)
