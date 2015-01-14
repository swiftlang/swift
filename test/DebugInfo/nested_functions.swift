// RUN: %swift -target x86_64-apple-macosx10.9 -primary-file %s -emit-ir -g -o - | FileCheck %s
func outer(a: Int) -> Int {
     // Inner functions have a linkage name of "closure[0-9]+", but
     // their DW_AT_name is preserved.

     // CHECK: \00_TFF16nested_functions5outerFSiSiL33_C327CA4691633C3CAC5D0746F943AABD_5innerfSiSi\00[[@LINE+1]]\000\001\000\000\000\000\00[[@LINE+1]]", !1, !24, !31, null, i64 (i64, i64)* @_TFF16nested_functions5outerFSiSiL33_C327CA4691633C3CAC5D0746F943AABD_5innerfSiSi, null, null, !2} ; [ DW_TAG_subprogram ] [line [[@LINE+1]]] [def] [inner]
     func inner(b: Int) -> Int {
       return a+b
     }

     return inner(42)
}
