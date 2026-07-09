// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-ide-test -disable-safe-interop-wrappers -Xcc -fexperimental-bounds-safety-attributes -print-module -module-to-print=NullableTypedef -I %t/Inputs -source-filename=x | %FileCheck %s --check-prefixes=CHECK,ATTR --match-full-lines
// RUN: %target-swift-ide-test -disable-safe-interop-wrappers -Xcc -fbounds-safety -disable-objc-interop -print-module -module-to-print=NullableTypedef -I %t/Inputs -source-filename=x | %FileCheck %s --check-prefixes=CHECK,BOUNDS --match-full-lines

// Regression test for nullability being dropped when a __counted_by/__sized_by/
// __ended_by (clang BoundsAttributedType) is applied to a pointer whose nullability attribute is inside a typedef.
//
// In the default -fexperimental-bounds-safety-attributes mode clang rebuilds the
// pointer to its canonical type when forming the BoundsAttributedType, discarding
// the typedef sugar and with it the _Nonnull, so the pointer is imported as
// optional. Under -fbounds-safety the nullability is carried on the pointer type
// itself and is preserved. Tracked as https://github.com/swiftlang/llvm-project/issues/13277

//--- Inputs/nullable-typedef.h
#pragma once

#include <ptrcheck.h>

// The typedef itself is (correctly) nonnullable.
// CHECK: typealias nonnull_int_ptr_t = UnsafeMutablePointer<Int{{[0-9]+}}>
typedef int * _Nonnull nonnull_int_ptr_t;

// FIXME: pointer parameters and return types should be nonnull in both modes
// ATTR:   func cb(_ len: Int{{[0-9]+}}, _ p: UnsafeMutablePointer<Int{{[0-9]+}}>!) -> UnsafeMutablePointer<Int{{[0-9]+}}>!
// BOUNDS: func cb(_ len: Int{{[0-9]+}}, _ p: UnsafeMutablePointer<Int{{[0-9]+}}>) -> UnsafeMutablePointer<Int{{[0-9]+}}>
nonnull_int_ptr_t __counted_by(len) cb(int len, nonnull_int_ptr_t __counted_by(len) p);
// FIXME: ditto 
// ATTR:   func sb(_ len: Int{{[0-9]+}}, _ p: UnsafeMutablePointer<Int{{[0-9]+}}>!) -> UnsafeMutablePointer<Int{{[0-9]+}}>!
// BOUNDS: func sb(_ len: Int{{[0-9]+}}, _ p: UnsafeMutablePointer<Int{{[0-9]+}}>) -> UnsafeMutablePointer<Int{{[0-9]+}}>
nonnull_int_ptr_t __sized_by(len) sb(int len, nonnull_int_ptr_t __sized_by(len) p);
// FIXME: ditto
// ATTR:   func eb(_ p: UnsafeMutablePointer<Int{{[0-9]+}}>!, _ end: nonnull_int_ptr_t) -> UnsafeMutablePointer<Int{{[0-9]+}}>!
// BOUNDS: func eb(_ p: UnsafeMutablePointer<Int{{[0-9]+}}>, _ end: UnsafeMutablePointer<Int{{[0-9]+}}>) -> UnsafeMutablePointer<Int{{[0-9]+}}>
nonnull_int_ptr_t __ended_by(end) eb(nonnull_int_ptr_t __ended_by(end) p, nonnull_int_ptr_t end);

// __single keeps the sugar in both modes.
// CHECK: func sg(_ p: UnsafeMutablePointer<Int{{[0-9]+}}>) -> UnsafeMutablePointer<Int{{[0-9]+}}>
nonnull_int_ptr_t __single sg(nonnull_int_ptr_t __single p);

// No bounds attribute: the typedef (and its _Nonnull) is preserved in both modes.
// ATTR:   func plain(_ p: nonnull_int_ptr_t) -> nonnull_int_ptr_t
// BOUNDS: func plain(_ p: UnsafeMutablePointer<Int{{[0-9]+}}>) -> UnsafeMutablePointer<Int{{[0-9]+}}>
nonnull_int_ptr_t plain(nonnull_int_ptr_t p);

// _Nonnull spelled directly on the pointer (not via a typedef) is preserved even
// with a bounds attribute, in both modes.
// CHECK: func cb_direct(_ len: Int{{[0-9]+}}, _ p: UnsafeMutablePointer<Int{{[0-9]+}}>) -> UnsafeMutablePointer<Int{{[0-9]+}}>
int *_Nonnull __counted_by(len) cb_direct(int len, int *_Nonnull __counted_by(len) p);

//--- Inputs/module.modulemap
module NullableTypedef {
  header "nullable-typedef.h"
  export *
}
