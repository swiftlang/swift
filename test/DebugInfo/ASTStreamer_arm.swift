// Check that we use 4-byte relocations on ARM32. <rdar://problem/15360043>
// RUN: %swift -triple armv7-apple-ios7 %S/ASTStreamer.swift -S -g -o - | FileCheck %s -check-prefix=ARM32
// REQUIRES: ARM

// ARM32: .section	__SWIFT,__ast
// ARM32: [[SECTION:.*]]:
// Magic
// ARM32: .long 1095980147
// Version
// ARM32: .long 1
// Language
// ARM32: .long	15
// Flags
// ARM32: .long
// Bitstream offset
// ARM32: [[BSSYM:.*]] = [[BITSTREAM:.*]]-[[SECTION]]
// ARM32-NEXT: .long{{.*}}[[BSSYM]]
// Bitstream bytesize
// ARM32: .long
// ARM32: .long
// ARM32: .ascii "ASTStreamer"
// ARM32: [[BITSTREAM]]:
// ARM32: .long
// ARM32: .long

// ======================================================================
// Make sure we don't crash building for ARM32. <rdar://problem/15360043>
// ======================================================================
// RUN: %swift %s -triple armv7-apple-ios7 -c -g -o %t.o
