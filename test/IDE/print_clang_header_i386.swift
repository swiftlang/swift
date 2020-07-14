// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64
// FIXME: rdar://problem/19648117 Needs splitting objc parts out
// XFAIL: linux, freebsd

// RUN: echo '#include "header-to-print.h"' > %t.i386.m
// RUN: %target-swift-ide-test -source-filename %s -print-header -header-to-print %S/Inputs/print_clang_header/header-to-print.h -print-regular-comments --cc-args -arch i386 -isysroot %clang-importer-sdk-path -fsyntax-only %t.i386.m -I %S/Inputs/print_clang_header > %t.i386.txt
// RUN: diff -u %S/Inputs/print_clang_header/header-to-print.h.printed.txt %t.i386.txt
