// RUN: echo '#include "header-to-print.h"' > %t.m
// RUN: %target-swift-ide-test -source-filename %s -print-header -header-to-print %S/Inputs/print_clang_header/header-to-print.h -print-regular-comments --cc-args -Xclang -triple -Xclang %target-triple -isysroot %clang-importer-sdk-path -fsyntax-only %t.m -I %S/Inputs/print_clang_header > %t.txt
// RUN: diff -u %S/Inputs/print_clang_header/header-to-print.h.printed.txt %t.txt

// FIXME: rdar://problem/19648117 Needs splitting objc parts out
// XFAIL: linux
