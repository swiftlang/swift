// RUN: echo '#include "header-to-print.h"' > %t.m
// RUN: %target-swift-ide-test -source-filename %s -print-header -header-to-print %S/Inputs/header-to-print.h -print-regular-comments --cc-args -Xclang -triple -Xclang %target-triple -isysroot %clang-importer-sdk-path -fsyntax-only %t.m -I %S/Inputs > %t.txt
// RUN: diff -u %S/Inputs/header-to-print.h.printed.txt %t.txt
