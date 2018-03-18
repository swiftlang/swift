// RUN: %empty-directory(%t)
// RUN: mkdir %t/idx

// Before indexing, do a dry-run to ensure any clang modules are cached. We
// want to isolate the error that comes from swift's indexing support, not
// any failures indexing a clang module.
// RUN: %target-swift-frontend %s -typecheck

// RUN: chmod -w %t/idx
// RUN: not %target-swift-frontend -index-store-path %t/idx %s -typecheck -o %t/oof.o 2> %t/dir.txt || chmod +w %t/idx
// This is not combined with the previous chmod because of pipefail mode.
// RUN: chmod +w %t/idx
// RUN: %FileCheck %s -check-prefix=DIR_ERR < %t/dir.txt
// DIR_ERR: error: creating index directory

// RUN: %target-swift-frontend -index-store-path %t/idx %s -typecheck -o %t/oof.o
// test -s %t/idx/*/units/oof.o*

// RUN: chmod -w %t/idx/*/units
// RUN: not %target-swift-frontend -index-store-path %t/idx %s -typecheck -o %t/oof.o 2> %t/file.txt || chmod +w %t/idx/*/units
// This is not combined with the previous chmod because of pipefail mode.
// RUN: chmod +w %t/idx/*/units
// RUN: %FileCheck %s -check-prefix=FILE_ERR < %t/file.txt
// FILE_ERR: error: writing index unit file

// RUN: rm -rf %t/idx/*/records/*
// RUN: chmod -x %t/idx/*/records
// RUN: not %target-swift-frontend -index-store-path %t/idx %s -typecheck -o %t/oof.o 2> %t/record.txt || chmod +x %t/idx/*/records
// This is not combined with the previous chmod because of pipefail mode.
// RUN: chmod +x %t/idx/*/records
// RUN: %FileCheck %s -check-prefix=RECORD_ERR < %t/record.txt
// RECORD_ERR: error: writing index record file

// RUN: rm -rf %t/idx/*/records/*
// RUN: chmod -w %t/idx/*/records
// RUN: not %target-swift-frontend -index-store-path %t/idx %s -typecheck -o %t/oof.o 2> %t/record2.txt || chmod +w %t/idx/*/records
// This is not combined with the previous chmod because of pipefail mode.
// RUN: chmod +w %t/idx/*/records
// RUN: %FileCheck %s -check-prefix=RECORD_ERR < %t/record2.txt

func foo() {}
