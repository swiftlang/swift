var s = 10
s.

// RUN: %empty-directory(%t)
// RUN: touch %t/t.h
// RUN: %sourcekitd-test -req=sema %s -- %s -import-objc-header %t/t.h -pch-output-dir %t/pch -index-store-path %t/idx | %FileCheck %s -check-prefix=DIAG
// RUN: not find %t/idx
// DIAG: expected member name

// RUN: rm -rf %t/pch %t/idx
// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- %s -import-objc-header %t/t.h -pch-output-dir %t/pch -index-store-path %t/idx | %FileCheck %s -check-prefix=COMPLETE
// RUN: not find %t/idx
// COMPLETE: littleEndian
