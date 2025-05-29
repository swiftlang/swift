func foo() {}

// REQUIRES: swift_swift_parser
// RUN: %sourcekitd-test -req=syntax-map %s == -req=stats | %FileCheck %s -check-prefix=SYNTAX_1

// SYNTAX_1:   {{.*}} source.statistic.instruction-count
// SYNTAX_1: 2 {{.*}} source.statistic.num-requests
// SYNTAX_1: 0 {{.*}} source.statistic.num-semantic-requests
// SYNTAX_1: 0 {{.*}} source.statistic.num-ast-builds
// SYNTAX_1: 1 {{.*}} source.statistic.num-open-documents
// SYNTAX_1: 1 {{.*}} source.statistic.max-open-documents

// RUN: %sourcekitd-test -req=syntax-map %s == -req=close %s == -req=stats | %FileCheck %s -check-prefix=SYNTAX_2

// SYNTAX_2: 3 {{.*}} source.statistic.num-requests
// SYNTAX_2: 0 {{.*}} source.statistic.num-semantic-requests
// SYNTAX_2: 0 {{.*}} source.statistic.num-ast-builds
// SYNTAX_2: 0 {{.*}} source.statistic.num-open-documents
// SYNTAX_2: 1 {{.*}} source.statistic.max-open-documents

// RUN: %sourcekitd-test -req=sema %s -- -Xfrontend -disable-implicit-concurrency-module-import  %s == -req=stats | %FileCheck %s -check-prefix=SEMA_1

// SEMA_1: 3 {{.*}} source.statistic.num-requests
// SEMA_1: 0 {{.*}} source.statistic.num-semantic-requests
// SEMA_1: 1 {{.*}} source.statistic.num-ast-builds
// SEMA_1: 1 {{.*}} source.statistic.num-asts-in-memory
// SEMA_1: 1 {{.*}} source.statistic.max-asts-in-memory
// SEMA_1: 0 {{.*}} source.statistic.num-ast-cache-hits
// SEMA_1: 0 {{.*}} source.statistic.num-ast-snapshot-uses

// RUN: %sourcekitd-test -req=sema %s -- -Xfrontend -disable-implicit-concurrency-module-import  %s == -req=edit -pos=1:1 -replace=" " %s == -req=stats | %FileCheck %s -check-prefix=SEMA_2

// SEMA_2: 5 {{.*}} source.statistic.num-requests
// SEMA_2: 0 {{.*}} source.statistic.num-semantic-requests
// SEMA_2: 2 {{.*}} source.statistic.num-ast-builds
// NOTE: we cannot match num-asts-in-memory, or num-ast-cache-hits reliably when
// doing edits. The first ASTConsumer can still be running when the edit happens
// in which case we may trigger an extra processLatestSnapshotAsync() which will
// hit the cache, and/or keep the first AST in memory long enough to be reported
// here.
// SEMA_2: 0 {{.*}} source.statistic.num-ast-snapshot-uses

// RUN: %sourcekitd-test -req=sema %s -- -Xfrontend -disable-implicit-concurrency-module-import  %s == -req=cursor -pos=1:6 %s -- -Xfrontend -disable-implicit-concurrency-module-import  %s == -req=stats | %FileCheck %s -check-prefix=SEMA_3

// SEMA_3: 4 {{.*}} source.statistic.num-requests
// SEMA_3: 1 {{.*}} source.statistic.num-semantic-requests
// SEMA_3: 1 {{.*}} source.statistic.num-ast-builds
// SEMA_3: 1 {{.*}} source.statistic.num-asts-in-memory
// SEMA_3: 1 {{.*}} source.statistic.max-asts-in-memory
// SEMA_3: 1 {{.*}} source.statistic.num-ast-cache-hits
// SEMA_3: 0 {{.*}} source.statistic.num-ast-snapshot-uses

// RUN: %sourcekitd-test -req=sema %s -- -Xfrontend -disable-implicit-concurrency-module-import  %s == -req=related-idents -pos=1:6 %s -- -Xfrontend -disable-implicit-concurrency-module-import  %s == -req=stats | %FileCheck %s -check-prefix=SEMA_4

// SEMA_4: 4 {{.*}} source.statistic.num-requests
// SEMA_4: 1 {{.*}} source.statistic.num-semantic-requests
// SEMA_4: 1 {{.*}} source.statistic.num-ast-builds
// SEMA_4: 1 {{.*}} source.statistic.num-asts-in-memory
// SEMA_4: 1 {{.*}} source.statistic.max-asts-in-memory
// SEMA_4: 1 {{.*}} source.statistic.num-ast-cache-hits
// SEMA_4: 0 {{.*}} source.statistic.num-ast-snapshot-uses

// Test that we can have two files open and don't need to rebuild an AST when doing the cursor info request on '%s' after opening '10bytes.swift'
// RUN: %sourcekitd-test \
// RUN: -req=sema %s -- -Xfrontend -disable-implicit-concurrency-module-import  %s == \
// RUN: -req=sema %S/Inputs/10bytes.swift -- -Xfrontend -disable-implicit-concurrency-module-import %S/Inputs/10bytes.swift == \
// RUN: -req=cursor -pos=1:6 %s -- -Xfrontend -disable-implicit-concurrency-module-import  %s == \
// RUN: -req=stats | %FileCheck %s -check-prefix=OPEN_TWO_FILES

// OPEN_TWO_FILES: 2 {{.*}} source.statistic.num-ast-builds
// OPEN_TWO_FILES: 2 {{.*}} source.statistic.num-asts-in-memory
// OPEN_TWO_FILES: 2 {{.*}} source.statistic.max-asts-in-memory
