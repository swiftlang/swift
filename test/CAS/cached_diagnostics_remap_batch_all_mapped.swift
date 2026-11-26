// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// A batch job whose inputs are all prefix mapped, as Xcode builds them with
/// SWIFT_ENABLE_PREFIX_MAPPING and batch mode on (swiftlang/swift#93917). Each
/// primary's diagnostics must reach only that primary's serialized diagnostics
/// file, on a cache miss and on replay.
// RUN: %target-swift-frontend -scan-dependencies -module-name Test -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/src/a.swift %t/src/b.swift -o %t/deps.json -cache-compile-job -cas-path %t/cas -scanner-prefix-map-paths %t/src /^src

// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps.json -o %t/MyApp.cmd

/// Cache miss: the first compile of this key in a fresh CAS.
// RUN: %target-swift-frontend-plain -cache-compile-job -module-name Test -O -cas-path %t/cas @%t/MyApp.cmd -cache-replay-prefix-map /^src %t/src \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -c -o %t/a.o -o %t/b.o -serialize-diagnostics-path %t/a.dia -serialize-diagnostics-path %t/b.dia \
// RUN:   -primary-file /^src/a.swift -primary-file /^src/b.swift \
// RUN:   2>&1 | %FileCheck %s --check-prefix=MISS-WARN
// RUN: c-index-test -read-diagnostics %t/a.dia 2>&1 | %FileCheck %s --check-prefix=A-DIAG
// RUN: c-index-test -read-diagnostics %t/b.dia 2>&1 | %FileCheck %s --check-prefix=B-DIAG

/// Replay: the same key, which -Rcache-compile-job does not change.
// RUN: rm %t/a.dia %t/b.dia
// RUN: %target-swift-frontend-plain -cache-compile-job -module-name Test -O -cas-path %t/cas @%t/MyApp.cmd -cache-replay-prefix-map /^src %t/src \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -c -o %t/a.o -o %t/b.o -serialize-diagnostics-path %t/a.dia -serialize-diagnostics-path %t/b.dia \
// RUN:   -primary-file /^src/a.swift -primary-file /^src/b.swift -Rcache-compile-job \
// RUN:   2>&1 | %FileCheck %s --check-prefix=HIT
// RUN: c-index-test -read-diagnostics %t/a.dia 2>&1 | %FileCheck %s --check-prefix=A-DIAG
// RUN: c-index-test -read-diagnostics %t/b.dia 2>&1 | %FileCheck %s --check-prefix=B-DIAG

// MISS-WARN-DAG: TMP_DIR{{/|\\}}src{{/|\\}}a.swift:1:10: warning: from a
// MISS-WARN-DAG: TMP_DIR{{/|\\}}src{{/|\\}}b.swift:1:10: warning: from b

// HIT-DAG: remark: replay output file
// HIT-DAG: TMP_DIR{{/|\\}}src{{/|\\}}a.swift:1:10: warning: from a
// HIT-DAG: TMP_DIR{{/|\\}}src{{/|\\}}b.swift:1:10: warning: from b

// A-DIAG-NOT: from b
// A-DIAG: {{/|\\}}src{{/|\\}}a.swift:1:10: warning: from a
// A-DIAG-NOT: from b

// B-DIAG-NOT: from a
// B-DIAG: {{/|\\}}src{{/|\\}}b.swift:1:10: warning: from b
// B-DIAG-NOT: from a

//--- src/a.swift
#warning("from a")

//--- src/b.swift
#warning("from b")
