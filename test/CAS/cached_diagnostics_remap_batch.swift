// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Check path remapping.
// RUN: %target-swift-frontend -scan-dependencies -module-name Test -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/sub/test.swift %t/sub/foo.swift %t/bar.swift -o %t/deps.json -cache-compile-job -cas-path %t/cas -scanner-prefix-map-paths %t/sub /^test

// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps.json -o %t/MyApp.cmd

// RUN: %target-swift-frontend-plain -cache-compile-job -module-name Test -O -cas-path %t/cas @%t/MyApp.cmd -cache-replay-prefix-map /^test %t/sub \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -c -o %t/test.o -primary-file /^test/test.swift /^test/foo.swift  %t/bar.swift 2>&1 | %FileCheck %s --check-prefix=TEST

// RUN: %target-swift-frontend-plain -cache-compile-job -module-name Test -O -cas-path %t/cas @%t/MyApp.cmd -cache-replay-prefix-map /^test %t/sub \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -c -o %t/foo.o -o %t/bar.o -serialize-diagnostics-path %t/foo.diag -serialize-diagnostics-path %t/bar.diag \
// RUN:   /^test/test.swift -primary-file /^test/foo.swift -primary-file %t/bar.swift 2>&1 | %FileCheck %s --check-prefix=FOO

// TEST: TMP_DIR{{/|\\}}sub{{/|\\}}test.swift:1:10: warning: this is a warning
// FOO: TMP_DIR{{/|\\}}sub{{/|\\}}foo.swift:1:10: warning: foo warning
// FOO: TMP_DIR{{/|\\}}bar.swift:1:10: warning: bar warning

//--- /sub/test.swift
#warning("this is a warning")

//--- /sub/foo.swift
#warning("foo warning")

//--- bar.swift
#warning("bar warning")
