// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)

/// Check that -write-output-hash-xattr without -cache-compile-job is an error.
// RUN: not %target-swift-frontend -c -parse-stdlib %s -o %t/test.o \
// RUN:   -write-output-hash-xattr 2>&1 | %FileCheck --check-prefix=NO-CACHING %s
// NO-CACHING: -write-output-hash-xattr requires -cache-compile-job

// RUN: %target-swift-frontend -scan-dependencies -module-name Test \
// RUN:   -disable-implicit-string-processing-module-import \
// RUN:   -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %s -o %t/deps.json -cache-compile-job -cas-path %t/cas

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-string-processing-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-concurrency-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-parse-stdlib\"" >> %t/MyApp.cmd

/// Run the command first time, expect cache miss.
// RUN: %target-swift-frontend-plain -c -cache-compile-job -Rcache-compile-job \
// RUN:   -cas-path %t/cas %s -o %t/test.o @%t/MyApp.cmd \
// RUN:   -write-output-hash-xattr 2>&1 | %FileCheck --check-prefix=CACHE-MISS %s

// RUN: xattr -lx %t/test.o | %FileCheck %s

/// Run the command a second time, expect cache hit.
// RUN: %target-swift-frontend-plain -c -cache-compile-job -Rcache-compile-job \
// RUN:   -cas-path %t/cas %s -o %t/test.o @%t/MyApp.cmd \
// RUN:   -write-output-hash-xattr 2>&1 | %FileCheck --check-prefix=CACHE-HIT %s

// RUN: xattr -lx %t/test.o | %FileCheck %s

// The following checks the hash schema name and the 32-byte hash size.
// CHECK: com.apple.clang.cas_output_hash:
// CHECK: 00000000  6C 6C 76 6D 2E 63 61 73 2E 62 75 69 6C 74 69 6E  |llvm.cas.builtin|
// CHECK: 00000010  2E 76 32 5B 42 4C 41 4B 45 33 5D 00 20 00 00 00  |.v2[BLAKE3]. ...|
// CHECK: 00000020  {{(([A-F0-9]{2} ){16})}}|
// CHECK: 00000030  {{(([A-F0-9]{2} ){16})}}|

// CACHE-MISS: remark: cache miss for input
// CACHE-HIT: remark: replay output file '{{.*}}{{/|\\}}test.o': key 'llvmcas://{{.*}}'

/// Check that -write-output-hash-xattr with -cas-backend is an error.
// RUN: not %target-swift-frontend-plain -c -cache-compile-job \
// RUN:   -cas-path %t/cas %s -o %t/test.o @%t/MyApp.cmd \
// RUN:   -cas-backend -write-output-hash-xattr 2>&1 \
// RUN:   | %FileCheck --check-prefix=CAS-BACKEND %s
// CAS-BACKEND: error: '-cas-backend' is incompatible with '-write-output-hash-xattr'

/// Check that -write-output-hash-xattr with -cas-backend is an error.
// RUN: not %target-swift-frontend-plain -c -cache-compile-job \
// RUN:   -cas-path %t/cas %s -o %t/test.o @%t/MyApp.cmd \
// RUN:   -cas-emit-casid-file -write-output-hash-xattr 2>&1 \
// RUN:   | %FileCheck --check-prefix=CASID-FILE %s
// CASID-FILE: error: '-cas-emit-casid-file' is incompatible with '-write-output-hash-xattr'

func testFunc() {}
