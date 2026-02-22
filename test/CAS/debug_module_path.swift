// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %s -o %t/deps.json -cache-compile-job -cas-path %t/cas

// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps.json -o %t/Test.cmd

/// Check embedding CASID.
// RUN: %target-swift-frontend-plain -O \
// RUN:   -emit-ir -g -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -primary-file %s @%t/Test.cmd -module-name Test -debug-module-cache-key llvmcas://casid -o - | %FileCheck %s --check-prefix FAKEID

// FAKEID: !DIModule(scope: null, name: "Test", includePath: "llvmcas://casid")

/// Check embedding self key requires swiftmodule output.
// RUN: not %target-swift-frontend-plain -O \
// RUN:   -emit-ir -g -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -primary-file %s @%t/Test.cmd -module-name Test -debug-module-self-key -o - 2>&1 | %FileCheck %s --check-prefix ERROR

// ERROR: error: option '-debug-module-self-key' is missing required 'swiftmodule' output

/// Check embedding self key matches by appending the key to the end of .ll output and make sure the debug info CASID matches the cache key.
// RUN: %target-swift-frontend-plain -O -emit-module-path %t/Test.swiftmodule \
// RUN:   -emit-ir -g -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %s @%t/Test.cmd -module-name Test -debug-module-self-key -o %t/test.ll

// RUN: %swift-scan-test -action compute_cache_key_from_index -cas-path %t/cas -input 0 -- \
// RUN: %target-swift-frontend-plain -O -emit-module-path %t/Test.swiftmodule \
// RUN:   -emit-ir -g -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %s @%t/Test.cmd -module-name Test -debug-module-self-key -o %t/test.ll >> %t/test.ll

// RUN: %FileCheck --input-file %t/test.ll --check-prefix CASID %s

// CASID: !DIModule(scope: null, name: "Test", includePath: "llvmcas://[[CASID:.*]]")
// CASID: llvmcas://[[CASID]]
