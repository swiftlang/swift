// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %llvm-profdata merge %t/empty.proftext -o %t/default.profdata

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/main.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas -profile-use=%t/default.profdata \
// RUN:   -scanner-prefix-map-paths %t /^tmp

// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps.json -o %t/Test.cmd

// RUN: %target-swift-frontend-plain -module-name Test -O \
// RUN:   -c -o %t/main.o -cache-compile-job -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   /^tmp/main.swift @%t/Test.cmd -profile-use=/^tmp/default.profdata 2>&1 | %FileCheck %s --allow-empty

/// Missing profile data is manifested as a note, not an error. Check word inside diagnositics.
// CHECK-NOT: failed

//--- empty.proftext
//--- main.swift
func main() {}
