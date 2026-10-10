// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend-plain -scan-dependencies -module-name Test -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/a.swift %t/b.swift %t/c.swift -o %t/deps.json -cache-compile-job -cas-path %t/cas

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-string-processing-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-concurrency-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-parse-stdlib\"" >> %t/MyApp.cmd

/// Computing the keys for multiple inputs in one call returns the same keys
/// as computing them one by one, in the order of the requested indices.
// RUN: %swift-scan-test -action compute_cache_key_from_index -cas-path %t/cas -input 0 -- \
// RUN:   %target-swift-frontend-plain -cache-compile-job -c @%t/MyApp.cmd \
// RUN:   -primary-file %t/a.swift %t/b.swift -primary-file %t/c.swift -o %t/a.o -o %t/c.o > %t/expected.txt
// RUN: %swift-scan-test -action compute_cache_key_from_index -cas-path %t/cas -input 2 -- \
// RUN:   %target-swift-frontend-plain -cache-compile-job -c @%t/MyApp.cmd \
// RUN:   -primary-file %t/a.swift %t/b.swift -primary-file %t/c.swift -o %t/a.o -o %t/c.o >> %t/expected.txt
// RUN: %swift-scan-test -action compute_cache_keys_from_indices -cas-path %t/cas -input 0,2 -- \
// RUN:   %target-swift-frontend-plain -cache-compile-job -c @%t/MyApp.cmd \
// RUN:   -primary-file %t/a.swift %t/b.swift -primary-file %t/c.swift -o %t/a.o -o %t/c.o > %t/keys.txt
// RUN: diff %t/expected.txt %t/keys.txt
// RUN: sed -n 1p %t/keys.txt > %t/key0.txt
// RUN: sed -n 2p %t/keys.txt > %t/key2.txt
// RUN: not diff %t/key0.txt %t/key2.txt

/// Same for a file list.
// RUN: echo "%t/a.swift" > %t/filelist
// RUN: echo "%t/b.swift" >> %t/filelist
// RUN: echo "%t/c.swift" >> %t/filelist
// RUN: %swift-scan-test -action compute_cache_key_from_index -cas-path %t/cas -input 2 -- \
// RUN:   %target-swift-frontend-plain -cache-compile-job -c @%t/MyApp.cmd \
// RUN:   -filelist %t/filelist -primary-file %t/b.swift -primary-file %t/c.swift > %t/expected-filelist.txt
// RUN: %swift-scan-test -action compute_cache_key_from_index -cas-path %t/cas -input 1 -- \
// RUN:   %target-swift-frontend-plain -cache-compile-job -c @%t/MyApp.cmd \
// RUN:   -filelist %t/filelist -primary-file %t/b.swift -primary-file %t/c.swift >> %t/expected-filelist.txt
// RUN: %swift-scan-test -action compute_cache_keys_from_indices -cas-path %t/cas -input 2,1 -- \
// RUN:   %target-swift-frontend-plain -cache-compile-job -c @%t/MyApp.cmd \
// RUN:   -filelist %t/filelist -primary-file %t/b.swift -primary-file %t/c.swift > %t/keys-filelist.txt
// RUN: diff %t/expected-filelist.txt %t/keys-filelist.txt

/// Errors are reported.
// RUN: not %swift-scan-test -action compute_cache_keys_from_indices -cas-path %t/cas -input 0 -- \
// RUN:   %target-swift-frontend-plain -cache-compile-job -c @%t/MyApp.cmd \
// RUN:   -filelist %t/missing-filelist -primary-file %t/a.swift 2>&1 | %FileCheck %s --check-prefix=ERROR

// ERROR: {{[Nn]}}o such file or directory

//--- a.swift
func a() {}

//--- b.swift
func b() {}

//--- c.swift
func c() {}
