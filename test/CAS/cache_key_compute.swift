// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/a.swift %t/b.swift -o %t/deps.json -cache-compile-job -cas-path %t/cas

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-string-processing-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-concurrency-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-parse-stdlib\"" >> %t/MyApp.cmd

/// Doesn't run because the command doesn't have CAS enabled.
// RUN: not %cache-tool -cas-path %t/cas -cache-tool-action print-base-key -- \
// RUN:   %target-swift-frontend %t/a.swift -c @%t/MyApp.cmd 2>&1 | \
// RUN:   %FileCheck %s --check-prefix=NO-CAS

// NO-CAS: Requested command-line arguments do not enable CAS

/// Check few working cases.
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-base-key -- \
// RUN:   %target-swift-frontend -cache-compile-job %t/a.swift -c @%t/MyApp.cmd > %t1.casid
/// A different CAS doesn't affect base key.
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-base-key -- \
// RUN:   %target-swift-frontend -cache-compile-job %t/a.swift -c @%t/MyApp.cmd -cas-path %t > %t2.casid
/// Output path doesn't affect base key.
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-base-key -- \
// RUN:   %target-swift-frontend -cache-compile-job %t/a.swift -c @%t/MyApp.cmd -o %t/test.o > %t3.casid
/// Add -D will change.
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-base-key -- \
// RUN:   %target-swift-frontend -cache-compile-job %t/a.swift -c @%t/MyApp.cmd -DTEST > %t4.casid

// RUN: diff %t1.casid %t2.casid
// RUN: diff %t1.casid %t3.casid
// RUN: not diff %t1.casid %t4.casid

/// Check filelist option.
// RUN: echo "%t/a.swift" > %t/filelist-1
// RUN: echo "%t/a.swift" > %t/filelist-2
// RUN: echo "%t/b.swift" > %t/filelist-3
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-base-key -- \
// RUN:   %target-swift-frontend -cache-compile-job -filelist %t/filelist-1 -c @%t/MyApp.cmd > %t5.casid
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-base-key -- \
// RUN:   %target-swift-frontend -cache-compile-job -filelist %t/filelist-2 -c @%t/MyApp.cmd > %t6.casid
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-base-key -- \
// RUN:   %target-swift-frontend -cache-compile-job -filelist %t/filelist-3 -c @%t/MyApp.cmd > %t7.casid
// RUN: diff %t5.casid %t6.casid
// RUN: not diff %t5.casid %t7.casid

/// Check switching CAS plugin path.
// RUN: %cache-tool -cas-path %t/cas -cas-plugin-path %llvm_libs_dir/libCASPluginTest%llvm_plugin_ext -cache-tool-action print-base-key -- \
// RUN:   %target-swift-frontend -cache-compile-job %t/a.swift -c @%t/MyApp.cmd -cas-path %t/cas -cas-plugin-path %llvm_libs_dir/libCASPluginTest%llvm_plugin_ext > %t8.casid
// RUN: ln -s %llvm_libs_dir/libCASPluginTest%llvm_plugin_ext %t/libCASPluginTest%llvm_plugin_ext
// RUN: %cache-tool -cas-path %t/cas -cas-plugin-path %t/libCASPluginTest%llvm_plugin_ext -cache-tool-action print-base-key -- \
// RUN:   %target-swift-frontend -cache-compile-job %t/a.swift -c @%t/MyApp.cmd -cas-path %t/cas -cas-plugin-path %t/libCASPluginTest%llvm_plugin_ext > %t9.casid
// RUN: diff %t8.casid %t9.casid

/// Test output keys.
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-output-keys -- \
// RUN:   %target-swift-frontend -cache-compile-job %t/a.swift -emit-module -c -emit-dependencies \
// RUN:   -emit-tbd -emit-tbd-path %t/test.tbd -o %t/test.o @%t/MyApp.cmd | %FileCheck %s

/// Test plugin CAS.
// RUN: %target-swift-frontend -scan-dependencies -module-name Test -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/a.swift -o %t/plugin_deps.json -cache-compile-job -cas-path %t/cas -cas-plugin-path %llvm_libs_dir/libCASPluginTest%llvm_plugin_ext \
// RUN:   -cas-plugin-option first-prefix=myfirst-

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/plugin_deps.json > %t/plugin_map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/plugin_map.json > %t/map.casid

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/plugin_deps.json Test > %t/plugin_MyApp.cmd
// RUN: %cache-tool -cas-path %t/cas -cas-plugin-path %llvm_libs_dir/libCASPluginTest%llvm_plugin_ext \
// RUN:   -cas-plugin-option first-prefix=myfirst- -cache-tool-action print-output-keys -- \
// RUN:   %target-swift-frontend -cache-compile-job %t/a.swift -emit-module -c -emit-dependencies \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -emit-tbd -emit-tbd-path %t/test.tbd -o %t/test.o @%t/plugin_MyApp.cmd | %FileCheck %s --check-prefix=CHECK --check-prefix=PLUGIN

// CHECK: "Input": "{{.*}}{{/|\\}}a.swift"
// CHECK-NEXT: "CacheKey"
// PLUGIN-SAME: myfirst-llvmcas://

// CHECK-NEXT: "Outputs": [
// CHECK-NEXT: {
// CHECK-NEXT: "Kind": "object",
// CHECK-NEXT: "Path":
// CHECK-SAME: test.o
// CHECK-NEXT: },
// CHECK-NEXT: {
// CHECK-NEXT: "Kind": "swiftmodule",
// CHECK-NEXT: "Path":
// CHECK-SAME: test.swiftmodule
// CHECK-NEXT: },
// CHECK-NEXT: {
// CHECK-NEXT: "Kind": "dependencies",
// CHECK-NEXT: "Path":
// CHECK-SAME: test.d
// CHECK-NEXT: },
// CHECK-NEXT: {
// CHECK-NEXT: "Kind": "tbd",
// CHECK-NEXT: "Path":
// CHECK-SAME: test.tbd
// CHECK-NEXT: },
// CHECK-NEXT: {
// CHECK-NEXT: "Kind": "cached-diagnostics",
// CHECK-NEXT: "Path": "<cached-diagnostics>"
// CHECK-NEXT: }
// CHECK-NEXT: ]

//--- a.swift
func a() {}

//--- b.swift
func b() {}
