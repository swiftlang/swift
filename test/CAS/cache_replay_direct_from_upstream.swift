// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %s -o %t/deps.json -cache-compile-job -cas-path %t/cas-plugin -cas-plugin-path %plugin(CASPluginTest)

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-string-processing-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-concurrency-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-parse-stdlib\"" >> %t/MyApp.cmd

// Get cache key.
// RUN: %swift-scan-test -action compute_cache_key_from_index -cas-path %t/cas-plugin -cas-plugin-path %plugin(CASPluginTest) -input 0 -- \
// RUN:   %target-swift-frontend-plain -cache-compile-job -Rcache-compile-job %s -emit-module -o %t/Test.swiftmodule -module-name Test \
// RUN:    -cas-path %t/cas-plugin -cas-plugin-path %plugin(CASPluginTest) @%t/MyApp.cmd > %t/key.casid

// Upload compiler results to upstream.
// RUN: %target-swift-frontend-plain -cache-compile-job -Rcache-compile-job %s -emit-module -o %t/Test.swiftmodule -module-name Test \
// RUN:  -cas-path %t/cas-plugin -cas-plugin-path %plugin(CASPluginTest) -cas-plugin-option upstream-path=%t/cas-upstream @%t/MyApp.cmd 2>&1 | %FileCheck --check-prefix=CACHE-MISS %s

// Download compiler results from upstream without having access to the input CAS tree.
// RUN: %swift-scan-test -action replay_result -id @%t/key.casid \
// RUN:   -cas-path %t/cas-plugin2 -cas-plugin-path %plugin(CASPluginTest) -cas-plugin-option upstream-path=%t/cas-upstream -- \
// RUN:    %target-swift-frontend-plain -cache-compile-job -Rcache-compile-job %s -emit-module -o %t/Test2.swiftmodule \
// RUN:     -module-name Test -cas-path %t/cas-plugin -cas-plugin-path %plugin(CASPluginTest) @%t/MyApp.cmd 2>&1 | %FileCheck --check-prefix=CACHE-HIT %s
// RUN: diff %t/Test.swiftmodule %t/Test2.swiftmodule

func testFunc() {}
#warning("some warning")

// CACHE-MISS: remark: cache miss for input
// CACHE-MISS-NOT: remark: replay output file
// CACHE-MISS: some warning

// CACHE-HIT: some warning
// CACHE-HIT: remark: replay output file
// CACHE-HIT-NOT: remark: cache miss for input
