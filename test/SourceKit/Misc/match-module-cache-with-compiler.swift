// This test ensures a certain set of arguments allows both compiler and sourcekitd invocations to share the same module cache.
// NOTE: Do not change this test without a review from @akyrtzi

// RUN: %empty-directory(%t)

// RUN: %swiftc_driver -emit-module -emit-module-path themod.swiftmodule -Xfrontend -experimental-skip-all-function-bodies -module-name themod -module-cache-path %t/mcp -sdk %sdk -swift-version 5 -I %S/Inputs/mymod -Xfrontend -experimental-allow-module-with-compiler-errors -Xcc -D__INDEX_BUILD__=1 -D__INDEX_BUILD__ -Xcc -Xclang -Xcc -fallow-pcm-with-compiler-errors -Xcc -Xclang -Xcc -fmodule-format=raw -Xcc -Xclang -Xcc -detailed-preprocessing-record %s
// RUN: %swiftc_driver -index-file -index-file-path %s -index-store-path %t/idx -index-ignore-system-modules -module-name themod -module-cache-path %t/mcp -sdk %sdk -swift-version 5 -I %S/Inputs/mymod -Xfrontend -experimental-allow-module-with-compiler-errors -Xcc -D__INDEX_BUILD__=1 -D__INDEX_BUILD__ -Xcc -Xclang -Xcc -fallow-pcm-with-compiler-errors -Xcc -Xclang -Xcc -fmodule-format=raw -Xcc -Xclang -Xcc -detailed-preprocessing-record %s
// RUN: %sourcekitd-test -req=sema %s -- -module-name themod -module-cache-path %t/mcp -sdk %sdk -swift-version 5 -I %S/Inputs/mymod -Xfrontend -experimental-allow-module-with-compiler-errors -Xcc -D__INDEX_BUILD__=1 -D__INDEX_BUILD__ -Xcc -Xclang -Xcc -fallow-pcm-with-compiler-errors -Xcc -Xclang -Xcc -fmodule-format=raw -Xcc -Xclang -Xcc -detailed-preprocessing-record %s
// RUN: find %t/mcp -name "mymod-*.pcm" | count 1

import mymod
