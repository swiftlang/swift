// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import %s -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas -cxx-interoperability-mode=default -clang-target %target-triple

// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps.json -o %t/MyApp.cmd

// RUN: %target-swift-frontend-plain -c -o %t/test.o -cache-compile-job -cas-path %t/cas -swift-version 5 -module-name Test -cxx-interoperability-mode=default -clang-target %target-triple -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import %s @%t/MyApp.cmd 2>&1 | %FileCheck %s --allow-empty

// CHECK-NOT: libcxxshim.modulemap' will be ignored

public func test() {}
