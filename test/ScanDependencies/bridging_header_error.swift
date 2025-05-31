// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/module-cache %s -o %t/deps.json -import-objc-header %t/does-this-header-even-exist.h &> %t/diagnostic_output.txt
// RUN: cat %t/diagnostic_output.txt | %FileCheck %s
// CHECK: error: Bridging header dependency scan failure: error: no such file or directory: '{{.*}}does-this-header-even-exist.h'
