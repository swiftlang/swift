// RUN: rm -rf %t && mkdir -p %t && %target-swift-frontend -c -primary-file %S/Inputs/ignore_type_placeholder.swift -emit-migrated-file-path %t/ignore_type_placeholder.swift.result -emit-remap-file-path %t/ignore_type_placeholder.swift.remap
// RUN: %FileCheck %s < %t/ignore_type_placeholder.swift.result
// CHECK-NOT: f(<#Void#>)
