// RUN: %empty-directory(%t/cache)
// RUN: %target-swift-frontend %s -I %S/Inputs -c -enable-experimental-cxx-interop -o %t/object.o
// RUN: %llvm-nm %t/object.o > %t/results.txt
// RUN: %target-swift-frontend %s -I %S/Inputs -c -enable-objc-interop -o %t/object.o
// RUN: %llvm-nm %t/object.o >> %t/results.txt
// RUN: cat %t/results.txt | %FileCheck %s

// REQUIRES: objc_interop

import CFAvailability

// Verify that this functions linkage name is the name with or without cxx interop enabled
public func useNSOption(foo param: StandardNSOption) {}

// CHECK: [[FUNC_LINKAGE_NAME:\$s.*useNSOption.*$]]
// CHECK: [[FUNC_LINKAGE_NAME]]
