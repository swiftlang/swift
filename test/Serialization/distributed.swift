// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)
// RUN: %target-swift-frontend -emit-module -o %t-scratch/def_async~partial.swiftmodule -primary-file %S/Inputs/def_distributed.swift -module-name def_async -enable-experimental-distributed
// RUN: %target-swift-frontend -merge-modules -emit-module -parse-as-library -enable-testing %t-scratch/def_distributed~partial.swiftmodule -module-name def_async -o %t/def_async.swiftmodule -enable-experimental-distributed
// RUN: %target-swift-frontend -typecheck -I%t -verify %s -verify-ignore-unknown -enable-experimental-distributed

// REQUIRES: concurrency
// REQUIRES: distributed

// TODO: fix this test, not sure what the issue is hm
// XFAIL: *

import def_distributed

func testDoSomethingDistributed() {
  // TODO: do the test
}
