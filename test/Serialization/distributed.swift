// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)
// RUN: %target-swift-frontend -emit-module -o %t-scratch/def_distributed~partial.swiftmodule -primary-file %S/Inputs/def_distributed.swift -module-name def_distributed  -disable-availability-checking
// RUN: %target-swift-frontend -merge-modules -emit-module -parse-as-library -enable-testing %t-scratch/def_distributed~partial.swiftmodule -module-name def_distributed -o %t/def_distributed.swiftmodule -disable-availability-checking
// RUN: %target-swift-frontend -typecheck -I%t -verify %s -verify-ignore-unknown -disable-availability-checking

// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import def_distributed

func testDoSomethingDistributed(system: LocalTestingDistributedActorSystem) {
  let _: DA = DA(actorSystem: system)
}
