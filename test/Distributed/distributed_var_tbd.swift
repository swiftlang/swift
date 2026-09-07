// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift

// The async function pointer of the distributed thunk synthesized for a
// 'distributed var' must be mangled off the thunk ('...SiyYaKFTETu') and not
// off the getter ('...SivgTETu'), otherwise TBDGen and IRGen disagree.
// '-enable-testing' is required to make the internal declarations visible to
// TBDGen, which is what SwiftPM does for debug builds.
// RUN: %target-swift-frontend -c -o /dev/null %s -module-name Library -swift-version 5 -target %target-swift-5.7-abi-triple -I %t -parse-as-library -enable-testing -validate-tbd-against-ir=all

// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

public distributed actor Pool {
  internal distributed var internalSize: Int { 0 }

  public distributed var publicSize: Int { 0 }

  public distributed func size() -> Int { 0 }
}
