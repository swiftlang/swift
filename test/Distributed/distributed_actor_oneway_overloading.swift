// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift

// 'func x() async' and 'func x() async oneway' are distinct overloads: if the
// 'oneway' modifier did not participate in the type identity they would be a
// redeclaration. This first RUN checks they type-check without a redeclaration
// error.
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature OnewayMethods -I %t 2>&1 %s

// The second RUN checks that the two overloads mangle differently: the 'oneway'
// variant carries the 'Yo' function-type flavor operator (right after the 'Ya'
// async operator), the plain 'async' one does not.
// RUN: %target-swift-frontend -emit-silgen -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature OnewayMethods -I %t %s -module-name main | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_OnewayMethods

import Distributed
import FakeDistributedActorSystems

// A plain actor: 'oneway' is permitted but inert here; it still participates in
// the type so it distinguishes the overloads and their mangled symbols.
actor Overloaded {
  func x() async {}
  func x() async oneway {}
}

// The 'async oneway' overload's symbol carries the 'Yo' operator immediately
// after 'Ya'; the plain 'async' overload's symbol does not.
// CHECK-DAG: sil hidden{{.*}} @{{.*}}1x{{.*}}YaYo{{.*}}F
// CHECK-DAG: sil hidden{{.*}} @{{.*}}1x{{[^Y]*}}Ya{{[^Y]*}}F
