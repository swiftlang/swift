// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-typecheck-verify-swift -target %target-swift-5.7-abi-triple -I %t
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

distributed actor DA: Comparable {}
// expected-error@-1 {{type 'DA' does not conform to protocol 'Comparable'}}
// expected-note@-2 {{automatic synthesis of 'Comparable' is not supported for distributed actor declarations}}
// expected-note@-3 {{add stubs for conformance}}
