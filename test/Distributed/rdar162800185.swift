// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Make sure we can resolve the conformance to DistributedActor from a secondary.
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple %t/a.swift %t/b.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple -primary-file %t/a.swift %t/b.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple %t/a.swift -primary-file %t/b.swift

// REQUIRES: concurrency
// REQUIRES: distributed

//--- a.swift
import Distributed

distributed actor A<ActorSystem: DistributedActorSystem>: DistributedActor {}

//--- b.swift
import Distributed

func foo<T: DistributedActor>(_: T.Type) where T.ActorSystem == LocalTestingDistributedActorSystem {}
func bar() { foo(A.self) }
