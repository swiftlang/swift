// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Make sure we can resolve the conformance to DistributedActor from a secondary.
// RUN: %target-swift-frontend -c -module-name main -target %target-swift-5.7-abi-triple %t/a.swift %t/b.swift -o %t/out.o
// RUN: %target-swift-frontend -c -module-name main -target %target-swift-5.7-abi-triple -primary-file %t/a.swift %t/b.swift -o %t/out.o
// RUN: %target-swift-frontend -c -module-name main -target %target-swift-5.7-abi-triple %t/a.swift -primary-file %t/b.swift -o %t/out.o

// REQUIRES: concurrency
// REQUIRES: distributed

//--- a.swift
import Distributed

distributed actor A<ActorSystem: DistributedActorSystem> {}

//--- b.swift
import Distributed

func foo<T: DistributedActor>(_: T.Type) where T.ActorSystem == LocalTestingDistributedActorSystem {}
func bar() { foo(A.self) }
