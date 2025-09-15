// RUN: %target-typecheck-verify-swift

// Reduced from the distributed actors implementation. This didn't type check in 5.10
// but works now.

protocol DistributedActorSystem<SerializationRequirement> {
  associatedtype SerializationRequirement
}

protocol DistributedActor where SerializationRequirement == ActorSystem.SerializationRequirement {
  associatedtype ActorSystem: DistributedActorSystem
  associatedtype SerializationRequirement
}

class Worker<ActorSystem>: DistributedActor where ActorSystem: DistributedActorSystem, ActorSystem.SerializationRequirement == Int {}

struct FakeActorSystem: DistributedActorSystem {
  typealias SerializationRequirement = Int
}

print(Worker<FakeActorSystem>.SerializationRequirement.self)
