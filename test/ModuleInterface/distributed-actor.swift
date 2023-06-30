// REQUIRES: rdar111572280

// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library
// RUN: %FileCheck %s < %t/Library.swiftinterface

// REQUIRES: distributed

import Distributed

// CHECK:      #if compiler(>=5.3) && $Actors
// CHECK-NEXT: @available(macOS 13.0, iOS 16.0, watchOS 9.0, tvOS 16.0, *)
// CHECK-NEXT: distributed public actor DA {
@available(SwiftStdlib 5.7, *)
public distributed actor DA {
  // CHECK: @_compilerInitialized nonisolated final public let id: Distributed.LocalTestingDistributedActorSystem.ActorID
  // CHECK: nonisolated final public let actorSystem: Library.DA.ActorSystem
  // CHECK: public typealias ActorSystem = Distributed.LocalTestingDistributedActorSystem
  public typealias ActorSystem = LocalTestingDistributedActorSystem

  // CHECK:       public static func resolve(id: Distributed.LocalTestingDistributedActorSystem.ActorID, using system: Library.DA.ActorSystem) throws -> Library.DA
  // CHECK:       public typealias ID = Distributed.LocalTestingDistributedActorSystem.ActorID
  // CHECK:       public typealias SerializationRequirement = any Swift.Decodable & Swift.Encodable
  // CHECK:       {{@objc deinit|deinit}}
  // CHECK:       nonisolated public var hashValue: Swift.Int {
  // CHECK-NEXT:    get
  // CHECK-NEXT:  }
  // CHECK:       public init(actorSystem system: Library.DA.ActorSystem)
  // CHECK:       @available(iOS 16.0, tvOS 16.0, watchOS 9.0, macOS 13.0, *)
  // CHECK-NEXT:  @_semantics("defaultActor") nonisolated final public var unownedExecutor: _Concurrency.UnownedSerialExecutor {
  // CHECK-NEXT:    get
  // CHECK-NEXT:  }
}
// CHECK: #endif


// CHECK:       #if compiler(>=5.3) && $Actors
// CHECK-NEXT:  @available(macOS 13.0, iOS 16.0, watchOS 9.0, tvOS 16.0, *)
// CHECK-NEXT:  extension Library.DA : Distributed.DistributedActor {}
// CHECK-NEXT:  #endif
// CHECK:       #if compiler(>=5.3) && $Actors
// CHECK-NEXT:  @available(macOS 13.0, iOS 16.0, watchOS 9.0, tvOS 16.0, *)
// CHECK-NEXT:  extension Library.DA : Swift.Encodable {}
// CHECK-NEXT:  #endif
// CHECK:       #if compiler(>=5.3) && $Actors
// CHECK-NEXT:  @available(macOS 13.0, iOS 16.0, watchOS 9.0, tvOS 16.0, *)
// CHECK-NEXT:  extension Library.DA : Swift.Decodable {}
// CHECK-NEXT:  #endif
