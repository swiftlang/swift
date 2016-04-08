//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import GameplayKit

@warn_unused_result
@_silgen_name("GK_Swift_GKEntity_componentForClass")
internal func GK_Swift_GKEntity_componentForClass(
  _ self_: AnyObject,
  _ componentClass: AnyObject) -> AnyObject?

@available(iOS, introduced: 9.0)
@available(OSX, introduced: 10.11)
@available(tvOS, introduced: 9.0)
extension GKEntity {
  /// Returns the component instance of the indicated class contained by the
  /// entity. Returns nil if entity does not have this component.
  @warn_unused_result
  public func componentForClass<ComponentType : GKComponent>(
    _ componentClass: ComponentType.Type) -> ComponentType? {
    return GK_Swift_GKEntity_componentForClass(
      self, componentClass) as! ComponentType?
  }
}

@warn_unused_result
@_silgen_name("GK_Swift_GKStateMachine_stateForClass")
internal func GK_Swift_GKStateMachine_stateForClass(
  _ self_: AnyObject,
  _ stateClass: AnyObject) -> AnyObject?

@available(iOS, introduced: 9.0)
@available(OSX, introduced: 10.11)
@available(tvOS, introduced: 9.0)
extension GKStateMachine {
  /// Returns the state instance of the indicated class contained by the state
  /// machine. Returns nil if state machine does not have this state.
  @warn_unused_result
  public func stateForClass<StateType : GKState>(
    _ stateClass: StateType.Type) -> StateType? {
    return GK_Swift_GKStateMachine_stateForClass(
      self, stateClass) as! StateType?
  }
}
