//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//


@asmname("GK_Swift_GKEntity_componentForClass")
internal func GK_Swift_GKEntity_componentForClass(
  self_: AnyObject,
  _ componentClass: AnyClass) -> AnyObject?

@available(iOS, introduced=9.0)
@available(OSX, introduced=10.11)
extension GKEntity {
  /// Returns the component instance of the indicated class contained by the
  /// entity. Returns nil if entity does not have this component.
  public func componentForClass<ComponentType : GKComponent>(
    componentClass: ComponentType.Type) -> ComponentType? {
    return GK_Swift_GKEntity_componentForClass(
      self, componentClass as AnyClass) as! ComponentType?
  }
}

@asmname("GK_Swift_GKStateMachine_stateForClass")
internal func GK_Swift_GKStateMachine_stateForClass(
  self_: AnyObject,
  _ stateClass: AnyClass) -> AnyObject?

@available(iOS, introduced=9.0)
@available(OSX, introduced=10.11)
extension GKStateMachine {
  /// Returns the state instance of the indicated class contained by the state
  /// machine. Returns nil if state machine does not have this state.
  public func stateForClass<StateType : GKState>(
    stateClass: StateType.Type) -> StateType? {
    return GK_Swift_GKStateMachine_stateForClass(
      self, stateClass as AnyClass) as! StateType?
  }
}
