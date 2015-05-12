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

@exported import GameplayKit

@asmname("GP_Swift_GPEntity_componentForClass")
internal func GP_Swift_GPEntity_componentForClass(
  self_: AnyObject,
  _ componentClass: AnyClass) -> GPComponent?

extension GPEntity {
  /// Returns the component instance of the indicated class contained by the
  /// entity. Returns nil if entity does not have this component.
  @available(iOS, introduced=9.0)
  @available(OSX, introduced=10.11)
  public func componentForClass<ComponentType : GPComponent>(
    componentClass: ComponentType.Type) -> ComponentType? {
    return GP_Swift_GPEntity_componentForClass(
      self, componentClass as AnyClass) as! ComponentType?
  }
}

@asmname("GP_Swift_GPStateMachine_stateForClass")
internal func GP_Swift_GPStateMachine_stateForClass(
  self_: AnyObject,
  _ stateClass: AnyClass) -> GPState?

extension GPStateMachine {
  /// Returns the state instance of the indicated class contained by the state
  /// machine. Returns nil if state machine does not have this state.
  @available(iOS, introduced=9.0)
  @available(OSX, introduced=10.11)
  public func stateForClass<StateType : GPState>(
    stateClass: StateType.Type) -> StateType? {
    return GP_Swift_GPStateMachine_stateForClass(
      self, stateClass as AnyClass) as! StateType?
  }
}
