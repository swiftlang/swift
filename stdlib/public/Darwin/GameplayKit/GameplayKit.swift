//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import GameplayKit // Clang module
import CoreGraphics
import simd


@available(iOS, introduced: 9.0)
@available(macOS, introduced: 10.11)
@available(tvOS, introduced: 9.0)
extension GKPath {
  /// Creates a path from an array of points
  /// - Parameter points: an array of simd.float2 points to make a path from
  /// - Parameter radius: radius of the path to create
  /// - Parameter cyclical: if the path is of a cycle that loops back on itself
  public convenience init(points: [simd.float2], radius: Float, cyclical: Bool) {
    var variablePoints = points
    self.init(__points: &variablePoints, count: points.count, radius: radius, cyclical: cyclical)
  }
}


@available(iOS, introduced: 10.0)
@available(macOS, introduced: 10.12)
@available(tvOS, introduced: 10.0)
extension GKPath {
  /// Creates a path from an array of points
  /// - Parameter points: an array of simd.float3 points to make a path from
  /// - Parameter radius: the radius of the path to create
  /// - Parameter cyclical: if the path is of a cycle that loops back on itself
  public convenience init(points: [simd.float3], radius: Float, cyclical: Bool) {
    var variablePoints = points
    self.init(__float3Points: &variablePoints, count: points.count, radius: radius, cyclical: cyclical)
  }
}

@available(iOS, introduced: 9.0)
@available(macOS, introduced: 10.11)
@available(tvOS, introduced: 9.0)
extension GKPolygonObstacle {
  /// Creates a polygon obstacle with an array of points.
  /// - Parameter points: array of points in counter-clockwise order that are the vertices of a convex polygon
  public convenience init(points: [simd.float2]) {
    var variablePoints = points
    self.init(__points: &variablePoints, count: points.count)
  }
}

@available(iOS, introduced: 9.0)
@available(macOS, introduced: 10.11)
@available(tvOS, introduced: 9.0)
extension GKEntity {
  /// Gets the component of the indicated class.  Returns nil if entity does not have this component
  /// - Parameter ofType: the type of the component you want to get
  public func component<ComponentType : GKComponent>(ofType componentClass: ComponentType.Type) -> ComponentType? {
     return self.__component(for: componentClass) as? ComponentType
  }
  /// Removes the component of the indicates class from this entity
  /// - Parameter ofType: the type of component you want to remove
  public func removeComponent<ComponentType : GKComponent>(ofType componentClass: ComponentType.Type) {
     self.__removeComponent(for: componentClass)
  }
}

@objc
internal protocol _SwiftGKStateMachineLike {
  @objc(stateForClass:)
  func state(forClass: AnyClass) -> AnyObject?
}

@available(iOS, introduced: 9.0)
@available(macOS, introduced: 10.11)
@available(tvOS, introduced: 9.0)
extension GKStateMachine {
  /// Gets the state of the indicated class.  Returns nil if the state machine does not have this state.
  /// - Parameter forClass: the type of the state you want to get
  public func state<StateType : GKState>(
    forClass stateClass: StateType.Type) -> StateType? {
    // FIXME: GameplayKit marked state(forClass:) unavailable, which means we
    // can't use it from SwiftPrivate. Bounce through perform(_:with:) instead.
    return self.perform(#selector(_SwiftGKStateMachineLike.state(forClass:)), with: stateClass)?.takeUnretainedValue() as! StateType?
  }
}

