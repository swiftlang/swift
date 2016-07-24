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

@_exported import Intents
import Foundation

#if os(iOS)
@available(iOS 10.0, *)
extension INStartWorkoutIntent {
  @nonobjc
  public convenience init(
      workoutName: INSpeakableString? = nil,
      goalValue: Double? = nil,
      workoutGoalUnitType: INWorkoutGoalUnitType = .unknown,
      workoutLocationType: INWorkoutLocationType = .unknown,
      isOpenEnded: Bool? = nil
  ) {
    self.init(__workoutName: workoutName,
      goalValue: goalValue.map { NSNumber(value: $0) },
      workoutGoalUnitType: workoutGoalUnitType,
      workoutLocationType: workoutLocationType,
      isOpenEnded: isOpenEnded.map { NSNumber(value: $0) })
  }

  @nonobjc
  public final var goalValue: Double? {
    return __goalValue?.doubleValue
  }

  @nonobjc
  public final var isOpenEnded: Bool? {
    return __isOpenEnded?.boolValue
  }
}
#endif
