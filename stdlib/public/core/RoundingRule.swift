//===--- RoundingRule.swift -----------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A rule for rounding a number to the closest allowed value.
public enum RoundingRule {
  /// Round to the closest allowed value; if two values are equally close, the
  /// one with greater magnitude is chosen.
  ///
  /// This rounding rule is also known as "schoolbook rounding." The following
  /// example shows the results of rounding numbers using this rule:
  ///
  ///     (5.2).rounded(.toNearestOrAwayFromZero)
  ///     // 5.0
  ///     (5.5).rounded(.toNearestOrAwayFromZero)
  ///     // 6.0
  ///     (-5.2).rounded(.toNearestOrAwayFromZero)
  ///     // -5.0
  ///     (-5.5).rounded(.toNearestOrAwayFromZero)
  ///     // -6.0
  ///
  /// This rule is equivalent to the C `round` function and implements the
  /// `roundToIntegralTiesToAway` operation defined by the [IEEE 754
  /// specification][spec].
  ///
  /// [spec]: http://ieeexplore.ieee.org/servlet/opac?punumber=4610933
  case toNearestOrAwayFromZero
  
  /// Round to the closest allowed value; if two values are equally close, the
  /// even one is chosen.
  ///
  /// This rounding rule is also known as "bankers rounding," and is the
  /// default IEEE 754 rounding mode for arithmetic. The following example
  /// shows the results of rounding numbers using this rule:
  ///
  ///     (5.2).rounded(.toNearestOrEven)
  ///     // 5.0
  ///     (5.5).rounded(.toNearestOrEven)
  ///     // 6.0
  ///     (4.5).rounded(.toNearestOrEven)
  ///     // 4.0
  ///
  /// This rule implements the `roundToIntegralTiesToEven` operation defined by
  /// the [IEEE 754 specification][spec].
  ///
  /// [spec]: http://ieeexplore.ieee.org/servlet/opac?punumber=4610933
  case toNearestOrEven
  
  /// Round to the closest allowed value that is greater than or equal to the
  /// source.
  ///
  /// The following example shows the results of rounding numbers using this
  /// rule:
  ///
  ///     (5.2).rounded(.up)
  ///     // 6.0
  ///     (5.5).rounded(.up)
  ///     // 6.0
  ///     (-5.2).rounded(.up)
  ///     // -5.0
  ///     (-5.5).rounded(.up)
  ///     // -5.0
  ///
  /// This rule is equivalent to the C `ceil` function and implements the
  /// `roundToIntegralTowardPositive` operation defined by the [IEEE 754
  /// specification][spec].
  ///
  /// [spec]: http://ieeexplore.ieee.org/servlet/opac?punumber=4610933
  case up
  
  /// Round to the closest allowed value that is less than or equal to the
  /// source.
  ///
  /// The following example shows the results of rounding numbers using this
  /// rule:
  ///
  ///     (5.2).rounded(.down)
  ///     // 5.0
  ///     (5.5).rounded(.down)
  ///     // 5.0
  ///     (-5.2).rounded(.down)
  ///     // -6.0
  ///     (-5.5).rounded(.down)
  ///     // -6.0
  ///
  /// This rule is equivalent to the C `floor` function and implements the
  /// `roundToIntegralTowardNegative` operation defined by the [IEEE 754
  /// specification][spec].
  ///
  /// [spec]: http://ieeexplore.ieee.org/servlet/opac?punumber=4610933
  case down
  
  /// Round to the closest allowed value whose magnitude is less than or equal
  /// to that of the source.
  ///
  /// The following example shows the results of rounding numbers using this
  /// rule:
  ///
  ///     (5.2).rounded(.towardZero)
  ///     // 5.0
  ///     (5.5).rounded(.towardZero)
  ///     // 5.0
  ///     (-5.2).rounded(.towardZero)
  ///     // -5.0
  ///     (-5.5).rounded(.towardZero)
  ///     // -5.0
  ///
  /// This rule is equivalent to the C `trunc` function and implements the
  /// `roundToIntegralTowardZero` operation defined by the [IEEE 754
  /// specification][spec].
  ///
  /// [spec]: http://ieeexplore.ieee.org/servlet/opac?punumber=4610933
  case towardZero
  
  /// Round to the closest allowed value whose magnitude is greater than or
  /// equal to that of the source.
  ///
  /// The following example shows the results of rounding numbers using this
  /// rule:
  ///
  ///     (5.2).rounded(.awayFromZero)
  ///     // 6.0
  ///     (5.5).rounded(.awayFromZero)
  ///     // 6.0
  ///     (-5.2).rounded(.awayFromZero)
  ///     // -6.0
  ///     (-5.5).rounded(.awayFromZero)
  ///     // -6.0
  case awayFromZero
}
