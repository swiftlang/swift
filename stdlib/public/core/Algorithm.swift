//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// Returns the lesser of two comparable values.
///
/// - Parameters:
///   - x: A value to compare.
///   - y: Another value to compare.
/// - Returns: The lesser of `x` and `y`. If `x` is equal to `y`, returns `x`.
@inlinable // protocol-only
public func min<T: Comparable>(_ x: T, _ y: T) -> T {
  // In case `x == y` we pick `x`.
  // This preserves any pre-existing order in case `T` has identity,
  // which is important for e.g. the stability of sorting algorithms.
  // `(min(x, y), max(x, y))` should return `(x, y)` in case `x == y`.
  return y < x ? y : x
}

/// Returns the least argument passed.
///
/// - Parameters:
///   - x: A value to compare.
///   - y: Another value to compare.
///   - z: A third value to compare.
///   - rest: Zero or more additional values.
/// - Returns: The least of all the arguments. If there are multiple equal
///   least arguments, the result is the first one.
@inlinable // protocol-only
public func min<T: Comparable>(_ x: T, _ y: T, _ z: T, _ rest: T...) -> T {
  var minValue = min(min(x, y), z)
  // In case `value == minValue`, we pick `minValue`. See min(_:_:).
  for value in rest where value < minValue {
    minValue = value
  }
  return minValue
}

/// Returns the greater of two comparable values.
///
/// - Parameters:
///   - x: A value to compare.
///   - y: Another value to compare.
/// - Returns: The greater of `x` and `y`. If `x` is equal to `y`, returns `y`.
@inlinable // protocol-only
public func max<T: Comparable>(_ x: T, _ y: T) -> T {
  // In case `x == y`, we pick `y`. See min(_:_:).
  return y >= x ? y : x
}

/// Returns the greatest argument passed.
///
/// - Parameters:
///   - x: A value to compare.
///   - y: Another value to compare.
///   - z: A third value to compare.
///   - rest: Zero or more additional values.
/// - Returns: The greatest of all the arguments. If there are multiple equal
///   greatest arguments, the result is the last one.
@inlinable // protocol-only
public func max<T: Comparable>(_ x: T, _ y: T, _ z: T, _ rest: T...) -> T {
  var maxValue = max(max(x, y), z)
  // In case `value == maxValue`, we pick `value`. See min(_:_:).
  for value in rest where value >= maxValue {
    maxValue = value
  }
  return maxValue
}

/// Returns all mutually equal values ​​in the input values
///
/// - Parameters:
///   - value: Many values to **compare**.
///   - orElse: **Default** output value.
///
/// - Returns: The values of all two or more **repeated** *values* will be return. **One or more** values **returned**, the **return** values **not** be repeated, and the **return** values arranged in the order of input.
///
/// - Note:
/// If **none** of the values in the input *value* are equal to other values, *orElse* can be return. There must be **two** or more input *value*.
///
/// - Complexity: O( *n^3* )
@inlinable // protocol-only
public func equal<T: Comparable>(_ value: T..., default orElse: T) -> [T] { // Create a function that contains a generic type T, two values, and outputs generic type T.
    
    var resultValue: [T] = [] // All variables with equal values.
    var marryCount: Int = 0 // Stores the number of repetitions of a value.
    
    for primitiveValue in value {
        for contrastValue in value {
            // Loop through all values, matching two values ​​against each other. Complexity is O(n^2).
            
            if primitiveValue == contrastValue { // When two values ​​match...
                if marryCount > 0 { // When the number of matches with a value reaches 2...
                    if !resultValue.contains(primitiveValue) { // To ensure that the return result is not repeated, check whether the result has been stored... Complexity is O(n).
                        resultValue.append(primitiveValue) // Store results!
                    }
                } else {
                    marryCount += 1 // Increase the secondary value an equal number of times.
                }
            }
        }
        
        marryCount = 0 // Initialize the value of the loop to prevent previous data from affecting current data.
    }
    
    return resultValue == [] ? [orElse] : resultValue // Output the result. If the result is empty, output the default value.
}
