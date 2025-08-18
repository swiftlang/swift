//===----------------------------------------------------------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

/// Writes the textual representations of the given items into the standard
/// output.
///
/// You can pass zero or more items to the `print(_:separator:terminator:)`
/// function. The textual representation for each item is the same as that
/// obtained by calling `String(item)`. The following example prints a string,
/// a closed range of integers, and a group of floating-point values to
/// standard output:
///
///     print("One two three four five")
///     // Prints "One two three four five"
///
///     print(1...5)
///     // Prints "1...5"
///
///     print(1.0, 2.0, 3.0, 4.0, 5.0)
///     // Prints "1.0 2.0 3.0 4.0 5.0"
///
/// To print the items separated by something other than a space, pass a string
/// as the `separator` parameter.
///
///     print("Red", "Green", "Blue", separator: " & ")
///     // Prints "Red & Green & Blue"
///
/// The output from each call to `print(_:separator:terminator:)` includes a
/// newline by default. To print the items without a trailing newline, pass an
/// empty string as the `terminator` parameter.
///
///     for n in 1...5 {
///         print(n, terminator: "")
///     }
///     // Prints "12345"
///
/// - Parameters:
///   - items: Zero or more items to print.
///   - separator: A string to print between each item. The default is a single
///     space (`" "`).
///   - terminator: The string to print after all items have been printed. The
///     default is a newline (`"\n"`).
public func print(_ items: Any..., separator: String = " ", terminator: String = "\n") {
  print(items, separator: separator, terminator: terminator, to: &_Stdout)
}

/// Writes the textual representations of the given items into the given output
/// stream.
///
/// You can pass zero or more items to the `print(_:separator:terminator:to:)`
/// function. The textual representation for each item is the same as that
/// obtained by calling `String(item)`. The following example prints a closed
/// range of integers to a string:
///
///     var range = ""
///     print(1...5, to: &range)
///     // range == "1...5\n"
///
/// To print the items separated by something other than a space, pass a string
/// as the `separator` parameter.
///
///     var colors = ""
///     print("Red", "Green", "Blue", separator: " & ", to: &colors)
///     // colors == "Red & Green & Blue\n"
///
/// The output from each call to `print(_:separator:terminator:to:)` includes a
/// newline by default. To print the items without a trailing newline, pass an
/// empty string as the `terminator` parameter.
///
///     var numbers = ""
///     for n in 1...5 {
///         print(n, terminator: "", to: &numbers)
///     }
///     // numbers == "12345"
///
/// - Parameters:
///   - items: Zero or more items to print.
///   - separator: A string to print between each item. The default is a single
///     space (`" "`).
///   - terminator: The string to print after all items have been printed. The
///     default is a newline (`"\n"`).
///   - output: An output stream to receive the text representation of the
///     items.
public func print<Target>(_ items: Any..., separator: String = " ", terminator: String = "\n", to output: inout Target) where Target : TextOutputStream {
  print(items, separator: separator, terminator: terminator, to: &output)
}

/// Writes the textual representations of the given items most efficiently
/// possible into the given output stream.
///
/// - Parameters:
///   - items: The items to print.
///   - separator: A string to print between each item.
///   - terminator: The string to print after all items have been printed.
///   - output: An output stream to receive the text representation of the
///     items.
public func print<Target>(_ items: [Any], separator: String, terminator: String, to output: inout Target) where Target : TextOutputStream {
  var first = true
  for item in items {
    if first {
      first = false
    } else {
      output.write(separator)
    }
    
    if let textOutputStreamable = item as? TextOutputStreamable {
      textOutputStreamable.write(to: &output)
    } else if let customStringConvertible = item as? CustomStringConvertible {
      output.write(customStringConvertible.description)
    } else {
      output.write(String(describing: item))
    }
  }
  output.write(terminator)
}

/// Writes the textual representations of the given items into the standard
/// output, followed by a newline.
///
/// You can pass zero or more items to the `print(_:)` function. The textual
/// representation for each item is the same as that obtained by calling
/// `String(item)`. The following example prints a string, a closed range of
/// integers, and a group of floating-point values to standard output:
///
///     print("One two three four five")
///     // Prints "One two three four five"
///
///     print(1...5)
///     // Prints "1...5"
///
///     print(1.0, 2.0, 3.0, 4.0, 5.0)
///     // Prints "1.0 2.0 3.0 4.0 5.0"
///
/// - Parameter items: Zero or more items to print.
public func print(_ items: Any...) {
  print(items, separator: " ", terminator: "\n", to: &_Stdout)
}

/// Writes the textual representations of the given items most efficiently
/// possible into the standard output.
///
/// - Parameter items: The items to print.
public func print(_ items: [Any]) {
  print(items, separator: " ", terminator: "\n", to: &_Stdout)
}

// MARK: - TextOutputStream protocol

/// A type that can be the target of text-streaming operations.
///
/// You can send the output of the standard library's `print(_:to:)` and
/// `dump(_:to:)` functions to any type that conforms to the
/// `TextOutputStream` protocol. Swift's `String` type conforms to
/// `TextOutputStream` already, so you can capture the output from `print(_:to:)`
/// and `dump(_:to:)` in a string instead of sending it to standard output.
///
/// Conforming to the TextOutputStream Protocol
/// ===========================================
///
/// To make your custom type conform to `TextOutputStream`, implement the
/// required `write(_:)` method. Functions that use a `TextOutputStream` target
/// may call `write(_:)` multiple times per output operation.
public protocol TextOutputStream {
  /// Appends the given string to the stream.
  mutating func write(_ string: String)
}

/// A type with a customized textual representation suitable for debugging
/// purposes.
///
/// Swift provides a default debugging textual representation for any type.
/// That default representation is used by the `String(reflecting:)` initializer
/// and the `debugPrint(_:)` function for types that don't provide their own.
/// To customize that representation, make your type conform to the
/// `CustomDebugStringConvertible` protocol.
///
/// Because the `String(reflecting:)` initializer works for instances of *any*
/// type, returning an instance's `debugDescription` if the value conforms to
/// `CustomDebugStringConvertible` and a default representation otherwise, you
/// can use it to debug the state of any value you create.
///
/// Consider this example of a shopping list made up of `PurchaseItem`
/// instances. Neither `PurchaseItem` nor the nested `Variety` enumeration
/// initially conforms to `CustomDebugStringConvertible`.
///
///     struct PurchaseItem {
///         let name: String
///         let price: Double
///         let variety: Variety
///
///         enum Variety {
///             case regularSize
///             case kingSize
///         }
///     }
///
///     let items = [
///         PurchaseItem(name: "Cereal", price: 3.99, variety: .regularSize),
///         PurchaseItem(name: "Milk", price: 2.95, variety: .kingSize)
///     ]
///
/// Here's what you'll see when calling the `String(reflecting:)` initializer
/// on the `items` array:
///
///     print(String(reflecting: items))
///     // Prints:
///     // [__lldb_expr_33.PurchaseItem(name: "Cereal", price: 3.99, variety:
///     //   __lldb_expr_33.PurchaseItem.Variety.regularSize),
///     //  __lldb_expr_33.PurchaseItem(name: "Milk", price: 2.95, variety:
///     //   __lldb_expr_33.PurchaseItem.Variety.kingSize)]
///
/// After implementing `CustomDebugStringConvertible` conformance for both the
/// `PurchaseItem` type and its `Variety` enumeration, the same function call
/// results in a much simpler output:
///
///     extension PurchaseItem: CustomDebugStringConvertible {
///         var debugDescription: String {
///             return "\(name): $\(price) (\(variety.debugDescription))"
///         }
///     }
///
///     extension PurchaseItem.Variety: CustomDebugStringConvertible {
///         var debugDescription: String {
///             switch self {
///             case .regularSize: return "R"
///             case .kingSize: return "K"
///             }
///         }
///     }
///
///     print(String(reflecting: items))
///     // Prints: [Cereal: $3.99 (R), Milk: $2.95 (K)]
///
/// Conforming to CustomDebugStringConvertible
/// ==========================================
///
/// Add `CustomDebugStringConvertible` conformance to your custom types by
/// defining a `debugDescription` property.
public protocol CustomDebugStringConvertible {
  /// A textual representation of this instance, suitable for debugging.
  ///
  /// Calling this property directly is discouraged. Instead, convert an
  /// instance of any type to a string by using the `String(reflecting:)`
  /// initializer. This initializer works with any type, and uses the custom
  /// `debugDescription` property for types that conform to
  /// `CustomDebugStringConvertible`:
  ///
  ///     struct Point {
  ///         let x: Int, y: Int
  ///     }
  ///
  ///     let p = Point(x: 21, y: 30)
  ///     let s = String(reflecting: p)
  ///     print(s)
  ///     // Prints "Point(x: 21, y: 30)"
  ///
  /// The conversion of `p` to a string in the assignment to `s` uses the
  /// default representation because `Point` does not conform to
  /// `CustomDebugStringConvertible`. After adding conformance, you can write a
  /// custom `debugDescription` property.
  ///
  ///     extension Point: CustomDebugStringConvertible {
  ///         var debugDescription: String {
  ///             return "(\(x), \(y))"
  ///         }
  ///     }
  ///
  ///     print(String(reflecting: p))
  ///     // Prints "(21, 30)"
  var debugDescription: String { get }
}

/// A type with a customized textual representation.
///
/// Types that conform to the `CustomStringConvertible` protocol can provide
/// their own representation to be used when converting an instance to a
/// string. The `String(describing:)` initializer is the preferred way to
/// convert an instance of *any* type to a string. If the passed instance
/// conforms to `CustomStringConvertible`, the `String(describing:)`
/// initializer and the `print(_:)` function use the instance's custom
/// `description` property.
///
/// Accessing a type's `description` property directly or using
/// `CustomStringConvertible` as a generic constraint is discouraged.
///
/// Conforming to the CustomStringConvertible Protocol
/// ==================================================
///
/// Add `CustomStringConvertible` conformance to your custom types by defining
/// a `description` property.
///
/// For example, this custom `Point` struct uses the default representation
/// supplied by the standard library:
///
///     struct Point {
///         let x: Int, y: Int
///     }
///
///     let p = Point(x: 21, y: 30)
///     print(p)
///     // Prints "Point(x: 21, y: 30)"
///
/// After adding `CustomStringConvertible` conformance by implementing the
/// `description` property, the `Point` type provides its own custom
/// representation.
///
///     extension Point: CustomStringConvertible {
///         var description: String {
///             return "(\(x), \(y))"
///         }
///     }
///
///     print(p)
///     // Prints "(21, 30)"
public protocol CustomStringConvertible {
  /// A textual representation of this instance.
  ///
  /// Calling this property directly is discouraged. Instead, convert an
  /// instance of any type to a string by using the `String(describing:)`
  /// initializer. This initializer works with any type, and uses the custom
  /// `description` property for types that conform to
  /// `CustomStringConvertible`:
  ///
  ///     struct Point {
  ///         let x: Int, y: Int
  ///     }
  ///
  ///     let p = Point(x: 21, y: 30)
  ///     let s = String(describing: p)
  ///     print(s)
  ///     // Prints "Point(x: 21, y: 30)"
  ///
  /// The conversion of `p` to a string in the assignment to `s` uses the
  /// default representation because `Point` does not conform to
  /// `CustomStringConvertible`. After adding conformance, you can write a
  /// custom `description` property.
  ///
  ///     extension Point: CustomStringConvertible {
  ///         var description: String {
  ///             return "(\(x), \(y))"
  ///         }
  ///     }
  ///
  ///     print(String(describing: p))
  ///     // Prints "(21, 30)"
  var description: String { get }
}

/// A type that can be written to a text output stream.
///
/// Types that conform to the `TextOutputStreamable` protocol can write their
/// value to instances of any type that conforms to `TextOutputStream`. The
/// Swift standard library's text-related types---`String`, `Character`, and
/// `Unicode.Scalar`---all conform to `TextOutputStreamable`.
///
/// Conforming to the TextOutputStreamable Protocol
/// ===============================================
///
/// To add `TextOutputStreamable` conformance to a custom type, implement the
/// required `write(to:)` method. Call the given output stream's `write(_:)`
/// method in your implementation.
public protocol TextOutputStreamable {
  /// Writes a textual representation of this instance into the given output
  /// stream.
  ///
  /// - Parameter target: An output stream to receive the textual
  ///   representation of this instance.
  func write<Target>(to target: inout Target) where Target : TextOutputStream
}

// MARK: - Standard output stream

/// The standard output stream.
@usableFromInline
internal var _Stdout = _StandardOutputStream()

/// A type that represents the standard output stream.
@usableFromInline
internal struct _StandardOutputStream: TextOutputStream {
  @usableFromInline
  internal init() {}

  @usableFromInline
  internal mutating func write(_ string: String) {
    // In a real implementation, this would write to stdout
    // For now, we'll use a simplified approach
    _writeToStdout(string)
  }
}

/// Writes a string to standard output.
@usableFromInline
internal func _writeToStdout(_ string: String) {
  // This would be implemented in C++ runtime
  // For now, we'll provide a placeholder
  _swift_print_stdout(string)
}

/// Runtime function to write to stdout (implemented in C++).
@_silgen_name("_swift_print_stdout")
internal func _swift_print_stdout(_ string: String)

// MARK: - String conformances for output

extension String: TextOutputStream {
  /// Appends the given string to this string.
  public mutating func write(_ string: String) {
    self += string
  }
}

extension String: TextOutputStreamable {
  /// Writes this string to the given output stream.
  public func write<Target>(to target: inout Target) where Target : TextOutputStream {
    target.write(self)
  }
}

// MARK: - Debug printing

/// Writes the textual representations of the given items into the standard
/// output, followed by a newline.
///
/// `debugPrint(_:separator:terminator:)` is similar to `print(_:separator:terminator:)`,
/// but uses the `debugDescription` property of each item when available.
///
/// - Parameters:
///   - items: Zero or more items to print.
///   - separator: A string to print between each item. The default is a single
///     space (`" "`).
///   - terminator: The string to print after all items have been printed. The
///     default is a newline (`"\n"`).
public func debugPrint(_ items: Any..., separator: String = " ", terminator: String = "\n") {
  debugPrint(items, separator: separator, terminator: terminator, to: &_Stdout)
}

/// Writes the textual representations of the given items into the given output
/// stream.
///
/// `debugPrint(_:separator:terminator:to:)` is similar to `print(_:separator:terminator:to:)`,
/// but uses the `debugDescription` property of each item when available.
///
/// - Parameters:
///   - items: Zero or more items to print.
///   - separator: A string to print between each item. The default is a single
///     space (`" "`).
///   - terminator: The string to print after all items have been printed. The
///     default is a newline (`"\n"`).
///   - output: An output stream to receive the text representation of the
///     items.
public func debugPrint<Target>(_ items: Any..., separator: String = " ", terminator: String = "\n", to output: inout Target) where Target : TextOutputStream {
  debugPrint(items, separator: separator, terminator: terminator, to: &output)
}

/// Writes the textual representations of the given items most efficiently
/// possible into the given output stream.
///
/// - Parameters:
///   - items: The items to print.
///   - separator: A string to print between each item.
///   - terminator: The string to print after all items have been printed.
///   - output: An output stream to receive the text representation of the
///     items.
public func debugPrint<Target>(_ items: [Any], separator: String, terminator: String, to output: inout Target) where Target : TextOutputStream {
  var first = true
  for item in items {
    if first {
      first = false
    } else {
      output.write(separator)
    }
    
    if let customDebugStringConvertible = item as? CustomDebugStringConvertible {
      output.write(customDebugStringConvertible.debugDescription)
    } else if let textOutputStreamable = item as? TextOutputStreamable {
      textOutputStreamable.write(to: &output)
    } else if let customStringConvertible = item as? CustomStringConvertible {
      output.write(customStringConvertible.description)
    } else {
      output.write(String(describing: item))
    }
  }
  output.write(terminator)
}