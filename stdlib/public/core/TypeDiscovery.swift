//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A function that instantiates the type described by a type descriptor.
///
/// - Parameters:
///   - descriptor: A type descriptor as passed to `_swift_enumerateTypes()`'s
///     callback.
///
/// - Returns: The initialized type corresponding to `descriptor`, or `nil` if
///   the type could not be initialized. This value can subsequently be cast to
///   `Any.Type` using `unsafeBitCast(_:to:)`.
private typealias _TypeGetter = @convention(c) (
  _ descriptor: UnsafeRawPointer
) -> UnsafeRawPointer?

/// A type describing another type available in the current process.
@available(SwiftStdlib 9999, *)
public struct _TypeRecord {
  /// Initialize an instance of this type.
  ///
  /// - Parameters:
  ///   - name: The name of this type.
  ///   - typeInstantiator: A function to call that calls back into the runtime
  ///     to instantiate the type.
  fileprivate init(
    _name name: String,
    instantiatingTypeUsing typeInstantiator: @escaping () -> Any.Type?
  ) {
    self.name = name
    _typeInstantiator = typeInstantiator
  }

  /// The name of this type.
  public let name: String

  /// Storage for `type`.
  ///
  /// This function calls back into the runtime to instantiate the type.
  private let _typeInstantiator: () -> Any.Type?

  /// The Swift type described by this type record.
  ///
  /// On first use, this property will "instantiate" the type, meaning that the
  /// type will become fully realized and will be available for use in Swift. If
  /// that operation fails, the value of this property is `nil`.
  public var type: Any.Type? {
    return _typeInstantiator()
  }
}

@_silgen_name("swift_enumerateAllTypesFromImage")
private func _swift_enumerateTypes(
  fromImageAt imageAddress: UnsafeRawPointer?,
  _ body: (
    _ name: UnsafePointer<CChar>,
    _ descriptor: UnsafeRawPointer,
    _ typeGetter: @escaping _TypeGetter,
    _ stop: inout Bool
  ) throws -> Void
) rethrows

/// Enumerate all types in a given image.
///
/// - Parameters:
///   - imageAddress: A platform-specific pointer to the image of interest. The
///     image must have been loaded into the current process. For the binary of
///     the calling function, you can pass `#dsohandle`. For all binaries, pass
///     `nil` (the default.)
///   - body: A closure to invoke once per conforming type.
///
/// - Throws: Whatever is thrown by `body`.
///
/// This function walks all known types in the given image and passes them to
/// `body` for evaluation.
///
/// Generic types are not enumerated.
///
/// - Bug: Objective-C class lookups are not supported yet.
@available(SwiftStdlib 9999, *)
public func _enumerateTypes(
  fromImageAt imageAddress: UnsafeRawPointer? = nil,
  _ body: (_ typeRecord: _TypeRecord, _ stop: inout Bool) throws -> Void
) rethrows {
  try _swift_enumerateTypes(
    fromImageAt: imageAddress
  ) { name, descriptor, getType, stop in
    let typeRecord = _TypeRecord(
      _name: String(cString: name),
      instantiatingTypeUsing: {
        getType(descriptor).map { unsafeBitCast($0, to: Any.Type.self) }
      }
    )
    try body(typeRecord, &stop)
  }
}
