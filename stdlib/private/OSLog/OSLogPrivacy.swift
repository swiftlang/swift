//===----------------- OSLogPrivacy.swift ---------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This file defines the APIs for specifying privacy in the log messages and also
// the logic for encoding them in the byte buffer passed to the libtrace library.

/// Privacy options for specifying privacy level of the interpolated expressions
/// in the string interpolations passed to the log APIs.
@frozen
public struct OSLogPrivacy {

  @usableFromInline
  internal enum PrivacyOption {
    case `private`
    case `public`
    case auto
  }

  public enum Mask {
    /// Applies a salted hashing transformation to an interpolated value to redact it in the logs.
    ///
    /// Its purpose is to permit the correlation of identical values across multiple log lines
    /// without revealing the value itself.
    case hash
    case none
  }

  @usableFromInline
  internal var privacy: PrivacyOption

  @usableFromInline
  internal var mask: Mask

  @_transparent
  @usableFromInline
  internal init(privacy: PrivacyOption, mask: Mask) {
    self.privacy = privacy
    self.mask = mask
  }

  /// Sets the privacy level of an interpolated value to public.
  ///
  /// When the privacy level is public, the value will be displayed
  /// normally without any redaction in the logs.
  @_semantics("constant_evaluable")
  @_optimize(none)
  @inlinable
  public static var `public`: OSLogPrivacy {
    OSLogPrivacy(privacy: .public, mask: .none)
  }

  /// Sets the privacy level of an interpolated value to private.
  ///
  /// When the privacy level is private, the value will be redacted in the logs,
  /// subject to the privacy configuration of the logging system.
  @_semantics("constant_evaluable")
  @_optimize(none)
  @inlinable
  public static var `private`: OSLogPrivacy {
    OSLogPrivacy(privacy: .private, mask: .none)
  }

  /// Sets the privacy level of an interpolated value to private and
  /// applies a `mask` to the interpolated value to redacted it.
  ///
  /// When the privacy level is private, the value will be redacted in the logs,
  /// subject to the privacy configuration of the logging system.
  ///
  /// If the value need not be redacted in the logs, its full value is captured as normal.
  /// Otherwise (i.e. if the value would be redacted) the `mask` is applied to
  /// the argument value and the result of the transformation is recorded instead.
  ///
  /// - Parameters:
  ///   - mask: Mask to use with the privacy option.
  @_semantics("constant_evaluable")
  @_optimize(none)
  @inlinable
  public static func `private`(mask: Mask) -> OSLogPrivacy {
    OSLogPrivacy(privacy: .private, mask: mask)
  }

  /// Auto-infers a privacy level for an interpolated value.
  ///
  /// The system will automatically decide whether the value should
  /// be captured fully in the logs or should be redacted.
  @_semantics("constant_evaluable")
  @_optimize(none)
  @inlinable
  public static var auto: OSLogPrivacy {
    OSLogPrivacy(privacy: .auto, mask: .none)
  }

  /// Auto-infers a privacy level for an interpolated value and applies a `mask`
  /// to the interpolated value to redacted it when necessary.
  ///
  /// The system will automatically decide whether the value should
  /// be captured fully in the logs or should be redacted.
  /// If the value need not be redacted in the logs, its full value is captured as normal.
  /// Otherwise (i.e. if the value would be redacted) the `mask` is applied to
  /// the argument value and the result of the transformation is recorded instead.
  ///
  /// - Parameters:
  ///   - mask: Mask to use with the privacy option.
  @_semantics("constant_evaluable")
  @_optimize(none)
  @inlinable
  public static func auto(mask: Mask) -> OSLogPrivacy {
    OSLogPrivacy(privacy: .auto, mask: mask)
  }

  /// Return an argument flag for the privacy option., as defined by the
  /// os_log ABI, which occupies four least significant bits of the first byte of the
  /// argument header. The first two bits are used to indicate privacy and
  /// the other two are reserved.
  @inlinable
  @_semantics("constant_evaluable")
  @_optimize(none)
  internal var argumentFlag: UInt8 {
    switch privacy {
    case .private:
      return 0x1
    case .public:
      return 0x2
    default:
      return 0
    }
  }

  @inlinable
  @_semantics("constant_evaluable")
  @_optimize(none)
  internal var isAtleastPrivate: Bool {
    switch privacy {
    case .public:
      return false
    case .auto:
      return false
    default:
      return true
    }
  }

  @inlinable
  @_semantics("constant_evaluable")
  @_optimize(none)
  internal var needsPrivacySpecifier: Bool {
    if case .hash = mask {
      return true
    }
    switch privacy {
    case .auto:
      return false
    default:
      return true
    }
  }

  @inlinable
  @_transparent
  internal var hasMask: Bool {
    if case .hash = mask {
      return true
    }
    return false
  }

  /// A 64-bit value obtained by interpreting the mask name as a little-endian unsigned
  /// integer.
  @inlinable
  @_transparent
  internal var maskValue: UInt64 {
    // Return the value of
    // 'h' | 'a' << 8 | 's' << 16 | 'h' << 24 which equals
    // 104  |  (97 << 8) | (115 << 16) | (104 << 24)
    1752392040
  }

  /// Return an os log format specifier for this `privacy` level. The
  /// format specifier goes within curly braces e.g. %{private} in the format
  /// string passed to the os log ABI.
  @inlinable
  @_semantics("constant_evaluable")
  @_optimize(none)
  internal var privacySpecifier: String? {
    let hasMask = self.hasMask
    var isAuto = false
    if case .auto = privacy {
      isAuto = true
    }
    if isAuto, !hasMask {
      return nil
    }
    var specifier: String
    switch privacy {
    case .public:
      specifier = "public"
    case .private:
      specifier = "private"
    default:
      specifier = ""
    }
    if hasMask {
      if !isAuto {
        specifier += ","
      }
      specifier += "mask.hash"
    }
    return specifier
  }
}

