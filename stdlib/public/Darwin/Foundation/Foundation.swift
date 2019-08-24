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

@_exported import Foundation // Clang module
import CoreFoundation
import CoreGraphics

//===----------------------------------------------------------------------===//
// NSObject
//===----------------------------------------------------------------------===//

// These conformances should be located in the `ObjectiveC` module, but they can't
// be placed there because string bridging is not available there.
extension NSObject : CustomStringConvertible {}
extension NSObject : CustomDebugStringConvertible {}

public let NSNotFound: Int = .max

//===----------------------------------------------------------------------===//
// NSLocalizedString
//===----------------------------------------------------------------------===//

/// Returns the localized version of a string.
///
/// - parameter key: An identifying value used to reference a localized string.
///   Don't use the empty string as a key. Values keyed by the empty string will
///   not be localized.
/// - parameter tableName: The name of the table containing the localized string
///   identified by `key`. This is the prefix of the strings file—a file with
///   the `.strings` extension—containing the localized values. If `tableName`
///   is `nil` or the empty string, the `Localizable` table is used.
/// - parameter bundle: The bundle containing the table's strings file. The main
///   bundle is used by default.
/// - parameter value: A user-visible string to return when the localized string
///   for `key` cannot be found in the table. If `value` is the empty string,
///   `key` would be returned instead.
/// - parameter comment: A note to the translator describing the context where
///   the localized string is presented to the user.
///
/// - returns: A localized version of the string designated by `key` in the
///   table identified by `tableName`. If the localized string for `key` cannot
///   be found within the table, `value` is returned. However, `key` is returned
///   instead when `value` is the empty string.
///
/// Export Localizations with Xcode
/// -------------------------------
///
/// Xcode can read through a project's code to find invocations of
/// `NSLocalizedString(_:tableName:bundle:value:comment:)` and automatically
/// generate the appropriate strings files for the project's base localization.
///
/// In Xcode, open the project file and, in the `Edit` menu, select
/// `Export for Localization`. This will generate an XLIFF bundle containing
/// strings files derived from your code along with other localizable assets.
/// `xcodebuild` can also be used to generate the localization bundle from the
/// command line with the `exportLocalizations` option.
///
///     xcodebuild -exportLocalizations -project <projectname>.xcodeproj \
///                                     -localizationPath <path>
///
/// These bundles can be sent to translators for localization, and then
/// reimported into your Xcode project. In Xcode, open the project file. In the
/// `Edit` menu, select `Import Localizations...`, and select the XLIFF
/// folder to import. You can also use `xcodebuild` to import localizations with
/// the `importLocalizations` option.
///
///     xcodebuild -importLocalizations -project <projectname>.xcodeproj \
///                                     -localizationPath <path>
///
/// Choose Meaningful Keys
/// ----------------------
///
/// Words can often have multiple different meanings depending on the context
/// in which they're used. For example, the word "Book" can be used as a noun—a
/// printed literary work—and it can be used as a verb—the action of making a
/// reservation. Words with different meanings which share the same spelling are
/// heteronyms.
///
/// Different languages often have different heteronyms. "Book" in English is
/// one such heteronym, but that's not so in French, where the noun translates
/// to "Livre", and the verb translates to "Réserver". For this reason, it's
/// important make sure that each use of the same phrase is translated
/// appropriately for its context by assigning unique keys to each phrase and
/// adding a description comment describing how that phrase is used.
///
///     NSLocalizedString("book-tag-title", value: "Book", comment: """
///     noun: A label attached to literary items in the library.
///     """)
///
///     NSLocalizedString("book-button-title", value: "Book", comment: """
///     verb: Title of the button that makes a reservation.
///     """)
///
/// Use Only String Literals
/// ------------------------
///
/// String literal values must be used with `key`, `tableName`, `value`, and
/// `comment`.
///
/// Xcode does not evaluate interpolated strings and string variables when
/// generating strings files from code. Attempting to localize a string using
/// those language features will cause Xcode to export something that resembles
/// the original code expression instead of its expected value at runtime.
/// Translators would then translate that exported value—leaving
/// international users with a localized string containing code.
///
///     // Translators will see "1 + 1 = (1 + 1)".
///     // International users will see a localization "1 + 1 = (1 + 1)".
///     let localizedString = NSLocalizedString("string-interpolation",
///                                             value: "1 + 1 = \(1 + 1)"
///                                             comment: "A math equation.")
///
/// To dynamically insert values within localized strings, set `value` to a
/// format string, and use `String.localizedStringWithFormat(_:_:)` to insert
/// those values.
///
///     // Translators will see "1 + 1 = %d" (they know what "%d" means).
///     // International users will see a localization of "1 + 1 = 2".
///     let format = NSLocalizedString("string-literal",
///                                    value: "1 + 1 = %d",
///                                    comment: "A math equation.")
///     let localizedString = String.localizedStringWithFormat(format, (1 + 1))
///
/// Multiline string literals are technically supported, but will result in
/// unexpected behavior during internationalization. A newline will be inserted
/// before and after the body of text within the string, and translators will
/// likely preserve those in their internationalizations.
///
/// To preserve some of the aesthetics of having newlines in the string mirrored
/// in their code representation, string literal concatenation with the `+`
/// operator can be used.
///
///     NSLocalizedString("multiline-string-literal",
///                       value: """
///     This multiline string literal won't work as expected.
///     An extra newline is added to the beginning and end of the string.
///     """,
///                       comment: "The description of a sample of code.")
///
///     NSLocalizedString("string-literal-contatenation",
///                       value: "This string literal concatenated with"
///                            + "this other string literal works just fine.",
///                       comment: "The description of a sample of code.")
///
/// Since comments aren't localized, multiline string literals can be safely
/// used with `comment`.
///
/// Work with Manually Managed Strings
/// ----------------------------------
///
/// If having Xcode generate strings files from code isn't desired behavior,
/// call `Bundle.localizedString(forKey:value:table:)` instead.
///
///     let greeting = Bundle.localizedString(forKey: "program-greeting",
///                                           value: "Hello, World!",
///                                           table: "Localization")
///
/// However, this requires the manual creation and management of that table's
/// strings file.
///
///     /* Localization.strings */
///
///     /* A friendly greeting to the user when the program starts. */
///     "program-greeting" = "Hello, World!";
///
/// - note: Although `NSLocalizedString(_:tableName:bundle:value:comment:)`
/// and `Bundle.localizedString(forKey:value:table:)` can be used in a project
/// at the same time, data from manually managed strings files will be
/// overwritten by Xcode when their table is also used to look up localized
/// strings with `NSLocalizedString(_:tableName:bundle:value:comment:)`.
public
func NSLocalizedString(_ key: String,
                       tableName: String? = nil,
                       bundle: Bundle = Bundle.main,
                       value: String = "",
                       comment: String) -> String {
  return bundle.localizedString(forKey: key, value:value, table:tableName)
}

//===----------------------------------------------------------------------===//
// NSLog
//===----------------------------------------------------------------------===//

public func NSLog(_ format: String, _ args: CVarArg...) {
  withVaList(args) { NSLogv(format, $0) }
}

//===----------------------------------------------------------------------===//
// AnyHashable
//===----------------------------------------------------------------------===//

extension AnyHashable : _ObjectiveCBridgeable {
  public func _bridgeToObjectiveC() -> NSObject {
    // This is unprincipled, but pretty much any object we'll encounter in
    // Swift is NSObject-conforming enough to have -hash and -isEqual:.
    return unsafeBitCast(base as AnyObject, to: NSObject.self)
  }

  public static func _forceBridgeFromObjectiveC(
    _ x: NSObject,
    result: inout AnyHashable?
  ) {
    result = AnyHashable(x)
  }

  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSObject,
    result: inout AnyHashable?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return result != nil
  }

  @_effects(readonly)
  public static func _unconditionallyBridgeFromObjectiveC(
    _ source: NSObject?
  ) -> AnyHashable {
    // `nil` has historically been used as a stand-in for an empty
    // string; map it to an empty string.
    if _slowPath(source == nil) { return AnyHashable(String()) }
    return AnyHashable(source!)
  }
}

//===----------------------------------------------------------------------===//
// CVarArg for bridged types
//===----------------------------------------------------------------------===//

extension CVarArg where Self: _ObjectiveCBridgeable {
  /// Default implementation for bridgeable types.
  public var _cVarArgEncoding: [Int] {
    let object = self._bridgeToObjectiveC()
    _autorelease(object)
    return _encodeBitsAsWords(object)
  }
}
