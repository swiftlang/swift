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

import ObjectiveC
import Foundation

internal var _temporaryLocaleCurrentLocale: Locale? = nil

extension Locale {
  @objc
  public class func _swiftUnittest_currentLocale() -> Locale {
    return _temporaryLocaleCurrentLocale!
  }
}

public func withOverriddenLocaleCurrentLocale<Result>(
  _ temporaryLocale: Locale,
  _ body: @noescape () -> Result
) -> Result {
  let oldMethod = class_getClassMethod(
    Locale.self, #selector(Locale.current))
  precondition(oldMethod != nil, "could not find +[Locale currentLocale]")

  let newMethod = class_getClassMethod(
    Locale.self, #selector(Locale._swiftUnittest_currentLocale))
  precondition(newMethod != nil, "could not find +[Locale _swiftUnittest_currentLocale]")

  precondition(_temporaryLocaleCurrentLocale == nil,
    "nested calls to withOverriddenLocaleCurrentLocale are not supported")

  _temporaryLocaleCurrentLocale = temporaryLocale
  method_exchangeImplementations(oldMethod, newMethod)
  let result = body()
  method_exchangeImplementations(newMethod, oldMethod)
  _temporaryLocaleCurrentLocale = nil

  return result
}

public func withOverriddenLocaleCurrentLocale<Result>(
  _ temporaryLocaleIdentifier: String,
  _ body: @noescape () -> Result
) -> Result {
  precondition(
    Locale.availableLocaleIdentifiers().contains(temporaryLocaleIdentifier),
    "requested locale \(temporaryLocaleIdentifier) is not available")

  return withOverriddenLocaleCurrentLocale(
    Locale(localeIdentifier: temporaryLocaleIdentifier), body)
}

/// Executes the `body` in an autorelease pool if the platform does not
/// implement the return-autoreleased optimization.
///
/// (Currently, only the i386 iOS and watchOS simulators don't implement the
/// return-autoreleased optimization.)
@inline(never)
public func autoreleasepoolIfUnoptimizedReturnAutoreleased(
  _ body: @noescape () -> Void
) {
#if arch(i386) && (os(iOS) || os(watchOS))
  autoreleasepool(body)
#else
  body()
#endif
}

@_silgen_name("swift_stdlib_NSArray_getObjects")
internal func _stdlib_NSArray_getObjects(
  nsArray: AnyObject,
  objects: AutoreleasingUnsafeMutablePointer<AnyObject?>?,
  rangeLocation: Int,
  rangeLength: Int)

extension NSArray {
  public func available_getObjects(
    _ objects: AutoreleasingUnsafeMutablePointer<AnyObject?>?, range: NSRange
  ) {
    return _stdlib_NSArray_getObjects(
      nsArray: self,
      objects: objects,
      rangeLocation: range.location,
      rangeLength: range.length)
  }
}

@_silgen_name("swift_stdlib_NSDictionary_getObjects")
func _stdlib_NSDictionary_getObjects(
  nsDictionary: NSDictionary,
  objects: AutoreleasingUnsafeMutablePointer<AnyObject?>?,
  andKeys keys: AutoreleasingUnsafeMutablePointer<AnyObject?>?
)

extension NSDictionary {
  public func available_getObjects(
    _ objects: AutoreleasingUnsafeMutablePointer<AnyObject?>?,
    andKeys keys: AutoreleasingUnsafeMutablePointer<AnyObject?>?
  ) {
    return _stdlib_NSDictionary_getObjects(
      nsDictionary: self,
      objects: objects,
      andKeys: keys)
  }
}
