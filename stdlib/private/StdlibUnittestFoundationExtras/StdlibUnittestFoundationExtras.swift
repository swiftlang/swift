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

internal var _temporaryNSLocaleCurrentLocale: NSLocale? = nil

extension NSLocale {
  @objc
  public class func _swiftUnittest_currentLocale() -> NSLocale {
    return _temporaryNSLocaleCurrentLocale!
  }
}

public func withOverriddenNSLocaleCurrentLocale<Result>(
  _ temporaryLocale: NSLocale,
  _ body: @noescape () -> Result
) -> Result {
  let oldMethod = class_getClassMethod(
    NSLocale.self, #selector(NSLocale.current))
  precondition(oldMethod != nil, "could not find +[NSLocale currentLocale]")

  let newMethod = class_getClassMethod(
    NSLocale.self, #selector(NSLocale._swiftUnittest_currentLocale))
  precondition(newMethod != nil, "could not find +[NSLocale _swiftUnittest_currentLocale]")

  precondition(_temporaryNSLocaleCurrentLocale == nil,
    "nested calls to withOverriddenNSLocaleCurrentLocale are not supported")

  _temporaryNSLocaleCurrentLocale = temporaryLocale
  method_exchangeImplementations(oldMethod, newMethod)
  let result = body()
  method_exchangeImplementations(newMethod, oldMethod)
  _temporaryNSLocaleCurrentLocale = nil

  return result
}

public func withOverriddenNSLocaleCurrentLocale<Result>(
  _ temporaryLocaleIdentifier: String,
  _ body: @noescape () -> Result
) -> Result {
  precondition(
    NSLocale.availableLocaleIdentifiers().contains(temporaryLocaleIdentifier),
    "requested locale \(temporaryLocaleIdentifier) is not available")

  return withOverriddenNSLocaleCurrentLocale(
    NSLocale(localeIdentifier: temporaryLocaleIdentifier), body)
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
