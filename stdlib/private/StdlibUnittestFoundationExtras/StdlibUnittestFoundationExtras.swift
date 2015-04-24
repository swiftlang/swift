//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
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
  temporaryLocale: NSLocale,
  _ body: () -> Result
) -> Result {
  let oldMethod = class_getClassMethod(
    NSLocale.self, Selector("currentLocale"))
  precondition(oldMethod != nil, "could not find +[NSLocale currentLocale]")

  let newMethod = class_getClassMethod(
    NSLocale.self, Selector("_swiftUnittest_currentLocale"))
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
  temporaryLocaleIdentifier: String,
  _ body: () -> Result
) -> Result {
  precondition(contains(
    NSLocale.availableLocaleIdentifiers(), temporaryLocaleIdentifier),
    "requested locale \(temporaryLocaleIdentifier) is not available")

  return withOverriddenNSLocaleCurrentLocale(
    NSLocale(localeIdentifier: temporaryLocaleIdentifier), body)
}
