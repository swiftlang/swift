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

import ObjectiveC
import Foundation
import StdlibUnittest

internal var _temporaryLocaleCurrentLocale: NSLocale?

extension NSLocale {
  @objc
  public class func __swiftUnittest_currentLocale() -> NSLocale {
    return _temporaryLocaleCurrentLocale!
  }
}

public func withOverriddenLocaleCurrentLocale<Result>(
  _ temporaryLocale: NSLocale,
  _ body: () -> Result
) -> Result {
  guard let oldMethod = class_getClassMethod(
    NSLocale.self, #selector(getter: NSLocale.current)) as Optional
  else {
    preconditionFailure("Could not find +[Locale currentLocale]")
  }

  guard let newMethod = class_getClassMethod(
    NSLocale.self, #selector(NSLocale.__swiftUnittest_currentLocale)) as Optional
  else {
    preconditionFailure("Could not find +[Locale __swiftUnittest_currentLocale]")
  }

  precondition(_temporaryLocaleCurrentLocale == nil,
    "Nested calls to withOverriddenLocaleCurrentLocale are not supported")

  _temporaryLocaleCurrentLocale = temporaryLocale
  method_exchangeImplementations(oldMethod, newMethod)
  let result = body()
  method_exchangeImplementations(newMethod, oldMethod)
  _temporaryLocaleCurrentLocale = nil

  return result
}

public func withOverriddenLocaleCurrentLocale<Result>(
  _ temporaryLocaleIdentifier: String,
  _ body: () -> Result
) -> Result {
  precondition(
    NSLocale.availableLocaleIdentifiers.contains(temporaryLocaleIdentifier),
    "Requested locale \(temporaryLocaleIdentifier) is not available")

  return withOverriddenLocaleCurrentLocale(
    NSLocale(localeIdentifier: temporaryLocaleIdentifier), body)
}

/// Executes the `body` in an autorelease pool if the platform does not
/// implement the return-autoreleased optimization.
///
/// (Currently, only the i386 iOS and watchOS simulators don't implement the
/// return-autoreleased optimization.)
@inline(never)
public func autoreleasepoolIfUnoptimizedReturnAutoreleased(
  invoking body: () -> Void
) {
#if targetEnvironment(simulator) && arch(i386) && (os(iOS) || os(watchOS))
  autoreleasepool(invoking: body)
#else
  body()
#endif
}

@usableFromInline
@_silgen_name("NSArray_getObjects")
func NSArray_getObjects(
  nsArray: AnyObject,
  objects: AutoreleasingUnsafeMutablePointer<AnyObject?>?,
  rangeLocation: Int,
  rangeLength: Int)

extension NSArray {
  @nonobjc // FIXME: there should be no need in this attribute.
  public func available_getObjects(
    _ objects: AutoreleasingUnsafeMutablePointer<AnyObject?>?, range: NSRange
  ) {
    return NSArray_getObjects(
      nsArray: self,
      objects: objects,
      rangeLocation: range.location,
      rangeLength: range.length)
  }
}

@_silgen_name("NSDictionary_getObjectsAndKeysWithCount")
func NSDictionary_getObjectsAndKeysWithCount(
  nsDictionary: NSDictionary,
  objects: AutoreleasingUnsafeMutablePointer<AnyObject?>?,
  andKeys keys: AutoreleasingUnsafeMutablePointer<AnyObject?>?,
  count: Int
)

extension NSDictionary {
  @nonobjc // FIXME: there should be no need in this attribute.
  public func available_getObjects(
    _ objects: AutoreleasingUnsafeMutablePointer<AnyObject?>?,
    andKeys keys: AutoreleasingUnsafeMutablePointer<AnyObject?>?,
    count: Int
  ) {
    return NSDictionary_getObjectsAndKeysWithCount(
      nsDictionary: self,
      objects: objects,
      andKeys: keys,
      count: count)
  }
}

public func expectBridgeToNSValue<T>(_ value: T,
                                     nsValueInitializer: ((T) -> NSValue)? = nil,
                                     nsValueGetter: ((NSValue) -> T)? = nil,
                                     equal: (T, T) -> Bool) {
  let object = value as AnyObject
  let nsValue = object as! NSValue
  if let nsValueInitializer = nsValueInitializer {
    expectEqual(nsValueInitializer(value), nsValue)
  }
  if let nsValueGetter = nsValueGetter {
    expectTrue(equal(value, nsValueGetter(nsValue)))
  }
  if let nsValueInitializer = nsValueInitializer,
     let nsValueGetter = nsValueGetter {
    expectTrue(equal(value, nsValueGetter(nsValueInitializer(value))))
  }
  expectTrue(equal(value, object as! T))

}

