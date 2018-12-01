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

@_exported import WatchKit
import Foundation

@available(iOS, introduced: 8.2)
extension WKInterfaceController {
  @available(*, unavailable,
             renamed: "reloadRootControllers(withNamesAndContexts:)")
  @nonobjc final public class func reloadRootControllers(
    _ namesAndContexts: [(name: String, context: AnyObject)]
  ) {
    reloadRootControllers(withNamesAndContexts: namesAndContexts)
  }

  // Swift convenience type (class) method for
  // reloadRootControllersWithNames:contexts: that takes an array of tuples
  public class func reloadRootControllers(
    withNamesAndContexts namesAndContexts: [(name: String, context: AnyObject)]
  ) {
    WKInterfaceController.reloadRootControllers(
      withNames: namesAndContexts.map { $0.name },
      contexts: namesAndContexts.map { $0.context })
  }

  @available(*, deprecated,
             renamed: "presentController(withNamesAndContexts:)")
  @nonobjc final public func presentController(
    _ namesAndContexts: [(name: String, context: AnyObject)]
  ) {
    presentController(withNamesAndContexts: namesAndContexts)
  }

  // Swift convenience method for presentControllerWithNames:contexts: that
  // takes an array of tuples
  public func presentController(
    withNamesAndContexts namesAndContexts: [(name: String, context: AnyObject)]
  ) {
    self.presentController(
      withNames: namesAndContexts.map { $0.name },
      contexts: namesAndContexts.map { $0.context })
  }
}

