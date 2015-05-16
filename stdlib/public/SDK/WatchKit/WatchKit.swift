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

@exported import WatchKit
import Foundation

extension WatchKitErrorCode : _BridgedNSError {
  public static var _NSErrorDomain: String { return WatchKitErrorDomain }
}

@available(iOS, introduced=8.2)
extension WKInterfaceController {
  // Swift convenience type (class) method for
  // reloadRootControllersWithNames:contexts: that takes an array of tuples
  public class func reloadRootControllers(
    namesAndContexts: [(name: String, context: AnyObject)]
  ) {
    WKInterfaceController.reloadRootControllersWithNames(
      namesAndContexts.map { $0.name },
      contexts: namesAndContexts.map { $0.context })
  }

  // Swift convenience method for presentControllerWithNames:contexts: that
  // takes an array of tuples
  public func presentController(
    namesAndContexts: [(name: String, context: AnyObject)]
  ) {
    self.presentControllerWithNames(
      namesAndContexts.map { $0.name },
      contexts: namesAndContexts.map { $0.context })
  }
}

