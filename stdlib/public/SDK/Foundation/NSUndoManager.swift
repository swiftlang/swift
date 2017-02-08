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

@_silgen_name("NS_Swift_NSUndoManager_registerUndoWithTargetHandler")
internal func NS_Swift_NSUndoManager_registerUndoWithTargetHandler(
  _ self_: AnyObject,
  _ target: AnyObject,
  _ handler: @escaping @convention(block) (AnyObject) -> Void)

extension UndoManager {
  @available(*, unavailable, renamed: "registerUndo(withTarget:handler:)")
  public func registerUndoWithTarget<TargetType : AnyObject>(_ target: TargetType, handler: (TargetType) -> Void) {
    fatalError("This API has been renamed")
  }

  @available(OSX 10.11, iOS 9.0, *)
  public func registerUndo<TargetType : AnyObject>(withTarget target: TargetType, handler: @escaping (TargetType) -> Void) {
    // The generic blocks use a different ABI, so we need to wrap the provided
    // handler in something ObjC compatible.
    let objcCompatibleHandler: (AnyObject) -> Void = { internalTarget in
      handler(internalTarget as! TargetType)
    }
    NS_Swift_NSUndoManager_registerUndoWithTargetHandler(
      self as AnyObject, target as AnyObject, objcCompatibleHandler)
  }
}
