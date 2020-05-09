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

@available(macOS 10.13, iOS 11.0, watchOS 4.0, tvOS 11.0, *)
extension NSItemProvider  {

  @available(macOS 10.13, iOS 11.0, watchOS 4.0, tvOS 11.0, *)
  public func registerObject<
    T : _ObjectiveCBridgeable
  > (
    ofClass: T.Type,
    visibility: NSItemProviderRepresentationVisibility,
    loadHandler: @escaping ((T?, Error?) -> Void) -> Progress?
  ) where T._ObjectiveCType : NSItemProviderWriting {
    self.registerObject(
      ofClass: T._ObjectiveCType.self, visibility: visibility) {
      completionHandler in loadHandler {
        // Using `x as! T._ObjectiveCType?` triggers an assertion in the
        // compiler, hence the explicit call to `_bridgeToObjectiveC`.
        (x, error) in completionHandler(x?._bridgeToObjectiveC(), error)
      }
    }
  }

  @available(macOS 10.13, iOS 11.0, watchOS 4.0, tvOS 11.0, *)
  public func canLoadObject<
    T : _ObjectiveCBridgeable
  >(ofClass: T.Type) -> Bool
  where T._ObjectiveCType : NSItemProviderReading {
    return self.canLoadObject(ofClass: T._ObjectiveCType.self)
  }

  @available(macOS 10.13, iOS 11.0, watchOS 4.0, tvOS 11.0, *)
  public func loadObject<
    T : _ObjectiveCBridgeable
  >(
    ofClass: T.Type,
    completionHandler: @escaping (T?, Error?) -> Void
  ) -> Progress where T._ObjectiveCType : NSItemProviderReading {
    return self.loadObject(ofClass: T._ObjectiveCType.self) {
      x, error in completionHandler(x as! T?, error)
    }
  }

}
