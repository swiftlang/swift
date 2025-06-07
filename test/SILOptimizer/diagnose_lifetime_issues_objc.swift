// RUN: %target-swift-frontend -emit-sil %s -enable-copy-propagation -o /dev/null -verify
// REQUIRES: objc_interop

import Foundation

class MyServiceDelegate : NSObject, NSXPCListenerDelegate { }

public func warningForDeadDelegate() {
  let delegate = MyServiceDelegate()
  let listener = NSXPCListener.service()
  listener.delegate = delegate
  listener.resume()
}
