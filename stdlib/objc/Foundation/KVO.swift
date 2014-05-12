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

// FIXME: Make a nested class of 'NSObject'.  Currently symbols
// are not being generated during compilation when it is a nested class.
class KVOContext {
  class func fromVoidContext(context: CMutableVoidPointer) -> KVOContext {
    return reinterpretCast(context.value)
  }
}

extension NSObject {
  func removeObserver(observer: NSObject!,
            forKeyPath keyPath: String!,
                    kvoContext: KVOContext)
  {
     let ptr = CMutableVoidPointer(owner: kvoContext, 
                                   value: reinterpretCast(kvoContext))
     self.removeObserver(observer,
                         forKeyPath: keyPath,
                            context: ptr)
     Unmanaged(_private: kvoContext).release()
  }

  func addObserver(observer: NSObject!,
         forKeyPath keyPath: String!,
                    options: NSKeyValueObservingOptions,
                 kvoContext: KVOContext)
  {
     let ptr = CMutableVoidPointer(owner: kvoContext, 
                                   value: reinterpretCast(kvoContext))
     Unmanaged(_private: kvoContext).retain()
     self.addObserver(observer,
                      forKeyPath: keyPath,
                         options: options,
                         context: ptr)
  }
}

