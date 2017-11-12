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

import Foundation
@_exported import CoreImage  // Clang module

extension CIFilter {
#if os(OSX)
  // - (CIImage *)apply:(CIKernel *)k, ...
  // @objc(apply:arguments:options:)
  // func apply(_ k: CIKernel,
  //            arguments args: [AnyObject]?,
  //            options dict: Dictionary<NSObject, AnyObject>?) -> CIImage?
  func apply(_ k: CIKernel, args: [AnyObject], options: (String, AnyObject)...) -> CIImage? {
    var dict = [String : AnyObject](minimumCapacity: options.count)
    for (key, value) in options {
      dict[key] = value
    }
    return self.apply(k, arguments: args, options: dict)
  }
#endif

  @available(iOS, introduced: 8.0)
  @available(OSX, introduced: 10.10)
  convenience init?(
    name: String, elements: (String, AnyObject)...
  ) {
    var dict = [String : AnyObject](minimumCapacity: elements.count)
    for (key, value) in elements {
      dict[key] = value
    }
    self.init(name: name, withInputParameters: dict)
  }
}

#if os(OSX)
extension CISampler {
  // - (id)initWithImage:(CIImage *)im keysAndValues:key0, ...;
  convenience init(im: CIImage, elements: (String, Any)...) {
    var dict = [AnyHashable : Any](minimumCapacity: elements.count)
    for (key, value) in elements {
      dict[key] = value
    }

    // @objc(initWithImage:options:)
    //   init(image im: CIImage,
    //        options dict: NSDictionary?)
    self.init(image: im, options: dict)
  }
}
#endif
