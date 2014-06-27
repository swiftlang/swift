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

#if os(OSX)
@exported import QuartzCore // Clang module
#elseif os(iOS)
@exported import CoreImage  // Clang module
#endif

// sdk overlays:
extension CIFilter {
#if os(OSX)
  // - (CIImage *)apply:(CIKernel *)k, ...
  // @objc(apply:arguments:options:)
  // func apply(k: CIKernel!,
  //            arguments args: [AnyObject]!,
  //            options dict: Dictionary<NSObject, AnyObject>!) -> CIImage!
  func apply(k: CIKernel!, args: [AnyObject]!, options: (NSCopying, AnyObject)...) -> CIImage {
    var dict = NSMutableDictionary()
    for (key, value) in options {
      dict[key] = value
    }
    return self.apply(k, arguments: args, options: dict)
  }
#endif

  convenience init(name: String!,
                   elements: (NSCopying, AnyObject)...) {
    var dict = NSMutableDictionary()
        for (key, value) in elements {
            dict[key] = value
        }
    self.init(name: name, withInputParameters: dict)
  } 
}

#if os(OSX)
// sdk overlays:
extension CISampler {
    // - (id)initWithImage:(CIImage *)im keysAndValues:key0, ...;
    convenience init(im: CIImage!, elements: (NSCopying, AnyObject)...) {
        var dict = NSMutableDictionary()
        for (key, value) in elements {
            dict[key] = value
        }

        // @objc(initWithImage:options:)
        //   init(image im: CIImage!,
        //        options dict: NSDictionary!)
        self.init(image: im, options: dict)
    }
}
#endif
