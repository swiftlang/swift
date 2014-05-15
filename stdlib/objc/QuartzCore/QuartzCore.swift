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

@exported import QuartzCore // Clang module

// sdk overlays:
extension CIFilter {
  convenience init(name : String!, elements: (NSCopying, AnyObject)...) {
    var dict =  NSMutableDictionary()
    for i in 0..elements.count {
      dict[i] = NSMutableDictionary(object: elements[i].1, forKey: elements[i].0)
    }
    
    // @objc(filterWithName:withInputParameters:)
    //    init(name: String!,
    //         withInputParameters params: NSDictionary!) -> CIFilter
    self.init(name: name, withInputParameters: dict)
  }
}

// sdk overlays:
extension CISampler {
    convenience init(im: CIImage!, elements: (NSCopying, AnyObject)...) {
        var dict =  NSMutableDictionary()
        for i in 0..elements.count {
            dict[i] = NSMutableDictionary(object: elements[i].1, forKey: elements[i].0)
        }
        // @objc(initWithImage:options:)
        //   init(image im: CIImage!,
        //        options dict: NSDictionary!)
        self.init(image: im, options: dict)
    }
}
