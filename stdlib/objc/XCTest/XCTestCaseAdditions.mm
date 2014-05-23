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

#import <XCTest/XCTest.h>
#include "swift/Runtime/Metadata.h"

// NOTE: This is a temporary workaround.
// XCTestCase needs the unmangled version of a test case name, so let -className
// return the demangled name for a test case class. NSStringFromClass will still
// return the mangled name.

@implementation XCTestCase (SwiftAdditions)

- (NSString *)className
{
  NSString *className = [super className];
  
  char *modulePart;
  char *classPart;
  bool ok = swift_demangleSimpleClass([className UTF8String], 
                                      &modulePart, &classPart);
  if (ok) {
    className = [NSString stringWithUTF8String:classPart];
    
    free(modulePart);
    free(classPart);
  }
  
  return className;
}

@end
