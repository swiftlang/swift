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

#include <UIKit/UIKit.h>
#include <objc/runtime.h>
#include "swift/Runtime/Metadata.h"

// UIViewController looks up a nib file name based on the class name. 
// We modify this lookup so it checks for human-friendly names when 
// presented with a Swift-mangled class name.

@implementation UIViewController (SwiftAdditions)

static NSString * (*old_existingNibName)
                    (UIViewController*, SEL, NSString*, NSBundle*);

// Replaces +[UIViewController existingNibNameMatchingClassName:bundle:]
static NSString *existingNibName(UIViewController *self, SEL _cmd, 
                                 NSString *className, NSBundle *bundle)
{
  NSString *result = nil;

  // Try unmodified name (possibly mangled).
  result = old_existingNibName(self, _cmd, className, bundle);
  if (result) return result;

  // Try demangling the Swift name.
  char *modulePart;
  char *classPart;
  bool ok = swift::swift_demangleSimpleClass([className UTF8String], 
                                             &modulePart, &classPart);
  if (!ok) return nil;

  if (0 == strncmp(classPart, "UI", 2)) {
    // Ignore UIKit classes.
  }
  else {
    // Try unmangled "Module.Class.nib" etc
    {
      NSString *fullyQualifiedClassName = 
        [[NSString alloc] initWithFormat:@"%s.%s", modulePart, classPart];
      result = old_existingNibName(self, _cmd, fullyQualifiedClassName, bundle);
      // old_existingNibName may return the given name without autoreleasing
      if (result != fullyQualifiedClassName) [fullyQualifiedClassName release];
      else [fullyQualifiedClassName autorelease];
    }

    // Try unmangled "Class.nib" etc
    if (!result) {
      NSString *unqualifiedClassName =
        [[NSString alloc] initWithFormat:@"%s", classPart];
      result = old_existingNibName(self, _cmd, unqualifiedClassName, bundle);
      // old_existingNibName may return the given name without autoreleasing
      if (result != unqualifiedClassName) [unqualifiedClassName release];
      else [unqualifiedClassName autorelease];
    }
  }

  free(modulePart);
  free(classPart);
  return result;
}

+ (void)initialize
{
  if (self == [UIViewController class]) {
    IMP oldImp = 
      class_replaceMethod(object_getClass(self), 
                          @selector(existingNibNameMatchingClassName:bundle:), 
                          (IMP)existingNibName, "@@:@");
    old_existingNibName = 
      (NSString *(*)(UIViewController*, SEL, NSString*, NSBundle*))oldImp;
  }
}

@end
