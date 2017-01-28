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

#import <XCTest/XCTest.h>
#include "swift/Runtime/Metadata.h"
#include "swift/Basic/Demangle.h"
#include "swift/Strings.h"

// NOTE: This is a temporary workaround.
// XCTestCase needs the unmangled version of a test case name, so let -className
// return the demangled name for a test case class. NSStringFromClass will still
// return the mangled name.

@interface XCTest (WarningAvoidance)
@property (readonly, copy) NSString *className;
@end

static char *scanIdentifier(const char *&mangled)
{
  const char *original = mangled;

  {
    if (*mangled == '0') goto fail;  // length may not be zero

    size_t length = 0;
    while (swift::Demangle::isDigit(*mangled)) {
      size_t oldlength = length;
      length *= 10;
      length += *mangled++ - '0';
      if (length <= oldlength) goto fail;  // integer overflow
    }

    if (length == 0) goto fail;
    if (length > strlen(mangled)) goto fail;

    char *result = strndup(mangled, length);
    assert(result);
    mangled += length;
    return result;
  }

fail:
  mangled = original;  // rewind
  return nullptr;
}


/// \brief Demangle a mangled class name into module+class.
/// Returns true if the name was successfully decoded.
/// On success, *outModule and *outClass must be freed with free().
/// FIXME: this should be replaced by a real demangler
static bool demangleSimpleClass(const char *mangledName,
                                char **outModule, char **outClass) {
  char *moduleName = nullptr;
  char *className = nullptr;

  {
    // Prefix for a mangled class
    const char *m = mangledName;
    if (0 != strncmp(m, "_TtC", 4))
      goto fail;
    m += 4;

    // Module name
    if (strncmp(m, "Ss", 2) == 0) {
      moduleName = strdup(swift::STDLIB_NAME);
      assert(moduleName);
      m += 2;
    } else {
      moduleName = scanIdentifier(m);
      if (!moduleName)
        goto fail;
    }

    // Class name
    className = scanIdentifier(m);
    if (!className)
      goto fail;

    // Nothing else
    if (strlen(m))
      goto fail;

    *outModule = moduleName;
    *outClass = className;
    return true;
  }

fail:
  if (moduleName) free(moduleName);
  if (className) free(className);
  *outModule = nullptr;
  *outClass = nullptr;
  return false;
}

@implementation XCTestCase (SwiftAdditions)

- (NSString *)className
{
  NSString *className = [super className];

  char *modulePart;
  char *classPart;
  bool ok = demangleSimpleClass([className UTF8String],
                                &modulePart, &classPart);
  if (ok) {
    className = [NSString stringWithUTF8String:classPart];

    free(modulePart);
    free(classPart);
  }

  return className;
}

@end


// Since Swift doesn't natively support exceptions, but Objective-C code can
// still throw them, use a helper to evaluate a block that may result in an
// exception being thrown that passes back the most important information about
// it.
//
// If no exception is thrown by the block, returns an empty dictionary.

XCT_EXPORT NSDictionary<NSString *, NSString *> *_XCTRunThrowableBlockBridge(void (^block)());

NSDictionary<NSString *, NSString *> *_XCTRunThrowableBlockBridge(void (^block)())
{
    NSDictionary<NSString *, NSString *> *result = nil;

    @try {
        block();
    }
    @catch (NSException *exception) {
        result = @{
                   @"type": @"objc",
                   @"className": NSStringFromClass(exception.class),
                   @"name": exception.name,
                   @"reason": exception.reason,
                   };
    }
    @catch (...) {
        result = @{
                   @"type": @"unknown",
                   };
    }

    return result;
}
