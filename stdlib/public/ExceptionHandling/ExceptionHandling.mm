//===--- ExceptionHandling.mm ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "ExceptionHandling.h"

#if SWIFT_OBJC_INTEROP
#import <Foundation/Foundation.h>

#include "swift/Runtime/HeapObject.h"

id swift::_swift_exceptionPointerGetThrownObjectiveCObject(
  const std::exception_ptr *ep) {
  if (_swift_exceptionPointerIsNil(ep)) {
    return nil;
  }
  
  try {
    std::rethrow_exception(*ep);
  } catch (id obj) {
    return obj;
  } catch (...) {
    return nil;
  }
}

namespace swift {
extern char *
_swift_exceptionPointerCopyObjectiveCExceptionDescription(id obj, bool debug);
}

char *
swift::_swift_exceptionPointerCopyObjectiveCExceptionDescription(id obj,
                                                                 bool debug) {
  id objcDesc;
  if (debug) {
    objcDesc = [obj debugDescription];
  } else {
    objcDesc = [obj description];
  }
  if (objcDesc) {
    auto szDesc = [objcDesc maximumLengthOfBytesUsingEncoding:NSUTF8StringEncoding];
    if (auto utf8Desc = reinterpret_cast<char *>(swift_slowAlloc(szDesc, 0))) {
      auto didCopy = [objcDesc getCString:utf8Desc
                                maxLength:szDesc
                                 encoding:NSUTF8StringEncoding];
      if (didCopy) {
        return utf8Desc;

      } else {
        // Failed to copy the string as UTF-8, so clean up the heap allocation.
        swift_slowDealloc(utf8Desc, szDesc, 0);
      }
    }
  }

  return nullptr;
}
#endif
