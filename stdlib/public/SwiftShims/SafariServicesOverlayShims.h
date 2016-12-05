//===--- SafariServicesOverlayShims.h ---------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_SAFARISERVICES_OVERLAY_H
#define SWIFT_STDLIB_SHIMS_SAFARISERVICES_OVERLAY_H

#import <SafariServices/SFSwiftOverlaySupport.h>

static inline bool _swift_SafariServices_isSafariServicesAvailable() {
  return NULL != &_SFSafariServicesAvailable;
}

#endif // SWIFT_STDLIB_SHIMS_SAFARISERVICES_OVERLAY_H

