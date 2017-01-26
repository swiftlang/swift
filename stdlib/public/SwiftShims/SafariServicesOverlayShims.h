//===--- SafariServicesOverlayShims.h ---------------------------*- C++ -*-===//
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

#ifndef SWIFT_STDLIB_SHIMS_SAFARISERVICES_OVERLAY_H
#define SWIFT_STDLIB_SHIMS_SAFARISERVICES_OVERLAY_H

#import <SafariServices/SFSwiftOverlaySupport.h>

static inline bool _swift_SafariServices_isSafariServicesAvailable(SFSafariServicesVersion version) {
  if (version == SFSafariServicesVersion10_0) {
    return NULL != &_SFSafariServicesAvailable;
  }

  SFSafariServicesVersion* latestVersion = &_SFSafariServicesVersion;
  if (NULL == latestVersion) {
    return false;
  }

  return *latestVersion >= version;
}

#endif // SWIFT_STDLIB_SHIMS_SAFARISERVICES_OVERLAY_H

