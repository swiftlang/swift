//===--- UIKitOverlayShims.h ---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===--------------------===//

#ifndef SWIFT_STDLIB_SHIMS_UIKIT_OVERLAY_H
#define SWIFT_STDLIB_SHIMS_UIKIT_OVERLAY_H

@import UIKit;

#if __has_feature(nullability)
#pragma clang assume_nonnull begin
#endif

#if TARGET_OS_TV || TARGET_OS_IOS
static inline BOOL _swift_UIKit_UIFocusEnvironmentContainsEnvironment(
  id<UIFocusEnvironment> environment,
  id<UIFocusEnvironment> otherEnvironment
) API_AVAILABLE(ios(11.0), tvos(11.0)) {
  return [UIFocusSystem environment:environment containsEnvironment:otherEnvironment];
}
#endif // TARGET_OS_TV || TARGET_OS_IOS

#if __has_feature(nullability)
#pragma clang assume_nonnull end
#endif


#endif // SWIFT_STDLIB_SHIMS_UIKIT_OVERLAY_H

