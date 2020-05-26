//===--- ClockKitOverlayShims.h --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_CLOCKKITOVERLAY_H
#define SWIFT_STDLIB_SHIMS_CLOCKKITOVERLAY_H

@import ClockKit;

@interface CLKTextProvider (OverlaySupport)
+ (CLKTextProvider *)textProviderWithFormat:(NSString *)format arguments:(va_list)arguments API_AVAILABLE(watchos(2.0)) API_UNAVAILABLE(macos, tvos, ios);
@end

// SWIFT_STDLIB_SHIMS_CLOCKKITOVERLAY_H
#endif
