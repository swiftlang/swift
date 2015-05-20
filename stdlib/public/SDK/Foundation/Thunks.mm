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

#import <Foundation/Foundation.h>

extern "C" void
NS_Swift_NSUndoManager_registerUndoWithTargetHandler(
    id NS_RELEASES_ARGUMENT __nonnull self_,
    id NS_RELEASES_ARGUMENT __nonnull target,
    void (^__nonnull handler)(id __nonnull)) {

  NSUndoManager *undoManager = self_;
  [undoManager registerUndoWithTarget:target handler:handler];
  
  [self_ release];
  [target release];
  [handler release];
}
