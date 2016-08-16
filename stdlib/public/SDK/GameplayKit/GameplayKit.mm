//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#import <GameplayKit/GameplayKit.h>

#include "swift/Runtime/Config.h"

extern "C" SWIFT_CC(swift) NS_RETURNS_RETAINED GKState * _Nullable
GK_Swift_GKStateMachine_stateForClass(id NS_RELEASES_ARGUMENT __nonnull self_,
                                      Class __nonnull stateClass) {
  GKStateMachine *stateMachine = self_;
  id state = [[stateMachine stateForClass:stateClass] retain];
  [self_ release];
  return state;
}
