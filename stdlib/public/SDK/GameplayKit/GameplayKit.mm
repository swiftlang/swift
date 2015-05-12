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

#import <GameplayKit/GameplayKit.h>

extern "C" NS_RETURNS_RETAINED __nullable GPComponent *
GP_Swift_GPEntity_componentForClass(
    id NS_RELEASES_ARGUMENT __nonnull self_,
    Class __nonnull componentClass) {
  GPEntity *entity = self_;
  id component = [[entity componentForClass:componentClass] retain];
  [self_ release];
  return component;
}

extern "C" NS_RETURNS_RETAINED __nullable GPState *
GP_Swift_GPStateMachine_stateForClass(
    id NS_RELEASES_ARGUMENT __nonnull self_,
    Class __nonnull stateClass) {
  GPStateMachine *stateMachine = self_;
  id state = [[stateMachine stateForClass:stateClass] retain];
  [self_ release];
  return state;
}
