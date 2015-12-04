//===--- Notifications.h - SIL Undef Value Representation -------*- C++ -*-===//
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

#ifndef SWIFT_SIL_NOTIFICATIONS_H
#define SWIFT_SIL_NOTIFICATIONS_H

namespace swift {

class SILInstruction;

/// A protocol (or inferface) for handling instruction deletion notifications.
///
/// This class is used as a base class for any class that need to accept
/// instruction deletion notification messages. This is used by passes and
/// analysis that need to invalidate data structures that contain pointers.
/// This is similar to LLVM's ValueHandle.
struct DeleteNotificationHandler {

  DeleteNotificationHandler() { }
  virtual ~DeleteNotificationHandler() {}

  /// Handle the invalidation message for the value \p Value.
  virtual void handleDeleteNotification(swift::SILInstruction *Instr) { }
};

} // end swift namespace

#endif

