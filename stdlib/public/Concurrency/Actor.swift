//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift
@_implementationOnly import _SwiftConcurrencyShims

/// Common protocol to which all actor classes conform.
///
/// The \c Actor protocol provides the core functionality of an actor class,
/// which involves enqueuing new partial tasks to be executed at some
/// point. Actor classes implicitly conform to this protocol as part of their
/// primary class definition.
public protocol Actor: AnyObject {
  /// Enqueue a new partial task that will be executed in the actor's context.
  func enqueue(partialTask: PartialAsyncTask)
}
