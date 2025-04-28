//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

import Synchronization

@available(SwiftStdlib 5.9, *)
internal struct _ManagedCriticalState<State: Sendable>: Sendable {
  final class StateContainer: Sendable {
    let state: Mutex<State>
    
    init(state: State) {
      self.state = Mutex(state)
    }
  }
  
  let container: StateContainer
  
  internal init(_ initial: State) {
    container = StateContainer(state: initial)
  }
  
  func withCriticalRegion<R>(
      _ critical: (inout State) throws -> R
  ) rethrows -> R {
    try container.state.withLock { state in
      try critical(&state)
    }
  }
}

@available(SwiftStdlib 5.9, *)
extension _ManagedCriticalState: @unchecked Sendable where State: Sendable { }

@available(SwiftStdlib 5.9, *)
extension _ManagedCriticalState: Identifiable {
  internal var id: ObjectIdentifier {
    ObjectIdentifier(container)
  }
}
