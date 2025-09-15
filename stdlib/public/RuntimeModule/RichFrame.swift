//===--- RichFrame.swift --------------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Defines the default rich frame storage type used by `Backtrace`
//
//===----------------------------------------------------------------------===//

import Swift

@_spi(Internal)
public enum RichFrame<T: FixedWidthInteger>: CustomStringConvertible, Equatable {
  public typealias Address = T

  /// A program counter value.
  ///
  /// This might come from a signal handler, or an exception or some
  /// other situation in which we have captured the actual program counter.
  ///
  /// These can be directly symbolicated, as-is, with no adjustment.
  case programCounter(Address)

  /// A return address.
  ///
  /// Corresponds to a normal function call.
  ///
  /// Requires adjustment when symbolicating for a backtrace, because it
  /// points at the address after the one that triggered the child frame.
  case returnAddress(Address)

  /// An async resume point.
  ///
  /// Corresponds to an `await` in an async task.
  ///
  /// Can be directly symbolicated, as-is.
  case asyncResumePoint(Address)

  /// Indicates a discontinuity in the backtrace.
  ///
  /// This occurs when you set a limit and a minimum number of frames at
  /// the top.  For example, if you set a limit of 10 frames and a minimum
  /// of 4 top frames, but the backtrace generated 100 frames, you will see
  ///
  ///    0: frame 100 <----- bottom of call stack
  ///    1: frame 99
  ///    2: frame 98
  ///    3: frame 97
  ///    4: frame 96
  ///    5: ...       <----- omittedFrames(92)
  ///    6: frame 3
  ///    7: frame 2
  ///    8: frame 1
  ///    9: frame 0   <----- top of call stack
  ///
  /// Note that the limit *includes* the discontinuity.
  ///
  /// This is good for handling cases involving deep recursion.
  case omittedFrames(Int)

  /// Indicates a discontinuity of unknown length.
  ///
  /// This can only be present at the end of a backtrace; in other cases
  /// we will know how many frames we have omitted.  For instance,
  ///
  ///    0: frame 100 <----- bottom of call stack
  ///    1: frame 99
  ///    2: frame 98
  ///    3: frame 97
  ///    4: frame 96
  ///    5: ...       <----- truncated
  case truncated

  /// The program counter, without any adjustment.
  public var originalProgramCounter: Address {
    switch self {
      case let .returnAddress(addr):
        return addr
      case let .programCounter(addr):
        return addr
      case let .asyncResumePoint(addr):
        return addr
      case .omittedFrames, .truncated:
        return 0
    }
  }

  /// The adjusted program counter to use for symbolication.
  public var adjustedProgramCounter: Address {
    switch self {
      case let .returnAddress(addr):
        return addr - 1
      case let .programCounter(addr):
        return addr
      case let .asyncResumePoint(addr):
        return addr
      case .omittedFrames, .truncated:
        return 0
    }
  }

  /// A textual description of this frame.
  public var description: String {
    switch self {
      case let .programCounter(addr):
        return "\(hex(addr))"
      case let .returnAddress(addr):
        return "\(hex(addr)) [ra]"
      case let .asyncResumePoint(addr):
        return "\(hex(addr)) [async]"
      case .omittedFrames, .truncated:
        return "..."
    }
  }
}

extension RichFrame: LimitableElement {
  // LimitableElement wants to call this "omitted"
  public static func omitted(_ count: Int) -> Self {
    return .omittedFrames(count)
  }
}

extension Backtrace.Frame {
  init<T>(_ frame: RichFrame<T>) {
    switch frame {
      case let .returnAddress(addr):
        self = .returnAddress(Backtrace.Address(addr)!)
      case let .programCounter(addr):
        self = .programCounter(Backtrace.Address(addr)!)
      case let .asyncResumePoint(addr):
        self = .asyncResumePoint(Backtrace.Address(addr)!)
      case let .omittedFrames(count):
        self = .omittedFrames(count)
      case .truncated:
        self = .truncated
    }
  }
}
