//===----------------------------------------------------------------------===//
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

#if os(Windows)
private import WinSDK
#endif

/// Manages COM initialisation on the current thread with explicit lifetime.
///
/// `COMContext` calls `CoInitializeEx` at construction and `CoUninitialize` at
/// deallocation. It is suited to long-lived programs where wrapping the entire
/// body in a `withCOMContext` closure is impractical.
///
/// ```swift
/// func main() throws {
///     let com = try COMContext(.apartment)
///
///     let voice = try SpVoice()
///     try voice.speak("Hello")
///     processEvents()
///     // ...
/// }
/// // CoUninitialize called when `com` goes out of scope
/// ```
///
/// For scoped usage, prefer `withCOMContext(_:operation:)`. For entry-point
/// programs, prefer `@COMMain`.
///
/// `COMContext` is non-copyable because `CoInitializeEx`/`CoUninitialize` are
/// paired per-thread and must not be duplicated.
public struct COMContext: ~Copyable {
  /// Initialises COM on the current thread with the specified threading model.
  ///
  /// - Parameter model: The apartment threading model for this thread.
  /// - Throws: If `CoInitializeEx` fails (e.g., the thread is already initialised
  ///   with a different threading model).
  public init(_ model: COMThreadingModel) throws(COMError) {
#if os(Windows)
    let hr = CoInitializeEx(nil, DWORD(model.rawValue))
    guard hr >= 0 else { throw COMError(hr: hr) }
#endif
  }

  deinit {
#if os(Windows)
  _ = CoUninitialize()
#endif
  }
}
