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

/// The COM apartment threading model for an activatable coclass.
///
/// The threading model declares how the COM runtime should handle concurrent
/// access to instances of the class. It is specified via
/// `@com(implementation:, threading:)` and written to the Windows registry by
/// the synthesised `DllRegisterServer` (for DLL servers) or used with
/// `CoRegisterClassObject` (for EXE servers).
///
/// The threading model is only meaningful on `@com` classes that carry a
/// `CLSID:`. Bare `@com` classes without a `CLSID:` are not registered with
/// the COM runtime and express their thread safety through Swift's own
/// concurrency model (actors, `Sendable`).
///
/// On `@com` actors, `.apartment` is implied and `.free` or `.both` is a
/// compile-time error, because actor isolation already provides the
/// serialisation that apartment threading requires.
public enum COMThreadingModel: Int {
  /// Single threading. The object lives on the main STA thread (thread 0)
  /// and all calls are marshalled there. This is the most restrictive model,
  /// used for legacy objects that must run on the process's main thread.
  /// Corresponds to `@MainActor` semantics in Swift.
  case single

  /// Apartment threading. The object lives on a single STA thread, and COM
  /// serialises calls through that thread's message loop. Only one thread
  /// may call the object at a time. This is the default threading model.
  case apartment

  /// Free threading. Any thread may call the object concurrently.
  /// The implementation must provide its own synchronisation.
  case free

  /// Compatible with both apartment and free threading. COM may place the
  /// object in whichever apartment type the caller is in. The implementation
  /// must be thread-safe.
  case both

  /// Neutral Apartment (COM+). Calls execute on the calling thread without
  /// thread switching or message-loop involvement. The implementation must
  /// be thread-safe.
  case neutral
}

extension COMThreadingModel {
  /// Alias for `.apartment`.
  ///
  /// Preserve `STA` alias as it is what developers use. `apartment` is the
  /// proper name, and `STA` is the mirror for `MTA` which was introduced with
  /// the multithreaded COM.
  @available(*, deprecated, renamed: "apartment")
  public static var sta: Self { .apartment }

  /// Alias for `.free`.
  ///
  /// Preserve `MTA` alias as it is what developers use. `free` is the
  /// proper name.
  @available(*, deprecated, renamed: "free")
  public static var mta: Self { .free }
}

extension COMThreadingModel: Sendable {
}
