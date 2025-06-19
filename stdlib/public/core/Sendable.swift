//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A type whose metatype can be shared across arbitrary concurrent contexts
/// without introducing a risk of data races. When a generic type `T` conforms
/// to `SendableMetatype`, its metatype `T.Type` conforms to `Sendable`.  All
/// concrete types implicitly conform to the `SendableMetatype` protocol, so its
/// primary purpose is in generic code to prohibit the use of isolated
/// conformances along with the generic type.
///
/// A generic type `T` will need a `SendableMetatype` conformance when its
/// metatype is shared across concurrency boundaries. For example,
///
///     protocol P {
///       static func f()
///     }
///
///     func useFromAnotherTask<T: P>(_: T.Type) {
///       Task { @concurrent in
///         T.f()   // error: capturing non-Sendable type `T.Type` in a concurrently-executing task
///       }
///     }
///
/// The potential data race above would occur when `useFromAnotherTask` is
/// provided with an isolated conformance to `P`. For example:
///
///     @MainActor
///     class MyModel: @MainActor P {
///       /*implicitly @MainActor/*
///       static func f() {
///         /* on the main actor */
///       }
///     }
///
///     useFromAnotherTask(MyModel.self)
///
/// Here, the error within the body of `useFromAnotherTask` is preventing the
/// isolated conformance from leaving the current task and actor. The signature
/// of `useFromAnotherTask` can be adjusted to introduce a requirement on
/// `SendableMetatype`:
///
///     func useFromAnotherTask<T: P & SendableMetatype>(_: T.Type) {
///       Task { @concurrent in
///         T.f()   // error: okay, T.Type is Sendable
///       }
///     }
///
///     useFromAnotherTask(MyModel.self) // error: cannot use main-actor-isolated conformance `MyModel: P` for a `SendableMetatype`-conforming type parameter `T`
///
/// The `Sendable` protocol inherits from `SendableMetatype`, so any generic
/// type `T` with a requirement `T: Sendable` will have the implied requirement
/// `T: SendableMetatype`.
@_marker public protocol SendableMetatype: ~Copyable, ~Escapable { }

/// A thread-safe type whose values can be shared across arbitrary concurrent
/// contexts without introducing a risk of data races. Values of the type may
/// have no shared mutable state, or they may protect that state with a lock or
/// by forcing it to only be accessed from a specific actor.
///
/// You can safely pass values of a sendable type
/// from one concurrency domain to another ---
/// for example, you can pass a sendable value as the argument
/// when calling an actor's methods.
/// All of the following can be marked as sendable:
///
/// - Value types
///
/// - Reference types with no mutable storage
///
/// - Reference types that internally manage access to their state
///
/// - Functions and closures (by marking them with `@Sendable`)
///
/// Although this protocol doesn't have any required methods or properties,
/// it does have semantic requirements that are enforced at compile time.
/// These requirements are listed in the sections below.
/// Conformance to `Sendable` must be declared
/// in the same file as the type's declaration.
///
/// To declare conformance to `Sendable` without any compiler enforcement,
/// write `@unchecked Sendable`.
/// You are responsible for the correctness of unchecked sendable types,
/// for example, by protecting all access to its state with a lock or a queue.
/// Unchecked conformance to `Sendable` also disables enforcement
/// of the rule that conformance must be in the same file.
///
/// For information about the language-level concurrency model that `Task` is part of,
/// see [Concurrency][concurrency] in [The Swift Programming Language][tspl].
///
/// [concurrency]: https://docs.swift.org/swift-book/LanguageGuide/Concurrency.html
/// [tspl]: https://docs.swift.org/swift-book/
///
/// ### Sendable Structures and Enumerations
///
/// To satisfy the requirements of the `Sendable` protocol,
/// an enumeration or structure must have only sendable
/// members and associated values.
/// In some cases, structures and enumerations
/// that satisfy the requirements implicitly conform to `Sendable`:
///
/// - Frozen structures and enumerations
///
/// - Structures and enumerations
///   that aren't public and aren't marked `@usableFromInline`.
///
/// Otherwise, you need to declare conformance to `Sendable` explicitly.
///
/// Structures that have nonsendable stored properties
/// and enumerations that have nonsendable associated values
/// can be marked as `@unchecked Sendable`,
/// disabling compile-time correctness checks,
/// after you manually verify that
/// they satisfy the `Sendable` protocol's semantic requirements.
///
/// ### Sendable Actors
///
/// All actor types implicitly conform to `Sendable`
/// because actors ensure that all access to their mutable state
/// is performed sequentially.
///
/// ### Sendable Classes
///
/// To satisfy the requirements of the `Sendable` protocol,
/// a class must:
///
/// - Be marked `final`
///
/// - Contain only stored properties that are immutable and sendable
///
/// - Have no superclass or have `NSObject` as the superclass
///
/// Classes marked with `@MainActor` are implicitly sendable,
/// because the main actor coordinates all access to its state.
/// These classes can have stored properties that are mutable and nonsendable.
///
/// Classes that don't meet the requirements above
/// can be marked as `@unchecked Sendable`,
/// disabling compile-time correctness checks,
/// after you manually verify that
/// they satisfy the `Sendable` protocol's semantic requirements.
///
/// ### Sendable Functions and Closures
///
/// Instead of conforming to the `Sendable` protocol,
/// you mark sendable functions and closures with the `@Sendable` attribute.
/// Any values that the function or closure captures must be sendable.
/// In addition, sendable closures must use only by-value captures,
/// and the captured values must be of a sendable type.
///
/// In a context that expects a sendable closure,
/// a closure that satisfies the requirements
/// implicitly conforms to `Sendable` ---
/// for example, in a call to `Task.detached(priority:operation:)`.
///
/// You can explicitly mark a closure as sendable
/// by writing `@Sendable` as part of a type annotation,
/// or by writing `@Sendable` before the closure's parameters ---
/// for example:
///
///     let sendableClosure = { @Sendable (number: Int) -> String in
///         if number > 12 {
///             return "More than a dozen."
///         } else {
///             return "Less than a dozen"
///         }
///     }
///
/// ### Sendable Tuples
///
/// To satisfy the requirements of the `Sendable` protocol,
/// all of the elements of the tuple must be sendable.
/// Tuples that satisfy the requirements implicitly conform to `Sendable`.
///
/// ### Sendable Metatypes
///
/// Metatypes such as `Int.Type` implicitly conform to the `Sendable` protocol.
/// For a generic type `T`, its metatype `T.Type` does not necessarily conform
/// to `Sendable`. Please see the `SendableMetatype` protocol for more
/// information.
@_marker public protocol Sendable: SendableMetatype, ~Copyable, ~Escapable { }

///
/// A type whose values can safely be passed across concurrency domains by copying,
/// but which disables some safety checking at the conformance site.
///
/// Use an unchecked conformance to `Sendable` instead --- for example:
///
///     struct MyStructure: @unchecked Sendable { ... }
@available(*, deprecated, message: "Use @unchecked Sendable instead")
@available(swift, obsoleted: 6.0, message: "Use @unchecked Sendable instead")
@_marker public protocol UnsafeSendable: Sendable { }

// Historical names
@available(*, deprecated, renamed: "Sendable")
@available(swift, obsoleted: 6.0, renamed: "Sendable")
public typealias ConcurrentValue = Sendable

@available(*, deprecated, renamed: "Sendable")
@available(swift, obsoleted: 6.0, renamed: "Sendable")
public typealias UnsafeConcurrentValue = UnsafeSendable
