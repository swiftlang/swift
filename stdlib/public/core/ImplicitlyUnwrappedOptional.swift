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

/// An optional type that allows implicit member access.
///
/// The `ImplicitlyUnwrappedOptional` type is deprecated. To create an optional
/// value that is implicitly unwrapped, place an exclamation mark (`!`) after
/// the type that you want to denote as optional.
///
///     // An implicitly unwrapped optional integer
///     let guaranteedNumber: Int! = 6
///
///     // An optional integer
///     let possibleNumber: Int? = 5
@available(*, unavailable, renamed: "Optional")
public typealias ImplicitlyUnwrappedOptional<Wrapped> = Optional<Wrapped>
