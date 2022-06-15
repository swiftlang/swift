//===--- SyntaxRegexFallbackLexing.h --------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

namespace swift {
/// SwiftSyntax parsing currently doesn't link against
/// SwiftExperimentalStringProcessing and is thus missing the regex lexing
/// functions defined in it. This registers a fallback regex-lexing function
/// implemented in C++ that is sufficient to generate a valid SwiftSyntax tree.
/// The regex parser registered by this function will accept all regex literals
/// and is not suited for normal compilation.
void registerSyntaxFallbackRegexParser();
} // end namespace swift
