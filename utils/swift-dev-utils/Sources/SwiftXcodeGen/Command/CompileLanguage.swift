//===--- CompileLanguage.swift --------------------------------------------===//
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

enum CompileLanguage: Hashable {
  case swift, cxx, c
}

extension PathProtocol {
  var language: CompileLanguage? {
    if hasExtension(.swift) {
      return .swift
    }
    if hasExtension(.cpp) {
      return .cxx
    }
    if hasExtension(.c) {
      return .c
    }
    return nil
  }
}
