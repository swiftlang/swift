// Copyright © 2026 Saleem Abdulrasool <compnerd@compnerd.org>. All rights reserved.
// SPDX-License-Identifier: BSD-3-Clause

internal struct AnalyzerError: Error, CustomStringConvertible {
  internal let description: String

  internal init(_ description: String) {
    self.description = description
  }
}
