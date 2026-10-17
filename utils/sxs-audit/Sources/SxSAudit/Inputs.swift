// Copyright © 2026 Saleem Abdulrasool <compnerd@compnerd.org>. All rights reserved.
// SPDX-License-Identifier: BSD-3-Clause

internal import struct FoundationEssentials.URL

internal struct Inputs {
  internal let root: URL
  internal let runtime: URL
  internal let exclusions: Array<String>
}

internal func inputs(root: String, runtime: String, exclusions: Array<String>)
    throws(AnalyzerError) -> Inputs {
  let root = URL(fileURLWithPath: root)
  guard root.directory else {
    throw AnalyzerError("scan root not found: \(root.path)")
  }
  let runtime = URL(fileURLWithPath: runtime)
  guard runtime.directory else {
    throw AnalyzerError("runtime root not found: \(runtime.path)")
  }
  return Inputs(root: root, runtime: runtime,
                exclusions: exclusions.map {
                  URL(fileURLWithPath: $0).stem
                })
}
