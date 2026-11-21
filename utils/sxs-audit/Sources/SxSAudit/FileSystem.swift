// Copyright © 2026 Saleem Abdulrasool <compnerd@compnerd.org>. All rights reserved.
// SPDX-License-Identifier: BSD-3-Clause

internal import class FoundationEssentials.FileManager
internal import struct FoundationEssentials.Data
internal import struct FoundationEssentials.URL

extension URL {
  internal var stem: String {
    deletingPathExtension().lastPathComponent
  }

  internal var directory: Bool {
    var directory = false
    return FileManager.default.fileExists(atPath: path,
                                          isDirectory: &directory) &&
      directory
  }

  internal var file: Bool {
    var directory = false
    return FileManager.default.fileExists(atPath: path,
                                          isDirectory: &directory) &&
      !directory
  }

  internal func children() throws(AnalyzerError) -> Array<URL> {
    let names: Array<String>
    do {
      names = try FileManager.default.contentsOfDirectory(atPath: path)
    } catch {
      throw AnalyzerError("unable to enumerate \(path): \(error)")
    }
    return names.map { appendingPathComponent($0) }
  }

  internal func children(withExtension extension: String)
      throws(AnalyzerError) -> Array<URL> {
    try children()
      .filter { $0.pathExtension.lowercased() == `extension`.lowercased() }
  }
}

internal func contents(_ url: URL) throws(AnalyzerError) -> Data {
  do {
    return try Data(contentsOf: url, options: .mappedIfSafe)
  } catch {
    throw AnalyzerError("unable to read \(url.path): \(error)")
  }
}
