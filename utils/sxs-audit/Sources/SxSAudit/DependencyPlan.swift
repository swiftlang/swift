// Copyright © 2026 Saleem Abdulrasool <compnerd@compnerd.org>. All rights reserved.
// SPDX-License-Identifier: BSD-3-Clause

internal import struct FoundationEssentials.URL

internal struct FilePlan {
  internal let file: String
  internal let package: String
  internal let roots: Array<String>
  internal let closure: Array<String>
}

internal struct PackagePlan {
  internal let package: String
  internal let feature: String?
  internal let required: Array<String>
  internal let authored: Array<String>?
  internal let missing: Array<String>
  internal let extra: Array<String>
}

internal struct Plan {
  internal let files: Array<FilePlan>
  internal let packages: Array<PackagePlan>
  internal let missing: Array<String>
  internal let absent: Array<String>
  internal let incomplete: Array<String>
  internal let unmapped: Array<String>
  internal let unused: Array<String>
}

private let kExcludedDLLs: InlineArray<_, String> = [
  "concrt140",
  "msvcp140*",
  "testing",
  "_testing_foundation",
  "_testing_winsdk",
  "_testinginterop",
  "vccorlib140",
  "vcruntime140*",
  "xctest",
]

extension Span where Element == String {
  internal borrowing func excludes(_ value: String) -> Bool {
    let value = value.lowercased()
    for pattern in self {
      let pattern = pattern.lowercased()
      let matches = if pattern.hasSuffix("*") {
        value.hasPrefix(pattern.dropLast())
      } else {
        value == pattern
      }
      if matches { return true }
    }
    return false
  }
}

extension Sequence where Element == String {
  internal func sorted() -> Array<String> {
    sorted { $0.lowercased() < $1.lowercased() }
  }
}

private func mapping(_ paths: Array<URL>) throws(AnalyzerError)
    -> Dictionary<String, URL> {
  var result = Dictionary<String, URL>()
  for path in paths {
    let key = path.lastPathComponent.lowercased()
    if let previous = result[key] {
      throw AnalyzerError("case-insensitive filename collision: " +
                          "\(previous) and \(path)")
    }
    result[key] = path
  }
  return result
}

extension ImportReader {
  internal mutating func imports(of path: URL,
                                 names: Dictionary<String, String>) throws
      -> Set<String> {
    var result = Set<String>()
    for imported in try imports(of: path) {
      let file = URL(fileURLWithPath: imported)
      let name = file.stem.lowercased()
      if let canonical = names[name] {
        result.insert(canonical)
      }
    }
    return result
  }

  internal mutating func roots(of path: URL, files: Dictionary<String, URL>,
                               names: Dictionary<String, String>) throws
      -> Set<String> {
    var roots = Set<String>()
    var visited = Set<URL>()
    var queue = [path]
    var cursor = 0
    while cursor < queue.count {
      let current = queue[cursor]
      cursor += 1
      guard visited.insert(current).inserted else { continue }

      for imported in try imports(of: current) {
        let file = URL(fileURLWithPath: imported)
        if let canonical = names[file.stem.lowercased()] {
          roots.insert(canonical)
        } else if let local = files[file.lastPathComponent.lowercased()] {
          queue.append(local)
        }
      }
    }
    return roots
  }
}

private func assemblies(in root: URL) throws(AnalyzerError) -> Array<URL> {
  try root.children().compactMap { directory in
    guard directory.directory else { return nil }
    let assembly = directory
      .appendingPathComponent(directory.lastPathComponent)
      .appendingPathExtension("dll")
    return assembly.file ? assembly : nil
  }
}

private func closure(of roots: Set<String>,
                     graph: Dictionary<String, Set<String>>) -> Set<String> {
  var result = Set<String>()
  var queue = Array(roots)
  var cursor = 0
  while cursor < queue.count {
    let current = queue[cursor]
    cursor += 1
    guard result.insert(current).inserted else { continue }
    queue.append(contentsOf: graph[current] ?? [])
  }
  return result
}

internal func plan(root: URL, runtime: URL, packages: Array<PackageInput>,
                   exclusions: Array<String>) throws
    -> Plan {
  var provisions = Dictionary<String, String>()
  for package in packages {
    for (folded, install) in package.files {
      if let previous = provisions[folded],
          previous != package.name {
        throw AnalyzerError("\(install) is referenced by both " +
                            "\(previous) and \(package.name)")
      }
      provisions[folded] = package.name
    }
  }

  let runtimes = try runtime.children(withExtension: "dll")
    .filter {
      !kExcludedDLLs.span.excludes($0.stem) &&
        !exclusions.span.excludes($0.stem)
    }
  guard !runtimes.isEmpty else {
    throw AnalyzerError("no runtime DLLs found in \(runtime.path)")
  }

  var files = try mapping(runtimes)
  var incomplete = Set<String>()
  for path in try assemblies(in: root)
      where !kExcludedDLLs.span.excludes(path.stem) &&
        !exclusions.span.excludes(path.stem) {
    let key = path.lastPathComponent.lowercased()
    if files[key] == nil {
      files[key] = path
      incomplete.insert(path.stem)
    }
  }
  let names = Dictionary(uniqueKeysWithValues: files.map { key, path in
    (String(key.dropLast(4)), path.stem)
  })
  let installed = try mapping(root.children(withExtension: "exe") +
                              root.children(withExtension: "dll"))
  var reader = ImportReader()

  var graph = Dictionary<String, Set<String>>()
  for path in files.values {
    graph[path.stem] = try reader.imports(of: path, names: names)
  }

  var plans = Array<FilePlan>()
  var missing = Array<String>()
  var needs = Dictionary<String, Set<String>>()
  for (folded, package) in provisions.sorted(by: { $0.key < $1.key }) {
    guard let path = installed[folded] else {
      missing.append(folded)
      continue
    }
    let roots = try reader.roots(of: path, files: installed, names: names)
    let closure = closure(of: roots, graph: graph)
    needs[package, default: []].formUnion(closure)
    plans.append(FilePlan(file: path.lastPathComponent, package: package,
                          roots: roots.sorted(), closure: closure.sorted()))
  }

  var unmapped = Array<String>()
  for (folded, path) in installed.sorted(by: { $0.key < $1.key })
      where provisions[folded] == nil {
    let roots = try reader.roots(of: path, files: installed, names: names)
    if !roots.isEmpty { unmapped.append(path.lastPathComponent) }
  }

  var features = Array<PackagePlan>()
  let auditing = packages.contains { $0.authored != nil }
  var absent = Array<String>()
  var used = Set<String>()
  for package in packages {
    let required = needs[package.name] ?? []
    let authored = package.authored
    if auditing, package.feature == nil {
      absent.append(package.name)
    }
    used.formUnion(required)
    features.append(PackagePlan(package: package.name, feature: package.feature,
                                required: required.sorted(),
                                authored: authored.map { $0.sorted() },
                                missing: authored.map {
                                  required.subtracting($0).sorted()
                                } ?? [],
                                extra: authored.map {
                                  $0.subtracting(required).sorted()
                                } ?? []))
  }

  let order = Dictionary(uniqueKeysWithValues: packages.enumerated().map {
    ($0.element.name, $0.offset)
  })
  plans.sort {
    let lhs = order[$0.package] ?? Int.max
    let rhs = order[$1.package] ?? Int.max
    return if lhs == rhs {
      $0.file.lowercased() < $1.file.lowercased()
    } else {
      lhs < rhs
    }
  }

  return Plan(files: plans, packages: features, missing: missing.sorted(),
              absent: absent.sorted(),
              incomplete: incomplete.sorted(),
              unmapped: unmapped.sorted(),
              unused: Set(names.values).subtracting(used).sorted())
}
