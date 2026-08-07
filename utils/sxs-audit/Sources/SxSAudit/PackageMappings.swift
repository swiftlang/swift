// Copyright © 2026 Saleem Abdulrasool <compnerd@compnerd.org>. All rights reserved.
// SPDX-License-Identifier: BSD-3-Clause

internal struct PackageInput {
  internal let name: String
  internal let files: Dictionary<String, String>
  internal let feature: String?
  internal let authored: Set<String>?
}

private func mapping(_ value: String, option: String) throws(AnalyzerError)
    -> (String, String) {
  let mapping = value
  guard let separator = value.firstIndex(of: "=") else {
    throw AnalyzerError("\(option) requires PACKAGE=VALUE: \(value)")
  }
  let package = String(value[..<separator]).lowercased()
  let value = String(value[value.index(after: separator)...])
  guard !package.isEmpty, !value.isEmpty else {
    throw AnalyzerError("\(option) requires PACKAGE=VALUE: \(mapping)")
  }
  return (package, value)
}

internal func packages(files: Array<String>, features: Array<String>,
                       authored: Array<String>) throws(AnalyzerError)
    -> Array<PackageInput> {
  var order = Array<String>()
  var provisions = Dictionary<String, Dictionary<String, String>>()
  for value in files {
    let (package, file) = try mapping(value, option: "--file")
    if provisions[package] == nil {
      order.append(package)
    }
    provisions[package, default: Dictionary<String, String>()][
      file.lowercased()
    ] = file
  }
  guard !order.isEmpty else {
    throw AnalyzerError("at least one --file PACKAGE=FILE is required")
  }

  var names = Dictionary<String, String>()
  for value in features {
    let (package, feature) = try mapping(value, option: "--feature")
    guard provisions[package] != nil else {
      throw AnalyzerError("--feature references unknown package: \(package)")
    }
    if let previous = names[package], previous != feature {
      throw AnalyzerError("\(package) references both \(previous) and " +
                          "\(feature)")
    }
    names[package] = feature
  }

  var assemblies = Dictionary<String, Set<String>>()
  for value in authored {
    let (package, assembly) = try mapping(value, option: "--authored")
    guard provisions[package] != nil else {
      throw AnalyzerError("--authored references unknown package: \(package)")
    }
    guard names[package] != nil else {
      throw AnalyzerError("--authored requires --feature for \(package)")
    }
    assemblies[package, default: []].insert(assembly)
  }

  return order.map { package in
    PackageInput(name: package,
                 files: provisions[package] ?? Dictionary<String, String>(),
                 feature: names[package],
                 authored: names[package] == nil ? nil :
                   assemblies[package] ?? [])
  }
}
