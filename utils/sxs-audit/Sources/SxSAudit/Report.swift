// Copyright © 2026 Saleem Abdulrasool <compnerd@compnerd.org>. All rights reserved.
// SPDX-License-Identifier: BSD-3-Clause

private func field(_ value: String) -> String {
  guard value.contains(where: {
    $0 == "," || $0 == "\"" || $0 == "\r" || $0 == "\n"
  }) else {
    return value
  }

  var escaped = "\""
  for character in value {
    if character == "\"" { escaped.append("\"") }
    escaped.append(character)
  }
  escaped.append("\"")
  return escaped
}

private func row(_ record: String, _ package: String, _ subject: String,
                 _ category: String,
                 _ value: String) -> String {
  "\(field(record)),\(field(package)),\(field(subject))," +
    "\(field(category)),\(field(value))"
}

private func append(_ lines: inout Array<String>, record: String,
                    package: String = "",
                    subject: String = "",
                    category: String,
                    values: Array<String>) {
  for value in values {
    lines.append(row(record, package, subject, category, value))
  }
}

internal func report(_ plan: Plan) -> String {
  var lines = ["record,package,subject,category,value"]
  for file in plan.files {
    append(&lines, record: "file", package: file.package, subject: file.file,
           category: "runtime-root", values: file.roots)
    append(&lines, record: "file", package: file.package, subject: file.file,
           category: "runtime-closure", values: file.closure)
  }

  for package in plan.packages {
    let subject = package.feature ?? ""
    append(&lines, record: "package", package: package.package,
           subject: subject, category: "required", values: package.required)
    if let authored = package.authored {
      append(&lines, record: "package", package: package.package,
             subject: subject, category: "authored", values: authored)
      append(&lines, record: "package", package: package.package,
             subject: subject, category: "missing", values: package.missing)
      append(&lines, record: "package", package: package.package,
             subject: subject, category: "extra", values: package.extra)
    }
  }

  append(&lines, record: "diagnostic",
         category: "missing-referenced-package-files", values: plan.missing)
  append(&lines, record: "diagnostic",
         category: "missing-authored-features", values: plan.absent)
  append(&lines, record: "diagnostic",
         category: "missing-flat-runtime-assemblies",
         values: plan.incomplete)
  append(&lines, record: "diagnostic",
         category: "unmapped-runtime-dependent-files",
         values: plan.unmapped)
  append(&lines, record: "diagnostic",
         category: "unused-runtime-assemblies", values: plan.unused)
  return lines.joined(separator: "\r\n") + "\r\n"
}
