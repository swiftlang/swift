#!/usr/bin/env swift -enable-upcoming-feature BareSlashRegexLiterals

import Foundation

let usage = """
./\(CommandLine.arguments[0]) <swift-source-directory> [output-directory]

Generates index files for diagnostics groups and upcoming features.
"""

let docsDir = "userdocs/diagnostics"
let topLevelFileName = "diagnostics.md"

let groupsDocFileName = "diagnostic-groups.md"
let groupsHeader = """
# Diagnostic groups

<!-- This file is auto-generated via `swift swift/utils/generate-doc-index.swift` -->

Detailed explanations for various compiler diagnostics.


## Overview

Diagnostic groups collect some number of diagnostics together under a common group name. This allows
for extra documentation to help explain relevant language concepts, as well as the ability to
control the behavior of warnings in a more precise manner (when that group contains warnings):
- `-Werror <group>` - upgrades warnings in the specified group to errors
- `-Wwarning <group>` - indicates that warnings in the specified group should remain warnings, even
  if they were previously upgraded to errors

As a concrete example, to upgrade deprecated declaration warnings to errors:
```sh
-Werror DeprecatedDeclaration
```

Or upgrade all warnings except deprecated declaration to errors:
```sh
-warnings-as-errors -Wwarning DeprecatedDeclaration
```
"""

let featuresDocFileName = "upcoming-language-features.md"
let featuresHeader = """
# Upcoming language features

<!-- This file is auto-generated via `swift swift/utils/generate-doc-index.swift` -->

Upcoming language features enable new (but potentially source breaking) functionality that be
enabled by default in an upcoming language mode.


## Overview

Upcoming language features allow the incremental adoption of language features that would otherwise
only be available in a new language mode, without having to fully migrate to that mode. They can be
enabled on the command line with `-enable-upcoming-feature <feature>`.

Some upcoming features have an additional "migration" mode, where the compiler will emit warnings
with fix-its to help migrate to that mode. This can be enabled with `-enable-upcoming-feature
<feature>:migrate`.
"""

let topicsHeader = "\n\n## Topics\n"

let swiftIncludeDir = "include/swift"

let groupsFileName = "\(swiftIncludeDir)/AST/DiagnosticGroups.def"
let groupRegex = /GROUP\((?<name>[a-zA-Z]+),[^,]+,"(?<file>.+)"\)/

let featuresFileName = "\(swiftIncludeDir)/Basic/Features.def"
let featuresRegex = /UPCOMING_FEATURE\((?<name>[a-zA-Z]+), .+\)/

let nameRegex = /# .+ \((?<name>[a-zA-Z]+)\)/

var args = CommandLine.arguments.dropFirst()
if args.count != 1 && args.count != 2 {
  print(usage)
  exit(2)
}

let swiftSourceDir = args.removeFirst()
let outputDir: String
if !args.isEmpty {
  outputDir = args.removeFirst()
} else {
  outputDir = "\(swiftSourceDir)/\(docsDir)"
}

do {
  try generateIndex()
} catch {
  print("error: \(error)")
  exit(1)
}

func generateIndex() throws {
  let groupsWithWarnings = try groupNamesWithWarnings()
  let docs = try retrieveDocs(groupsWithWarnings).sorted { a, b in
    return a.title < b.title
  }

  let groupsHandle = try createIndex(name: groupsDocFileName, header: groupsHeader)
  defer { try? groupsHandle.close() }

  let featuresHandle = try createIndex(name: featuresDocFileName, header: featuresHeader)
  defer { try? featuresHandle.close() }

  try groupsHandle.write(contentsOf: "\n\n## Groups with warnings\n".data(using: .utf8)!)
  for doc in docs where doc.kind == .groupWithWarnings {
    let ref = "- <doc:\(doc.name.dropLast(3))>\n"
    try groupsHandle.write(contentsOf: ref.data(using: .utf8)!)
  }

  try groupsHandle.write(contentsOf: topicsHeader.data(using: .utf8)!)
  try featuresHandle.write(contentsOf: topicsHeader.data(using: .utf8)!)

  for doc in docs {
    let handle: FileHandle
    switch doc.kind {
    case .group, .groupWithWarnings:
      handle = groupsHandle
    case .feature:
      handle = featuresHandle
    }

    let ref = "- <doc:\(doc.name.dropLast(3))>\n"
    try handle.write(contentsOf: ref.data(using: .utf8)!)
  }
}

func createIndex(name: String, header: String) throws -> FileHandle {
  let path = "\(outputDir)/\(name)"

  if FileManager.default.fileExists(atPath: path) {
    try FileManager.default.removeItem(atPath: path)
  }
  FileManager.default.createFile(atPath: path, contents: nil)

  let handle = try FileHandle(forWritingTo: URL(filePath: path))
  try handle.write(contentsOf: header.data(using: .utf8)!)
  return handle
}

func retrieveDocs(_ groupsWithWarnings: Set<String>) throws -> [UserDoc] {
  let groups = Dictionary(try matches(in: "\(swiftSourceDir)/\(groupsFileName)", with: groupRegex) {
    (file: String($0.file), name: String($0.name))
  }, uniquingKeysWith: { a, b in a })
  let features = Set(try matches(in: "\(swiftSourceDir)/\(featuresFileName)", with: featuresRegex) {
    String($0.1)
  })

  var docs: [UserDoc] = []

  let files = try FileManager.default.contentsOfDirectory(atPath: "\(swiftSourceDir)/\(docsDir)")
  for name in files {
    if !name.hasSuffix(".md")
        || name.hasSuffix(topLevelFileName)
        || name.hasSuffix(groupsDocFileName)
        || name.hasSuffix(featuresDocFileName) {
      continue
    }

    guard let groupName = groups[String(name.dropLast(3))] else {
      throw GenerationError.unknownGroup(file: name)
    }

    let path = try String(contentsOfFile: "\(swiftSourceDir)/\(docsDir)/\(name)", encoding: .utf8)
    guard let match = try? nameRegex.prefixMatch(in: path) else {
      throw GenerationError.missingGroup(name: groupName, file: name)
    }

    let titleGroupName = String(match.name)
    if groupName != titleGroupName {
      throw GenerationError.incorrectGroup(defsName: groupName, titleName: titleGroupName, file: name)
    }

    let kind: UserDoc.Kind
    if features.contains(groupName) {
      kind = .feature
    } else if groupsWithWarnings.contains(groupName) {
      kind = .groupWithWarnings
    } else {
      kind = .group
    }

    docs.append(UserDoc(name: name, title: String(match.0), kind: kind))
  }

  return docs
}

func groupNamesWithWarnings() throws -> Set<String> {
  let includePath = "\(swiftSourceDir)/\(swiftIncludeDir)"
  let defPaths = try FileManager.default.subpathsOfDirectory(atPath: includePath)
    .compactMap { subpath in
      if subpath.hasSuffix(".def") {
        return "\(includePath)/\(subpath)"
      }
      return nil
    }

  enum WarningGroupState {
    case outside, inside, name
  }

  var groups: Set<String> = []
  for path in defPaths {
    let file = try String(contentsOfFile: path, encoding: .utf8)

    var state = WarningGroupState.outside
    for line in file.components(separatedBy: .newlines) {
      var line = Substring(line)

      switch state {
      case .outside:
        if !line.hasPrefix("GROUPED_WARNING") {
          continue
        }

        state = .inside
        fallthrough

      case .inside:
        guard let index = line.firstIndex(of: ",") else {
          continue
        }
        line = line[index...].dropFirst()

        state = .name
        fallthrough

      case .name:
        if let index = line.firstIndex(of: ",") {
          line = line[..<index]
        }
        line = line.trimmingPrefix { $0.isWhitespace }

        if line.isEmpty {
          continue
        }

        groups.insert(String(line))
        state = .outside
      }
    }
  }

  return groups
}

func matches<R, E>(in path: String, with regex: Regex<R>, _ transform: (Regex<R>.Match) -> E) throws -> [E] {
  let file = try String(contentsOfFile: path, encoding: .utf8)

  var matches = [E]()
  for line in file.components(separatedBy: .newlines) {
    if let match = try? regex.firstMatch(in: line) {
      matches.append(transform(match))
    }
  }

  return matches
}

struct UserDoc {
  enum Kind {
    case group
    case groupWithWarnings
    case feature
  }

  let name: String
  let title: String
  let kind: Kind
}

enum GenerationError: CustomStringConvertible, Error {
  case incorrectGroup(defsName: String, titleName: String, file: String)
  case missingGroup(name:String, file: String)
  case unknownGroup(file: String)

  var description: String {
    switch self {
    case .incorrectGroup(let defsName, let titleName, let file):
      return "The title in '\(file)' contains the name '\(titleName)', but it should be '\(defsName)' as per 'DiagnosticGroups.def'"
    case .missingGroup(let name, let file):
      return "The title in '\(file)' does not end with a group name, add ' (\(name))' to the end of the title"
    case .unknownGroup(let file):
      return "'\(file)' has no corresponding listing in 'DiagnosticGroups.def'"
    }
  }
}
