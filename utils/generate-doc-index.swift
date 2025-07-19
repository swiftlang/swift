#!/usr/bin/env swift -enable-upcoming-feature BareSlashRegexLiterals

import Foundation

let usage = """
./\(CommandLine.arguments[0]) <swift-source-directory> [output-directory]

Generates index files for diagnostics notes, groups, and upcoming features.
"""

let docsDir = "userdocs/diagnostics"
let topLevelFileName = "diagnostics.md"

let notesDocFileName = "diagnostic-descriptions.md"
let notesHeader = """
# Diagnostic descriptions

<!-- This file is auto-generated via `swift swift/utils/generate-doc-index.swift` -->

Detailed explanations for various compiler diagnostics.


## Overview

Swift diagnostics are classified into errors and warnings. Warnings can only be silenced in an
intentional manner, e.g., adding `_ =` for an unused function result.

Some diagnostics have more detailed explanations available. These include a `[#Name]` inline and
reference to this documentation at the end of the compiler output on the command line, or is
presented specially within your IDE of choice. See below for the full list of these notes.


## Topics


"""

let groupsDocFileName = "diagnostic-groups.md"
let groupsHeader = """
# Diagnostic groups

<!-- This file is auto-generated via `swift swift/utils/generate-doc-index.swift` -->

Diagnostic groups allow controlling the behavior of warnings in a more precise manner.


## Overview

Diagnostic groups collect some number of diagnostics together under a common group name. This allows
for extra documentation to help explain relevant language concepts, as well as the ability to
control the behavior of warnings in a more precise manner:
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


## Topics


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


## Topics


"""

let groupsFileName = "include/swift/AST/DiagnosticGroups.def"
let groupRegex = /GROUP\(([a-zA-Z]+), ".+"\)/

let featuresFileName = "include/swift/Basic/Features.def"
let featuresRegex = /UPCOMING_FEATURE\(([a-zA-Z]+), .+\)/

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

let generator = GenerateUserDocs(swiftSourceDir: swiftSourceDir, outputDir: outputDir)
do {
  try generator.generateIndex()
} catch {
  print("error: \(error)")
  exit(1)
}

struct GenerateUserDocs {
  let swiftSourceDir: String
  let outputDir: String

  func generateIndex() throws {
    let notesHandle = try createIndex(name: notesDocFileName, header: notesHeader)
    defer { try? notesHandle.close() }

    let groupsHandle = try createIndex(name: groupsDocFileName, header: groupsHeader)
    defer { try? groupsHandle.close() }

    let featuresHandle = try createIndex(name: featuresDocFileName, header: featuresHeader)
    defer { try? featuresHandle.close() }

    let docs = try retrieveDocs().sorted { a, b in
      return a.title < b.title
    }

    for doc in docs {
      let handle: FileHandle
      switch doc.kind {
      case .note:
        handle = notesHandle
      case .group:
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

  func matches(in fileName: String, with regex: Regex<(Substring, Substring)>) throws -> Set<String> {
    let file = try String(contentsOfFile: "\(swiftSourceDir)/\(fileName)", encoding: .utf8)

    var matches: Set<String> = []
    for line in file.components(separatedBy: .newlines) {
      if let match = try? regex.firstMatch(in: line) {
        matches.insert(String(match.1))
      }
    }

    return matches
  }

  func retrieveDocs() throws -> [UserDoc] {
    let groups = try matches(in: groupsFileName, with: groupRegex)
    let features = try matches(in: featuresFileName, with: featuresRegex)

    var docs: [UserDoc] = []

    let files = try FileManager.default.contentsOfDirectory(atPath: "\(swiftSourceDir)/\(docsDir)")
    for name in files {
      if !name.hasSuffix(".md")
          || name.hasSuffix(topLevelFileName)
          || name.hasSuffix(notesDocFileName)
          || name.hasSuffix(groupsDocFileName)
          || name.hasSuffix(featuresDocFileName) {
        continue
      }

      let file = try String(contentsOfFile: "\(swiftSourceDir)/\(docsDir)/\(name)", encoding: .utf8)

      if let match = try? nameRegex.prefixMatch(in: file) {
        let groupName = String(match.name)

        let kind: UserDoc.Kind
        if features.contains(groupName) {
          kind = .feature
        } else if groups.contains(groupName) {
          kind = .group
        } else {
          kind = .note
        }

        docs.append(UserDoc(name: name, title: String(match.0), kind: kind))
      } else {
        if let newlineIndex = file.firstIndex(of: "\n") {
          docs.append(UserDoc(name: name, title: String(file[..<newlineIndex]), kind: .note))
        } else {
          docs.append(UserDoc(name: name, title: file, kind: .note))
        }
      }
    }

    return docs
  }
}

struct UserDoc {
  enum Kind {
    case note
    case group
    case feature
  }

  let name: String
  let title: String
  let kind: Kind
}
