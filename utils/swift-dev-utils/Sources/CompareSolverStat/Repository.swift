//===--- Repository.swift -------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Foundation
import System
import Utils

/// A jj or git repository, which the comparison checks, moves to main@origin
/// and moves back.
struct Repository: Sendable {
  var path: AbsolutePath
  var kind: Kind
  /// Whether `switchTo(commit:)` stashed uncommitted files, which
  /// `restore(_:)` puts back.
  private(set) var hasStash = false

  init(path: AbsolutePath, kind: Kind) {
    self.path = path
    self.kind = kind
  }
}

extension Repository {
  enum Kind: Sendable {
    case jj
    case git
  }

  /// The repository at a path, using the version control system asked for.
  ///
  /// - Throws: `ComparisonRun.Failure` when git is asked for in a colocated
  ///   jj repository without `force`, because mutating git commands would
  ///   fight with jj, or when the repository does not use the system asked
  ///   for.
  init(path: AbsolutePath, choice: Configuration.VersionControl, force: Bool)
    throws
  {
    let hasJJ = path.appending(".jj").exists
    let hasGit = path.appending(".git").exists
    let kind: Kind =
      switch choice {
      case .auto: hasJJ ? .jj : .git
      case .jj: .jj
      case .git: .git
      }
    if kind == .git && hasJJ && !force {
      throw ComparisonRun.Failure(
        "\(path) is a colocated jj repository. Mutating git commands would fight with jj. Use --vcs jj, or pass --force-vcs."
      )
    }
    if kind == .jj && !hasJJ {
      throw ComparisonRun.Failure("\(path) is not a jj repository.")
    }
    if kind == .git && !hasGit {
      throw ComparisonRun.Failure("\(path) is not a git repository.")
    }
    self.init(path: path, kind: kind)
  }
}

// MARK: - Position

extension Repository {
  /// A position the repository can be moved back to.
  enum Position: Sendable, Hashable {
    /// A jj working-copy change, which is edited again.
    case jjChange(id: String)
    /// An empty, undescribed jj working-copy change. jj abandons such a change
    /// when it is left, so a new empty change on the same parents stands in
    /// for it.
    case jjEmptyChange(parents: [String])
    /// A git branch, which is checked out again.
    case gitBranch(String)
    /// A detached git commit.
    case gitCommit(String)
  }
}

extension Repository.Position {
  /// The command that moves the repository back to this position by hand.
  var restoreCommand: String {
    switch self {
    case .jjChange(let id): "jj edit \(id)"
    case .jjEmptyChange(let parents):
      (["jj", "new"] + parents).joined(separator: " ")
    case .gitBranch(let name), .gitCommit(let name): "git checkout \(name)"
    }
  }

  /// Whether the repository is at this position. An empty jj change on the
  /// same parents counts, whatever the order of the parents.
  func isSame(as other: Self) -> Bool {
    switch (self, other) {
    case (.jjEmptyChange(let parents), .jjEmptyChange(let otherParents)):
      Set(parents) == Set(otherParents)
    default:
      self == other
    }
  }
}

extension Repository.Position: CustomStringConvertible {
  var description: String {
    switch self {
    case .jjChange(let id): id
    case .jjEmptyChange(let parents):
      "an empty change on \(parents.map { String($0.prefix(12)) }.joined(separator: ", "))"
    case .gitBranch(let name), .gitCommit(let name): name
    }
  }
}

// MARK: - Problem

extension Repository {
  /// A reason the repository is not safe to move away from.
  enum Problem: Sendable, Hashable {
    case conflicts
    case operationInProgress(String)
    case unmergedPaths
    case uncommittedFiles
  }
}

extension Repository.Problem: CustomStringConvertible {
  var description: String {
    switch self {
    case .conflicts: "the working-copy change has conflicts"
    case .operationInProgress(let state):
      "a git operation is in progress (\(state))"
    case .unmergedPaths: "there are unmerged paths"
    case .uncommittedFiles:
      "there are uncommitted or untracked files (use --allow-dirty to stash them while main is built)"
    }
  }
}

// MARK: - Checking

extension Repository {
  /// The reasons the repository is not safe to move away from, if any. Edits
  /// in a jj working-copy change are kept by jj, so only git can be dirty.
  func problems(allowDirty: Bool) async throws -> [Problem] {
    switch kind {
    case .jj:
      return try await jjLog("@ & conflicts()", "change_id").isEmpty
        ? [] : [.conflicts]
    case .git:
      var found: [Problem] = []
      for state in [
        "MERGE_HEAD", "CHERRY_PICK_HEAD", "REVERT_HEAD", "BISECT_LOG",
        "rebase-merge", "rebase-apply",
      ] {
        let statePath = try await run("rev-parse", "--git-path", state)
          .trimmingCharacters(in: .whitespacesAndNewlines)
        if AnyPath(FilePath(statePath)).absolute(in: path).exists {
          found.append(.operationInProgress(state))
        }
      }
      if try await !run("diff", "--name-only", "--diff-filter=U").isEmpty {
        found.append(.unmergedPaths)
      }
      if !allowDirty {
        if try await isDirty() {
          found.append(.uncommittedFiles)
        }
      }
      return found
    }
  }

  /// Where the repository is now.
  func currentPosition() async throws -> Position {
    switch kind {
    case .jj:
      let isEmpty = try await jjLog("@", "empty") == "true"
      let isDescribed = try await !jjLog("@", "description").isEmpty
      if isEmpty && !isDescribed {
        let parents = try await run(
          "log", "-r", "@-", "--no-graph", "-T", #"commit_id ++ "\n""#)
        return .jjEmptyChange(
          parents: parents.split(whereSeparator: \.isWhitespace).map(
            String.init))
      }
      let id = try await jjLog("@", "change_id")
      return .jjChange(id: id)
    case .git:
      // symbolic-ref fails when HEAD is detached.
      if let branch = try? await run("symbolic-ref", "-q", "--short", "HEAD")
        .trimmingCharacters(in: .whitespacesAndNewlines), !branch.isEmpty
      {
        return .gitBranch(branch)
      }
      let commit = try await run("rev-parse", "HEAD").trimmingCharacters(
        in: .whitespacesAndNewlines)
      return .gitCommit(commit)
    }
  }

  /// The commit main@origin points to.
  func mainCommit() async throws -> String {
    switch kind {
    case .jj: try await jjLog("main@origin", "commit_id")
    case .git:
      try await run("rev-parse", "origin/main").trimmingCharacters(
        in: .whitespacesAndNewlines)
    }
  }
}

// MARK: - Moving

extension Repository {
  /// Fetches from origin, with the output going to the terminal.
  func fetch() async throws {
    let arguments = kind == .jj ? ["git", "fetch"] : ["fetch", "origin"]
    try await Invocation(.named(command), arguments, in: path).runInTerminal()
  }

  /// Moves the repository to a commit. With git, uncommitted files are
  /// stashed first.
  mutating func switchTo(commit: String) async throws {
    switch kind {
    case .jj:
      // An empty child of the commit has the same tree. Unlike `jj edit` this
      // also works when the commit is immutable, and jj abandons the child
      // when the repository moves back.
      _ = try await run("new", commit)
    case .git:
      if try await isDirty() {
        _ = try await run("stash", "push", "--include-untracked")
        hasStash = true
      }
      _ = try await run("checkout", "--detach", commit)
    }
  }

  /// Moves the repository back to a position, and puts back stashed files.
  ///
  /// - Throws: `ComparisonRun.Failure` when a command fails, or when the
  ///   repository is not at the position afterwards.
  mutating func restore(_ position: Position) async throws {
    switch position {
    case .jjChange(let id):
      _ = try await run("edit", id)
    case .jjEmptyChange(let parents):
      _ = try await run(["new"] + parents)
    case .gitBranch(let name), .gitCommit(let name):
      _ = try await run("checkout", name)
      if hasStash {
        _ = try await run("stash", "pop")
        hasStash = false
      }
    }
    guard try await currentPosition().isSame(as: position) else {
      throw ComparisonRun.Failure(
        "Could not verify the restore of \(position).")
    }
  }
}

// MARK: - Commands

extension Repository {
  private var command: String {
    kind == .jj ? "jj" : "git"
  }

  private func run(_ arguments: String...) async throws -> String {
    try await run(arguments)
  }

  private func run(_ arguments: [String]) async throws -> String {
    try await Invocation(.named(command), arguments, in: path).output()
  }

  /// The output of a one-revision jj log with a template, trimmed.
  private func jjLog(_ revset: String, _ template: String) async throws
    -> String
  {
    try await run("log", "-r", revset, "--no-graph", "-T", template)
      .trimmingCharacters(in: .whitespacesAndNewlines)
  }

  private func isDirty() async throws -> Bool {
    try await !run("status", "--porcelain").trimmingCharacters(
      in: .whitespacesAndNewlines
    ).isEmpty
  }
}
