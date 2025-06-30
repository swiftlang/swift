// --- sourcekit_fuzzer.swift - a simple code completion fuzzer ---------------
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
// ----------------------------------------------------------------------------
//
// The idea here is we start with a source file and proceed to place the cursor
// at random locations in the file, eventually visiting all locations exactly
// once in a shuffled random order.
//
// If completion at a location crashes, we run the test case through 'creduce'
// to find a minimal reproducer that also crashes (possibly with a different
// crash, but in practice all the examples I've seen continue to crash in the
// same way as creduce performs its reduction).
//
// Once creduce fully reduces a test case, we save it to a file named
// 'crash-NNN.swift', with a RUN: line suitable for placing the test case in
// 'validation-tests/IDE/crashers_2'.
//
// The overall script execution stops once all source locations in the file
// have been tested.
//
// You must first install creduce <https://embed.cs.utah.edu/creduce/>
// somewhere in your $PATH. Then, run this script as follows:
//
// swift utils/sourcekit_fuzzer/sourcekit_fuzzer.swift <build dir> <source file>
//
// - <build dir> is your Swift build directory (the one with subdirectories
//   named swift-macosx-x86_64 and llvm-macosx-x86_64).
//
// - <source file> is the source file to fuzz. Try any complex but
//   self-contained Swift file that exercises a variety of language features;
//   for example, I've had good results with the files in test/Prototypes/.
//
// TODO:
// - Add fuzzing for CursorInfo and RangeInfo
// - Get it running on Linux
// - Better error handling
// - More user-friendly output

import Darwin
import Foundation

// https://stackoverflow.com/questions/24026510/how-do-i-shuffle-an-array-in-swift/24029847
extension MutableCollection {
  /// Shuffles the contents of this collection.
  mutating func shuffle() {
    let c = count
    guard c > 1 else { return }

    for (firstUnshuffled, unshuffledCount) in zip(indices, stride(from: c, to: 1, by: -1)) {
      let d: Int = numericCast(arc4random_uniform(numericCast(unshuffledCount)))
      guard d != 0 else { continue }
      let i = index(firstUnshuffled, offsetBy: d)
      swapAt(firstUnshuffled, i)
    }
  }
}

extension String {
  func write(to path: String) throws {
    try write(to: URL(fileURLWithPath: path), atomically: true, encoding: String.Encoding.utf8)
  }
}

// Gross
enum ProcessError: Error {
  case failed
}

func run(_ args: [String]) throws -> Int32 {
  var pid: pid_t = 0

  let argv = args.map {
    $0.withCString(strdup)
  }
  defer { argv.forEach { free($0) } }

  let envp = ProcessInfo.processInfo.environment.map {
    "\($0.0)=\($0.1)".withCString(strdup)
  }
  defer { envp.forEach { free($0) } }

  let result = posix_spawn(&pid, argv[0], nil, nil, argv + [nil], envp + [nil])
  if result != 0 { throw ProcessError.failed }

  var stat: Int32 = 0
  waitpid(pid, &stat, 0)

  return stat
}

var arguments = CommandLine.arguments

// Workaround for behavior of CommandLine in script mode, where we don't drop
// the filename argument from the list.
if arguments.first == "sourcekit_fuzzer.swift" {
  arguments = Array(arguments[1...])
}

if arguments.count != 2 {
  print("Usage: sourcekit_fuzzer <build directory> <file>")
  exit(1)
}

let buildDir = arguments[0]

let notPath = "\(buildDir)/llvm-macosx-x86_64/bin/not"
let swiftIdeTestPath = "\(buildDir)/swift-macosx-x86_64/bin/swift-ide-test"
let creducePath = "/usr/local/bin/creduce"

let file = arguments[1]

let contents = try! String(contentsOfFile: file)

var offsets = Array(0 ... contents.count)
offsets.shuffle()

var good = 0
var bad = 0

for offset in offsets {
  print("TOTAL FAILURES: \(bad) out of \(bad + good)")

  let index = contents.index(contents.startIndex, offsetBy: offset)
  let prefix = contents[..<index]
  let suffix = contents[index...]
  let newContents = String(prefix + "#^A^#" + suffix)

  let sourcePath = "out\(offset).swift"
  try! newContents.write(to: sourcePath)

  let shellScriptPath = "out\(offset).sh"
  let shellScript = """
    #!/bin/sh
    \(notPath) --crash \(swiftIdeTestPath) -code-completion -code-completion-token=A -source-filename=\(sourcePath)
    """
  try! shellScript.write(to: shellScriptPath)

  defer {
    unlink(shellScriptPath)
    unlink(sourcePath)
  }

  do {
    let result = chmod(shellScriptPath, 0o700)
    if result != 0 {
      print("chmod failed")
      exit(1)
    }
  }

  do {
    let result = try! run(["./\(shellScriptPath)"])
    if result != 0 {
      good += 1
      continue
    }
  }

  do {
    // Because we invert the exit code with 'not', an exit code for 0 actually
    // indicates failure
    print("Failed at offset \(offset)")
    print("Reducing...")

    let result = try! run([creducePath, shellScriptPath, sourcePath])
    if result != 0 {
      print("creduce failed")
      exit(1)
    }

    bad += 1
  }

  do {
    let reduction = try! String(contentsOfFile: sourcePath)

    let testcasePath = "crash-\(bad).swift"
    let testcase = """
      // RUN: \(notPath) --crash \(swiftIdeTestPath) -code-completion -code-completion-token=A -source-filename=%s
      // REQUIRES: asserts

      \(reduction)
      """

    try! testcase.write(to: testcasePath)
  }
}
