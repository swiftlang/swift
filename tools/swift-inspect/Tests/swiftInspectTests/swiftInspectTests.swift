//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import XCTest
import class Foundation.Bundle
import libswiftInspect

final class swiftInspectTests: XCTestCase {
  func testNoConcurrencyActivity() throws {
    let (tasks, _, threads) = try dumpConcurrency()
    XCTAssertEqual(tasks, [""])
    // FIXME: sometimes we get a bunch of objects misinterpreted as actors in the actor output
    XCTAssertEqual(threads, ["  no threads with active tasks"])
  }

  func testBasicOutput() async throws {
    actor BasicOutputTestActor {}
    let actor = BasicOutputTestActor()
    defer { withExtendedLifetime(actor) {} }

    let (tasks, actors, threads) = try dumpConcurrency()

    XCTAssertGreaterThan(count(ofSubstring: "Task 0x", in: tasks), 0,
                         "No tasks present in task output: \(tasks.joined(separator: "\n"))")
    XCTAssertGreaterThan(count(ofSubstring: "BasicOutputTestActor", in: actors), 0,
                         "No actors present in actor output: \(actors.joined(separator: "\n"))")
    XCTAssertGreaterThan(count(ofSubstring: "Thread 0x", in: threads), 0,
                         "No threads present in thread output: \(threads.joined(separator: "\n"))")
  }

  private func dumpConcurrency() throws
      -> (tasks: [String], actors: [String], threads: [String]) {
#if os(Windows)
    let pid = "\(GetProcessId(GetCurrentProcess()))"
#else
    let pid = "\(getpid())"
#endif

    var options = InspectOptions()
    options.nameOrPid = pid
#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
    options.forkCorpse = true
#endif

    var taskOutput: [String] = []
    var actorOutput: [String] = []
    var threadOutput: [String] = []
    try inspect(options: options) { process in
      var output: [String] = []
#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
      let dumper = ConcurrencyDumper(context: process.context,
                                     process: process as! DarwinRemoteProcess,
                                     printer: { output.append($0) })
#else
      // TODO
#endif

      dumper.dumpTasks()
      taskOutput = output
      XCTAssertEqual(taskOutput[0], "TASKS")
      taskOutput.removeFirst()
      output = []

      dumper.dumpActors()
      actorOutput = output
      XCTAssertEqual(actorOutput[0], "ACTORS")
      actorOutput.removeFirst()
      output = []

      dumper.dumpThreads()
      threadOutput = output
      XCTAssertEqual(threadOutput[0], "THREADS")
      threadOutput.removeFirst()
      output = []
    }
    return (taskOutput, actorOutput, threadOutput)
  }

  /// Returns path to the built products directory.
  var productsDirectory: URL {
    #if os(macOS)
      for bundle in Bundle.allBundles where bundle.bundlePath.hasSuffix(".xctest") {
        return bundle.bundleURL.deletingLastPathComponent()
      }
      fatalError("couldn't find the products directory")
    #else
      return Bundle.main.bundleURL
    #endif
  }

  enum TestError: Error {
    case castFailure(Any, Any.Type)
    case requiredStringNotPresent(String)
  }

  func checkedCast<Source, Target>(_ source: Source) throws -> Target {
    if let cast = source as? Target {
      return cast
    }
    throw TestError.castFailure(source, Target.self)
  }

  static var allTests = [
    ("testNoConcurrencyActivity", testNoConcurrencyActivity),
  ]
}

  fileprivate func count(ofSubstring substring: String, in strings: [String]) -> Int {
    var count = 0
    for str in strings {
      if str.contains(substring) {
        count += 1
      }
    }
    return count
  }
