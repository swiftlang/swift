// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-future-triple %S/../Inputs/FakeDistributedActorSystems.swift

// The signposts must come out the same way however the caller was compiled, so
// check an unoptimized and an optimized build.
// RUN: %target-build-swift -module-name main -target %target-future-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out.Onone -Onone
// RUN: %target-codesign %t/a.out.Onone
// RUN: %target-run %t/a.out.Onone | %FileCheck %s

// RUN: %target-build-swift -module-name main -target %target-future-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out.O -O
// RUN: %target-codesign %t/a.out.O
// RUN: %target-run %t/a.out.O | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: OS=macosx
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// Show what a trace of a distributed call looks like, by CHECK-ing the
// signposts 'log stream' reports for one. A 'FakeRoundtripActorSystem' drives
// both sides in this one process, so a single call produces the outbound
// signposts as well as the inbound ones.
//
// The stream is collected while the calls run and printed verbatim afterwards,
// rather than as it arrives, so the signposts land in one contiguous block that
// the actor system's own output cannot interleave with.

import Distributed
import FakeDistributedActorSystems
import Foundation

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

/// Accumulates the bytes `log stream` writes. Kept separate from
/// 'SignpostStream' so the pipe's readability handler does not have to capture
/// the object that owns the pipe.
final class StreamBuffer {
  private let lock = NSLock()
  private var data = Data()

  func append(_ more: Data) {
    lock.lock()
    data.append(more)
    lock.unlock()
  }

  var text: String {
    lock.lock()
    defer { lock.unlock() }
    return String(data: data, encoding: .utf8) ?? ""
  }
}

/// Collects everything `log stream` reports for this process on the Distributed
/// subsystem, to be printed once the traced calls are done.
final class SignpostStream {
  /// Must match 'SWIFT_LOG_DISTRIBUTED_SUBSYSTEM' in
  /// stdlib/public/Distributed/TracingDistributedSignpost.cpp
  static let subsystem = "com.apple.swift.distributed"

  private let process = Process()
  private let pipe = Pipe()
  private let buffer = StreamBuffer()

  init() {
    let pid = ProcessInfo.processInfo.processIdentifier

    process.executableURL = URL(fileURLWithPath: "/usr/bin/log")
    process.arguments = [
      "stream",
      "--signpost",
      "--predicate",
      "subsystem == \"\(SignpostStream.subsystem)\""
        + " AND processIdentifier == \(pid)",
    ]
    process.standardOutput = pipe
    process.standardError = FileHandle.nullDevice

    // Capture the buffer, not 'self': the handler is retained by the pipe,
    // which this object owns, so capturing 'self' would be a cycle
    let buffer = self.buffer
    pipe.fileHandleForReading.readabilityHandler = { handle in
      let data = handle.availableData
      guard !data.isEmpty else { return }
      buffer.append(data)
    }
  }

  /// Starts the stream and waits for it to connect to the logging daemon.
  /// `log stream` prints its header banner before it is actually live, so this
  /// simply waits long enough for the connection to come up; the earliest
  /// signposts would otherwise be missed.
  func start() async throws {
    try process.run()
    try await Task.sleep(for: .seconds(5))
  }

  /// Stops the stream and prints everything it reported, verbatim.
  func finishAndPrint() async throws {
    // Give the last signposts time to make it through the daemon
    try await Task.sleep(for: .seconds(3))

    process.terminate()
    process.waitUntilExit()

    print(buffer.text)
  }
}

enum GreeterError: Error {
  case boom
}

distributed actor Greeter {
  distributed func greet(name: String) -> String {
    "Hello, \(name)!"
  }

  distributed func boom() throws -> String {
    throw GreeterError.boom
  }
}

@main struct Main {
  static func main() async throws {
    let stream = SignpostStream()
    try await stream.start()

    let system = DefaultDistributedActorSystem()

    // ==== A call that returns ------------------------------------------------
    do {
      let local = Greeter(actorSystem: system)
      let remote = try Greeter.resolve(id: local.id, using: system)

      let reply = try await remote.greet(name: "Caplin")
      precondition(reply == "Hello, Caplin!", "unexpected reply: \(reply)")
    }

    // ==== A call whose target throws -----------------------------------------
    do {
      let local = Greeter(actorSystem: system)
      let remote = try Greeter.resolve(id: local.id, using: system)

      do {
        _ = try await remote.boom()
        fatalError("expected 'boom()' to throw")
      } catch {
        // expected
      }
    }

    try await stream.finishAndPrint()

    // ==== The returning call -------------------------------------------------

    // The whole outbound call is one interval; the thunk opens it, then brackets
    // encoding the invocation inside it, carrying the target's mangled accessor
    // record name and how many arguments are being encoded
    // CHECK:      begin]{{.*}}distributed_outbound_encode_arguments: actor=0x{{[0-9a-f]+}} targetFunction=$s4main7GreeterC5greet4nameS2S_tYaKFTE argumentCount=1
    // CHECK-NEXT: end]{{.*}}distributed_outbound_encode_arguments: success=true errorType=

    // Only then is 'remoteCall' invoked, inside the outbound remote-call interval
    // CHECK-NEXT: begin]{{.*}}distributed_outbound_remote_call: actor=0x{{[0-9a-f]+}} actorType=main.Greeter targetActorId={{.*}} targetFunction=$s4main7GreeterC5greet4nameS2S_tYaKFTE

    // The callee side: 'executeDistributedTarget' is one interval spanning
    // decoding, invoking the target and the result handler
    // CHECK-NEXT: begin]{{.*}}distributed_inbound_execute_target: actor=0x{{[0-9a-f]+}} actorType=main.Greeter targetActorId={{.*}} targetFunction=$s4main7GreeterC5greet4nameS2S_tYaKFTE
    // CHECK-NEXT: begin]{{.*}}distributed_inbound_decode_arguments: actor=0x{{[0-9a-f]+}} targetFunction=$s4main7GreeterC5greet4nameS2S_tYaKFTE

    // Looking the accessor up falls inside the decode interval
    // CHECK-NEXT: event]{{.*}}distributed_inbound_find_accessible_function: targetName=$s4main7GreeterC5greet4nameS2S_tYaKFTE
    // CHECK-NEXT: end]{{.*}}distributed_inbound_decode_arguments: argumentCount=1 success=true errorType=

    // Executing the target, which decodes the argument values as it goes
    // CHECK-NEXT: begin]{{.*}}distributed_inbound_invoke_target: actor=0x{{[0-9a-f]+}} targetFunction=$s4main7GreeterC5greet4nameS2S_tYaKFTE
    // CHECK-NEXT: event]{{.*}}distributed_inbound_find_accessible_function: targetName=$s4main7GreeterC5greet4nameS2S_tYaKFTE
    // CHECK-NEXT: end]{{.*}}distributed_inbound_invoke_target: success=true errorType=
    // CHECK-NEXT: event]{{.*}}distributed_inbound_invoke_result_handler: actor=0x{{[0-9a-f]+}} targetFunction=$s4main7GreeterC5greet4nameS2S_tYaKFTE success=true errorType=

    // Then the two enclosing intervals close, innermost (callee) first
    // CHECK-NEXT: end]{{.*}}distributed_inbound_execute_target: success=true errorType=
    // CHECK-NEXT: end]{{.*}}distributed_outbound_remote_call: success=true errorType=

    // ==== The throwing call --------------------------------------------------

    // Encoding and decoding still succeed; only the target itself throws
    // CHECK-NEXT: begin]{{.*}}distributed_outbound_encode_arguments: actor=0x{{[0-9a-f]+}} targetFunction=$s4main7GreeterC4boomSSyYaKFTE argumentCount=0
    // CHECK-NEXT: end]{{.*}}distributed_outbound_encode_arguments: success=true errorType=
    // CHECK-NEXT: begin]{{.*}}distributed_outbound_remote_call: {{.*}}targetFunction=$s4main7GreeterC4boomSSyYaKFTE
    // CHECK-NEXT: begin]{{.*}}distributed_inbound_execute_target: {{.*}}targetFunction=$s4main7GreeterC4boomSSyYaKFTE
    // CHECK-NEXT: begin]{{.*}}distributed_inbound_decode_arguments: actor=0x{{[0-9a-f]+}} targetFunction=$s4main7GreeterC4boomSSyYaKFTE
    // CHECK-NEXT: event]{{.*}}distributed_inbound_find_accessible_function: targetName=$s4main7GreeterC4boomSSyYaKFTE
    // CHECK-NEXT: end]{{.*}}distributed_inbound_decode_arguments: argumentCount=0 success=true errorType=
    // CHECK-NEXT: begin]{{.*}}distributed_inbound_invoke_target: actor=0x{{[0-9a-f]+}} targetFunction=$s4main7GreeterC4boomSSyYaKFTE
    // CHECK-NEXT: event]{{.*}}distributed_inbound_find_accessible_function: targetName=$s4main7GreeterC4boomSSyYaKFTE

    // The three nested intervals that reach the error all report the target's
    // error type: the invoke interval, the enclosing execute interval, and the
    // outbound remote-call interval; the result handler event carries it too
    // CHECK-NEXT: end]{{.*}}distributed_inbound_invoke_target: success=false errorType=main.GreeterError
    // CHECK-NEXT: event]{{.*}}distributed_inbound_invoke_result_handler: actor=0x{{[0-9a-f]+}} targetFunction=$s4main7GreeterC4boomSSyYaKFTE success=false errorType=main.GreeterError
    // CHECK-NEXT: end]{{.*}}distributed_inbound_execute_target: success=false errorType=main.GreeterError
    // CHECK-NEXT: end]{{.*}}distributed_outbound_remote_call: success=false errorType=main.GreeterError
  }
}
