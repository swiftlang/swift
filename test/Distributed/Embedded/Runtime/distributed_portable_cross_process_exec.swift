// RUN: %empty-directory(%t)
//
// Build the ordinary-Swift (Codable/JSON) client and the Embedded server:
// RUN: %target-build-swift -module-name main -target %target-swift-5.7-abi-triple -parse-as-library %s %S/Inputs/PortableRoundtripActorSystem.swift -o %t/client.out
// RUN: %target-codesign %t/client.out
// RUN: %target-swift-frontend -module-name main -target %target-cpu-apple-macos14 -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -enable-experimental-feature Extern -parse-as-library %s %S/Inputs/PortableRoundtripActorSystem.swift -c -o %t/embedded.o
// RUN: %target-embedded-link %t/embedded.o %target-embedded-posix-shim -o %t/embedded.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lswift_Concurrency -lswiftDistributed %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
//
// 1) Client sends a request (Codable):
// RUN: %target-run %t/client.out send %t/request.bin | %FileCheck %s --check-prefix=SEND

// 2) Embedded server reads the request on stdin, writes the response on stdout (not using Codable):
// RUN: %target-run %t/embedded.out < %t/request.bin > %t/response.bin

// 3) Client decodes the response (Codable again):
// RUN: %target-run %t/client.out recv %t/response.bin | %FileCheck %s --check-prefix=RECV

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDistributed
// REQUIRES: swift_feature_Extern

// A truly cross-process, cross-mode distributed round-trip.
//
// The client is ordinary-Swift client, using `Codable`.
// The client is handled by an Embeddet Swift client that uses a minimal JSON decoder.
//
// The same source compiles to both programs - this is to prove the reuse of actor and system types.

import _Concurrency
import Distributed

distributed actor Greeter {
  // No `print`: on the Embedded server, stdout carries the raw response wire, so
  // the actor method must not write to it
  distributed func hello(name: String) -> String {
    return "Hello, \(name)!"
  }
}

#if $Embedded

// ==== ----------------------------------------------------------------------
// MARK: Embedded server

// Embedded has no `CommandLine.arguments` and no Foundation, so the server does
// not take a file path - it reads the request off stdin and writes the response
// to stdout, declaring the two libc byte-I/O calls it needs itself.

@_extern(c, "getchar")
func getchar() -> CInt

@_extern(c, "putchar")
@discardableResult
func putchar(_ c: CInt) -> CInt

@main
struct Main {
  static func main() async {
    // Implement how a "receive side" of an actor system would look like:
    let system = PortableRoundtripActorSystem()
    let greeter = Greeter(actorSystem: system) // assume we have an actor with the right actor-id

    // Read the whole request off stdin
    var wire: [UInt8] = []
    while true {
      let c = getchar()
      if c < 0 { break } // EOF
      wire.append(UInt8(truncatingIfNeeded: c))
    }

    // Field 0 is the mangled target identifier the client sent; the remaining
    // bytes are the argument wire the decoder consumes
    var offset = 0
    guard let targetIDBytes = takeField(wire, &offset) else { fatalError("malformed request") }

    let targetIDSpan = targetIDBytes.span
    let target = RemoteCallTarget(targetIDSpan.bytes)

    let argBuffer = CallBuffer()
    argBuffer.argBytes = Array(wire[offset...])

    var decoder = PortableDecoder(buffer: argBuffer, system: system)
    let responseBuffer = CallBuffer()
    let handler = PortableResultHandler(buffer: responseBuffer)

    do {
      try await system.executeDistributedTarget(
          on: greeter, target: target, invocationDecoder: &decoder, handler: handler)
    } catch {
      fatalError("dispatch failed")
    }

    // Write the framed JSON response back on stdout for the client to decode
    for b in responseBuffer.returnBytes {
      putchar(CInt(b))
    }
  }
}

#else

// ==== ----------------------------------------------------------------------
// MARK: Ordinary-Swift (Codable) client
//
// `send <file>`  - make the distributed call; the actor system in
//                  `.writeAndExit` mode serializes the request to <file> and exits
// `recv <file>`  - decode the response the Embedded server left in <file>

import Foundation

@main
struct Main {
  static func main() async throws {
    let system = PortableRoundtripActorSystem()
    let verb = CommandLine.arguments[1]
    let path = CommandLine.arguments[2]

    switch verb {
    case "send":
      system.crossProcessSimulatedMode = .writeAndExit(messageWritePath: path)
      let remoteRef = try Greeter.resolve(id: PortableActorID(id: 1), using: system)
      _ = try await remoteRef.hello(name: "World")

    case "recv":
      let bytes = [UInt8](try Data(contentsOf: URL(fileURLWithPath: path)))
      let buffer = CallBuffer()
      buffer.argBytes = bytes
      let decoder = PortableDecoder(buffer: buffer)
      let result: String = try decoder.decodeNextArgument()
      print("[swift] client decoded: \(result)")

    default:
      fatalError("Unknown mode: \(verb)")
    }
  }
}

#endif

// The client serialized the request with Foundation's JSONEncoder - real JSON:
// SEND: [swift] client sent request: {{.*}}{"value":"World"}
//
// The Embedded server decoded that JSON, ran `hello`, and serialized the reply
// with its naive JSON writer; the client decoded it back:
// RECV: [swift] client decoded: Hello, World!
