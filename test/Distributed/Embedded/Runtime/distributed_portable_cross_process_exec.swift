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

// A cross-process, cross-mode distributed round-trip. The request is serialized
// by an ordinary-Swift client whose actor system binds `SerializationRequirement`
// to `Codable` and serializes with Foundation's `JSONEncoder`. That request is
// then picked up and dispatched by a SEPARATELY built Embedded Swift server whose
// same actor system has no `Codable` / Foundation at all - it serializes through a
// tiny naive JSON writer (see `JSONWriter` in the shared Inputs file). Because the
// naive writer emits bytes byte-for-byte identical to `JSONEncoder`, the request
// the Codable side wrote decodes on the Embedded side, and the response the
// Embedded side wrote decodes back on the Codable side.
//
// The same source compiles to both programs; only `@main` differs per mode. The
// full distributed machinery is exercised on both ends: the client goes through
// the synthesized distributed thunk (its `remoteCall` captures the mangled target
// identifier and the argument wire), and the server dispatches through the
// compiler-synthesized `_executeDistributedTarget` if-chain keyed on that same
// identifier.
//
// Both programs are built `-module-name main` so the identifier agrees except for
// the mangling prefix: ordinary Swift uses "$s", Embedded uses "$e" for the very
// same declaration. The server bridges that one prefix before dispatching (a real
// transport would ship a stable logical selector, not a raw mangled name).

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
//
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
    guard var idBytes = takeField(wire, &offset) else { fatalError("malformed request") }

    // The non-embedded client mangles distributed-thunk identifiers with the
    // standard "$s" prefix; Embedded Swift mangles the very same declaration with
    // an "$e" prefix. Everything after the prefix is identical (same module name,
    // same symbol), so bridge the two here - the server's synthesized dispatch
    // compares against its own "$e"-prefixed names. Done on raw bytes to stay off
    // the Unicode tables. A production transport would agree on a stable logical
    // selector rather than shipping a raw mangled name in the first place
    if idBytes.count >= 2, idBytes[0] == UInt8(ascii: "$"), idBytes[1] == UInt8(ascii: "s") {
      idBytes[1] = UInt8(ascii: "e")
    }
    let target = RemoteCallTarget(String(decoding: idBytes, as: UTF8.self))

    let argBuffer = CallBuffer()
    argBuffer.argBytes = Array(wire[offset...])

    let system = PortableRoundtripActorSystem()
    let greeter = Greeter(actorSystem: system)
    var decoder = PortableDecoder(buffer: argBuffer, system: system)
    let responseBuffer = CallBuffer()
    let handler = PortableResultHandler(buffer: responseBuffer)

    do {
      // Same entry point as non-embedded: the system's `executeDistributedTarget`
      // dispatches the incoming call. In Embedded Swift it forwards to the actor's
      // synthesized `_executeDistributedTarget` witness
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
      // The thunk records the argument through JSONEncoder and calls remoteCall,
      // which (in .writeAndExit mode) writes the request and exits - never returns
      _ = try await remoteRef.hello(name: "World")

    default: // "recv"
      let bytes = [UInt8](try Data(contentsOf: URL(fileURLWithPath: path)))
      let buffer = CallBuffer()
      buffer.argBytes = bytes
      let decoder = PortableDecoder(buffer: buffer)
      let result: String = try decoder.decodeNextArgument()
      print("[swift] client decoded: \(result)")
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
