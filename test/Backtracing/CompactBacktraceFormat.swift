// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -Xfrontend -disable-availability-checking -Onone -o %t/CompactBacktraceFormat.exe
// RUN: %target-codesign %t/CompactBacktraceFormat.exe
// RUN: %target-run %t/CompactBacktraceFormat.exe | %FileCheck %s

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// REQUIRES: executable_test
// REQUIRES: backtracing
// REQUIRES: OS=macosx || OS=linux-gnu || OS=windows-msvc

import Runtime
@_spi(Internal) import Runtime
import Foundation

typealias Frame = Backtrace.Frame
typealias Rich = RichFrame<UInt64>

// Exercises every Compact Backtrace Format instruction (pc/ra/async,
// absolute and delta addressing, internal and external omit/rep, end and
// trunc), checks that the `rep` instruction's repeat count is limited, with
// an over-limit repeat producing a `.truncated` frame, and checks that an
// enormous `omittedFrames` count doesn't overflow while formatting a
// backtrace's description.

func printFrame(_ frame: Frame) {
  switch frame {
    case let .programCounter(addr):
      print("pc 0x\(String(UInt64(addr)!, radix: 16))")
    case let .returnAddress(addr):
      print("ra 0x\(String(UInt64(addr)!, radix: 16))")
    case let .asyncResumePoint(addr):
      print("async 0x\(String(UInt64(addr)!, radix: 16))")
    case let .omittedFrames(count):
      print("omit \(count)")
    case .truncated:
      print("truncated")
  }
}

// Round-trips a mix of frames through the encoder and decoder, covering
// pc/ra/async, an internal (<= 0x1f) and an external omit, and an internal
// (<= 8 repeats) and an external rep, ending in a `trunc` instruction.
func testAllInstructions() {
  let richFrames: [Rich] = [
    .programCounter(0x1000),
    .returnAddress(0x1010),
    .returnAddress(0xf10),
    .asyncResumePoint(0x7fff00002000),
    .omittedFrames(5),
    .omittedFrames(1000),
    .returnAddress(0x2020),
    .returnAddress(0x2020),
    .programCounter(0x3030), .programCounter(0x3030), .programCounter(0x3030),
    .programCounter(0x3030), .programCounter(0x3030), .programCounter(0x3030),
    .programCounter(0x3030), .programCounter(0x3030), .programCounter(0x3030),
    .programCounter(0x3030),
    .truncated
  ]

  let bt = Backtrace(architecture: "test", frames: richFrames, images: nil)

  print("-- all instructions --")
  var count = 0
  for frame in bt.frames {
    count += 1
    printFrame(frame)
  }
  print("count \(count)")
}

// CHECK: -- all instructions --
// CHECK-NEXT: pc 0x1000
// CHECK-NEXT: ra 0x1010
// CHECK-NEXT: ra 0xf10
// CHECK-NEXT: async 0x7fff00002000
// CHECK-NEXT: omit 5
// CHECK-NEXT: omit 1000
// CHECK-NEXT: ra 0x2020
// CHECK-NEXT: ra 0x2020
// CHECK-NEXT: pc 0x3030
// CHECK-NEXT: pc 0x3030
// CHECK-NEXT: pc 0x3030
// CHECK-NEXT: pc 0x3030
// CHECK-NEXT: pc 0x3030
// CHECK-NEXT: pc 0x3030
// CHECK-NEXT: pc 0x3030
// CHECK-NEXT: pc 0x3030
// CHECK-NEXT: pc 0x3030
// CHECK-NEXT: pc 0x3030
// CHECK-NEXT: truncated
// CHECK-NEXT: count 19

// A backtrace that finishes normally exercises the `end` instruction,
// rather than `trunc`.
func testEndInstruction() {
  let richFrames: [Rich] = [.programCounter(0x9999)]

  let bt = Backtrace(architecture: "test", frames: richFrames, images: nil)

  print("-- end instruction --")
  var count = 0
  for frame in bt.frames {
    count += 1
    printFrame(frame)
  }
  print("count \(count)")
}

// CHECK: -- end instruction --
// CHECK-NEXT: pc 0x9999
// CHECK-NEXT: count 1

// The encoder never emits a `rep` with more repeats than `maxRepeatCount`
// (presently 1_048_576), so to check the decoder's limit we have to craft
// raw Compact Backtrace Format bytes by hand and feed them in through
// `Backtrace`'s `Codable` conformance.
//
// Byte layout used below:
//   0x02              info byte: version 0, 64-bit words
//   0x18 0x7f         pc, absolute, 1-byte address 0x7f
//   0x8a <3 bytes>    rep, external, 3-byte repeat count
//   0x00              end
func testRepeatLimit() {
  let header: [UInt8] = [0x02, 0x18, 0x7f]

  // word == maxRepeatCount + 1, so iterations == maxRepeatCount, which is
  // exactly at the limit and should be accepted.
  let atLimit: [UInt8] = header + [0x8a, 0x10, 0x00, 0x01, 0x00]

  // word == maxRepeatCount + 2, so iterations == maxRepeatCount + 1, which
  // is one past the limit and must be rejected.
  let overLimit: [UInt8] = header + [0x8a, 0x10, 0x00, 0x02]

  func decode(_ bytes: [UInt8]) -> Backtrace {
    let base64 = Data(bytes).base64EncodedString()
    let json = "{\"architecture\":\"test\",\"backtrace\":\"\(base64)\"}"
    return try! JSONDecoder().decode(Backtrace.self, from: Data(json.utf8))
  }

  print("-- repeat limit: at limit --")
  var atLimitCount = 0
  var atLimitTruncated = false
  for frame in decode(atLimit).frames {
    atLimitCount += 1
    if case .truncated = frame {
      atLimitTruncated = true
    }
  }
  print("count \(atLimitCount) truncated \(atLimitTruncated)")

  print("-- repeat limit: over limit --")
  for frame in decode(overLimit).frames {
    printFrame(frame)
  }
}

// CHECK: -- repeat limit: at limit --
// CHECK-NEXT: count 1048578 truncated false

// CHECK: -- repeat limit: over limit --
// CHECK-NEXT: pc 0x7f
// CHECK-NEXT: truncated

// `Backtrace.description` keeps a running count of the frames it has
// printed so far, in order to number them; an `omittedFrames` count of
// close to `Int.max` used to trap with an arithmetic overflow when added
// to that running total.  It should now clamp to `Int.max` instead.
func testOmittedFramesOverflow() {
  let richFrames: [Rich] = [
    .programCounter(0x1111),
    .omittedFrames(Int.max),
    .programCounter(0x2222)
  ]

  let bt = Backtrace(architecture: "test", frames: richFrames, images: nil)

  print("-- omitted frames overflow --")

  let description = bt.description
  print(description)

  let lastLine = description.split(separator: "\n").last!
  let label = lastLine.split(separator: "\t", maxSplits: 1).first!
  print("clamped \(label == Substring("\(Int.max)"))")
}

@main
struct CompactBacktraceFormatTest {
  static func main() {
    testAllInstructions()
    testEndInstruction()
    testRepeatLimit()
    testOmittedFramesOverflow()
  }
}

// CHECK: -- omitted frames overflow --
// CHECK: clamped true
