// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -Onone -g -o %t/JSONAsync
// RUN: %target-codesign %t/JSONAsync

// RUN: env SWIFT_BACKTRACE=enable=yes,demangle=no,cache=no,format=json,output-to=%t/crash.json %target-run %t/JSONAsync 2>&1 || true
// RUN: %validate-json %t/crash.json | %FileCheck %s

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: asan
// REQUIRES: executable_test
// REQUIRES: backtracing
// REQUIRES: OS=macosx || OS=linux-gnu

@available(SwiftStdlib 5.1, *)
func crash() {
  let ptr = UnsafeMutablePointer<Int>(bitPattern: 4)!
  ptr.pointee = 42
}

@available(SwiftStdlib 5.1, *)
func level(_ n: Int) async {
  if n < 5 {
    await level(n + 1)
  } else {
    crash()
  }
}

@available(SwiftStdlib 5.1, *)
@main
struct JSONAsync {
  static func main() async {
    await level(1)
  }
}

// CHECK: {

// JSON logs start with an ISO timestamp

// CHECK-NEXT: "timestamp": "{{[0-9]{4,}-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])T([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]\.[0-9]{6}Z}}",


// Then a tag identifying this as a crash report

// CHECK-NEXT: "kind": "crashReport",


// This is followed by a description and the fault address

// CHECK-NEXT: "description": "Bad pointer dereference",
// CHECK-NEXT: "faultAddress": "0x{{0+}}4",


// And then by the platform and architecture

// CHECK-NEXT: "platform": "{{.*}}",
// CHECK-NEXT: "architecture": "{{.*}}",


// And then a list of threads

// CHECK-NEXT: "threads": [
// CHECK-NEXT:   {

// The crashing thread isn't necessarily the first

// CHECK:          "crashed": true,
// CHECK-NEXT:     "registers": {
// CHECK-NEXT:       "{{.*}}": "0x{{[0-9a-f]+}}",

// More registers here, but the number is system specific

// CHECK:          },
// CHECK-NEXT:     "frames": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "kind": "programCounter",
// CHECK-NEXT:         "address": "0x{{[0-9a-f]+}}",
// CHECK-NEXT:         "symbol": "{{_?}}$s9JSONAsync5crashyyF",
// CHECK-NEXT:         "offset": [[OFFSET:[0-9]+]],
// CHECK-NEXT:         "image": "JSONAsync",
// CHECK-NEXT:         "sourceLocation": {
// CHECK-NEXT:           "file": "{{.*}}/JSONAsync.swift",
// CHECK-NEXT:           "line": 18,
// CHECK-NEXT:           "column": 15
// CHECK-NEXT:         }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "kind": "returnAddress",
// CHECK-NEXT:         "address": "0x{{[0-9a-f]+}}",
// CHECK-NEXT:         "symbol": "{{_?}}$s9JSONAsync5levelyySiYaFTY0_",
// CHECK-NEXT:         "offset": [[OFFSET:[0-9]+]],
// CHECK-NEXT:         "image": "JSONAsync",
// CHECK-NEXT:         "sourceLocation": {
// CHECK-NEXT:           "file": "{{.*}}/JSONAsync.swift",
// CHECK-NEXT:           "line": 26,
// CHECK-NEXT:           "column": 5
// CHECK-NEXT:         }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "kind": "asyncResumePoint",
// CHECK-NEXT:         "address": "0x{{[0-9a-f]+}}",
// CHECK-NEXT:         "symbol": "{{_?}}$s9JSONAsync5levelyySiYaFTQ1_",
// CHECK-NEXT:         "offset": [[OFFSET:[0-9]+]],
// CHECK-NEXT:         "image": "JSONAsync",
// CHECK-NEXT:         "sourceLocation": {
// CHECK-NEXT:           "file": "{{.*}}/JSONAsync.swift",
// CHECK-NEXT:           "line": 24,
// CHECK-NEXT:           "column": 0
// CHECK-NEXT:         }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "kind": "asyncResumePoint",
// CHECK-NEXT:         "address": "0x{{[0-9a-f]+}}",
// CHECK-NEXT:         "symbol": "{{_?}}$s9JSONAsync5levelyySiYaFTQ1_",
// CHECK-NEXT:         "offset": [[OFFSET:[0-9]+]],
// CHECK-NEXT:         "image": "JSONAsync",
// CHECK-NEXT:         "sourceLocation": {
// CHECK-NEXT:           "file": "{{.*}}/JSONAsync.swift",
// CHECK-NEXT:           "line": 24,
// CHECK-NEXT:           "column": 0
// CHECK-NEXT:         }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "kind": "asyncResumePoint",
// CHECK-NEXT:         "address": "0x{{[0-9a-f]+}}",
// CHECK-NEXT:         "symbol": "{{_?}}$s9JSONAsync5levelyySiYaFTQ1_",
// CHECK-NEXT:         "offset": [[OFFSET:[0-9]+]],
// CHECK-NEXT:         "image": "JSONAsync",
// CHECK-NEXT:         "sourceLocation": {
// CHECK-NEXT:           "file": "{{.*}}/JSONAsync.swift",
// CHECK-NEXT:           "line": 24,
// CHECK-NEXT:           "column": 0
// CHECK-NEXT:         }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "kind": "asyncResumePoint",
// CHECK-NEXT:         "address": "0x{{[0-9a-f]+}}",
// CHECK-NEXT:         "symbol": "{{_?}}$s9JSONAsync5levelyySiYaFTQ1_",
// CHECK-NEXT:         "offset": [[OFFSET:[0-9]+]],
// CHECK-NEXT:         "image": "JSONAsync",
// CHECK-NEXT:         "sourceLocation": {
// CHECK-NEXT:           "file": "{{.*}}/JSONAsync.swift",
// CHECK-NEXT:           "line": 24,
// CHECK-NEXT:           "column": 0
// CHECK-NEXT:         }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "kind": "asyncResumePoint",
// CHECK-NEXT:         "address": "0x{{[0-9a-f]+}}",
// CHECK-NEXT:         "symbol": "{{_?}}$s9JSONAsyncAAV4mainyyYaFZTQ0_",
// CHECK-NEXT:         "offset": [[OFFSET:[0-9]+]],
// CHECK-NEXT:         "image": "JSONAsync",
// CHECK-NEXT:         "sourceLocation": {
// CHECK-NEXT:           "file": "{{.*}}/JSONAsync.swift",
// CHECK-NEXT:           "line": 34,
// CHECK-NEXT:           "column": 0
// CHECK-NEXT:         }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "kind": "asyncResumePoint",
// CHECK-NEXT:         "address": "0x{{[0-9a-f]+}}",
// CHECK-NEXT:         "system": true,
// CHECK-NEXT:         "symbol": "{{_?}}$s9JSONAsyncAAV5$mainyyYaFZTQ0_",
// CHECK-NEXT:         "offset": [[OFFSET:[0-9]+]],
// CHECK-NEXT:         "image": "JSONAsync",
// CHECK-NEXT:         "sourceLocation": {
// CHECK-NEXT:           "file": "{{/*}}<compiler-generated>",
// CHECK-NEXT:           "line": 0,
// CHECK-NEXT:           "column": 0
// CHECK-NEXT:         }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "kind": "asyncResumePoint",
// CHECK-NEXT:         "address": "0x{{[0-9a-f]+}}",
// CHECK-NEXT:         "system": true,
// CHECK-NEXT:         "symbol": "{{_?}}async_MainTQ0_",
// CHECK-NEXT:         "offset": [[OFFSET:[0-9]+]],
// CHECK-NEXT:         "image": "JSONAsync",
// CHECK-NEXT:         "sourceLocation": {
// CHECK-NEXT:           "file": "{{/*}}<compiler-generated>",
// CHECK-NEXT:           "line": 0,
// CHECK-NEXT:           "column": 0
// CHECK-NEXT:         }
// CHECK-NEXT:       },

// More frames here, but they're system specific

// CHECK:          ]
// CHECK-NEXT:   }

// Potentially more threads here

// CHECK:      ],
// CHECK-NEXT: "capturedMemory": {
// CHECK-NEXT:   "0x{{[[0-9a-f]+}}": "{{([0-9a-f][0-9a-f])+}}",

// More captures here, but system specific

// CHECK:      },
// CHECK-NEXT: "omittedImages": {{[0-9]+}},
// CHECK-NEXT: "images": [
// CHECK-NEXT:   {

// Maybe multiple images before this one

// CHECK:          "name": "JSONAsync",
//                 "buildId": ... is optional
// CHECK:          "path": "{{.*}}/JSONAsync",
// CHECK-NEXT:     "baseAddress": "0x{{[0-9a-f]+}}",
// CHECK-NEXT:     "endOfText": "0x{{[0-9a-f]+}}"
// CHECK-NEXT:   }

// Maybe multiple images after this one

// CHECK:      ],
// CHECK-NEXT: "backtraceTime": {{[0-9]+(\.[0-9]+)?}}

// CHECK-NEXT: }
