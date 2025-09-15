// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -Onone -g -o %t/Crash
// RUN: %target-codesign %t/Crash
// RUN: env SWIFT_BACKTRACE=enable=yes,cache=no,format=json,output-to=%t/crash.json %target-run %t/Crash 2>&1 || true
// RUN: %validate-json %t/crash.json | %FileCheck %s


// Also check that we generate valid JSON with various different options set.

// RUN: env SWIFT_BACKTRACE=enable=yes,cache=no,format=json,output-to=%t/crash2.json,threads=crashed %target-run %t/Crash 2>&1 || true
// RUN: env SWIFT_BACKTRACE=enable=yes,cache=no,format=json,output-to=%t/crash3.json,registers=all %target-run %t/Crash 2>&1 || true
// RUN: env SWIFT_BACKTRACE=enable=yes,cache=no,format=json,output-to=%t/crash4.json,sanitize=yes %target-run %t/Crash 2>&1 || true
// RUN: env SWIFT_BACKTRACE=enable=yes,cache=no,format=json,output-to=%t/crash5.json,images=none %target-run %t/Crash 2>&1 || true
// RUN: env SWIFT_BACKTRACE=enable=yes,cache=no,format=json,output-to=%t/crash6.json,images=all %target-run %t/Crash 2>&1 || true
// RUN: env SWIFT_BACKTRACE=enable=yes,cache=no,format=json,output-to=%t/crash7.json,symbolicate=off %target-run %t/Crash 2>&1 || true
// RUN: env SWIFT_BACKTRACE=enable=yes,cache=no,format=json,output-to=%t/crash8.json,demangle=no %target-run %t/Crash 2>&1 || true
// RUN: %validate-json %t/crash2.json
// RUN: %validate-json %t/crash3.json
// RUN: %validate-json %t/crash4.json
// RUN: %validate-json %t/crash5.json
// RUN: %validate-json %t/crash6.json
// RUN: %validate-json %t/crash7.json
// RUN: %validate-json %t/crash8.json

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: asan
// REQUIRES: executable_test
// REQUIRES: backtracing
// REQUIRES: OS=macosx || OS=linux-gnu

func level1() {
  level2()
}

func level2() {
  level3()
}

func level3() {
  level4()
}

func level4() {
  level5()
}

func level5() {
  print("About to crash")
  let ptr = UnsafeMutablePointer<Int>(bitPattern: 4)!
  ptr.pointee = 42
}

@main
struct Crash {
  static func main() {
    level1()
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

// On Linux there's a name here

// CHECK:          "crashed": true,
// CHECK-NEXT:     "registers": {
// CHECK-NEXT:       "{{.*}}": "0x{{[0-9a-f]+}}",

// More registers here, but the number is system specific

// CHECK:          },
// CHECK-NEXT:     "frames": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "kind": "programCounter",
// CHECK-NEXT:         "address": "0x{{[0-9a-f]+}}",
// CHECK-NEXT:         "symbol": "{{_?}}$s5Crash6level5yyF",
// CHECK-NEXT:         "offset": [[OFFSET:[0-9]+]],
// CHECK-NEXT:         "description": "level5() + [[OFFSET]]",
// CHECK-NEXT:         "image": "Crash",
// CHECK-NEXT:         "sourceLocation": {
// CHECK-NEXT:           "file": "{{.*}}/JSON.swift",
// CHECK-NEXT:           "line": 51,
// CHECK-NEXT:           "column": 15
// CHECK-NEXT:         }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "kind": "returnAddress",
// CHECK-NEXT:         "address": "0x{{[0-9a-f]+}}",
// CHECK-NEXT:         "symbol": "{{_?}}$s5Crash6level4yyF",
// CHECK-NEXT:         "offset": [[OFFSET:[0-9]+]],
// CHECK-NEXT:         "description": "level4() + [[OFFSET]]",
// CHECK-NEXT:         "image": "Crash",
// CHECK-NEXT:         "sourceLocation": {
// CHECK-NEXT:           "file": "{{.*}}/JSON.swift",
// CHECK-NEXT:           "line": 45,
// CHECK-NEXT:           "column": 3
// CHECK-NEXT:         }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "kind": "returnAddress",
// CHECK-NEXT:         "address": "0x{{[0-9a-f]+}}",
// CHECK-NEXT:         "symbol": "{{_?}}$s5Crash6level3yyF",
// CHECK-NEXT:         "offset": [[OFFSET:[0-9]+]],
// CHECK-NEXT:         "description": "level3() + [[OFFSET]]",
// CHECK-NEXT:         "image": "Crash",
// CHECK-NEXT:         "sourceLocation": {
// CHECK-NEXT:           "file": "{{.*}}/JSON.swift",
// CHECK-NEXT:           "line": 41,
// CHECK-NEXT:           "column": 3
// CHECK-NEXT:         }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "kind": "returnAddress",
// CHECK-NEXT:         "address": "0x{{[0-9a-f]+}}",
// CHECK-NEXT:         "symbol": "{{_?}}$s5Crash6level2yyF",
// CHECK-NEXT:         "offset": [[OFFSET:[0-9]+]],
// CHECK-NEXT:         "description": "level2() + [[OFFSET]]",
// CHECK-NEXT:         "image": "Crash",
// CHECK-NEXT:         "sourceLocation": {
// CHECK-NEXT:           "file": "{{.*}}/JSON.swift",
// CHECK-NEXT:           "line": 37,
// CHECK-NEXT:           "column": 3
// CHECK-NEXT:         }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "kind": "returnAddress",
// CHECK-NEXT:         "address": "0x{{[0-9a-f]+}}",
// CHECK-NEXT:         "symbol": "{{_?}}$s5Crash6level1yyF",
// CHECK-NEXT:         "offset": [[OFFSET:[0-9]+]],
// CHECK-NEXT:         "description": "level1() + [[OFFSET]]",
// CHECK-NEXT:         "image": "Crash",
// CHECK-NEXT:         "sourceLocation": {
// CHECK-NEXT:           "file": "{{.*}}/JSON.swift",
// CHECK-NEXT:           "line": 33,
// CHECK-NEXT:           "column": 3
// CHECK-NEXT:         }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "kind": "returnAddress",
// CHECK-NEXT:         "address": "0x{{[0-9a-f]+}}",
// CHECK-NEXT:         "symbol": "{{_?}}$s5CrashAAV4mainyyFZ",
// CHECK-NEXT:         "offset": [[OFFSET:[0-9]+]],
// CHECK-NEXT:         "description": "static Crash.main() + [[OFFSET]]",
// CHECK-NEXT:         "image": "Crash",
// CHECK-NEXT:         "sourceLocation": {
// CHECK-NEXT:           "file": "{{.*}}/JSON.swift",
// CHECK-NEXT:           "line": 57,
// CHECK-NEXT:           "column": 5
// CHECK-NEXT:         }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "kind": "returnAddress",
// CHECK-NEXT:         "address": "0x{{[0-9a-f]+}}",
// CHECK-NEXT:         "system": true,
// CHECK-NEXT:         "symbol": "{{_?}}$s5CrashAAV5$mainyyFZ",
// CHECK-NEXT:         "offset": [[OFFSET:[0-9]+]],
// CHECK-NEXT:         "description": "static Crash.$main() + [[OFFSET]]",
// CHECK-NEXT:         "image": "Crash",
// CHECK-NEXT:         "sourceLocation": {
// CHECK-NEXT:           "file": "{{/*}}<compiler-generated>",
// CHECK-NEXT:           "line": 0,
// CHECK-NEXT:           "column": 0
// CHECK-NEXT:         }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "kind": "returnAddress",
// CHECK-NEXT:         "address": "0x{{[0-9a-f]+}}",
// CHECK-NEXT:         "system": true,
// CHECK-NEXT:         "symbol": "{{_?main}}",
// CHECK-NEXT:         "offset": [[OFFSET:[0-9]+]],
// CHECK-NEXT:         "description": "main + [[OFFSET]]",
// CHECK-NEXT:         "image": "Crash",
// CHECK-NEXT:         "sourceLocation": {
// CHECK-NEXT:           "file": "{{.*}}/JSON.swift",
// CHECK-NEXT:           "line": 0,
// CHECK-NEXT:           "column": 0
// CHECK-NEXT:         }
// CHECK-NEXT:       },

// More frames here, but they're system specific

// CHECK:          ]
// CHECK:        }
// CHECK-NEXT: ],
// CHECK-NEXT: "capturedMemory": {
// CHECK-NEXT:   "0x{{[[0-9a-f]+}}": "{{([0-9a-f][0-9a-f])+}}",

// More captures here, but system specific

// CHECK:      },
// CHECK-NEXT: "omittedImages": {{[0-9]+}},
// CHECK-NEXT: "images": [
// CHECK-NEXT:   {

// Maybe multiple images before this one

// CHECK:          "name": "Crash",
//                 "buildId": ... is optional
// CHECK:          "path": "{{.*}}/Crash",
// CHECK-NEXT:     "baseAddress": "0x{{[0-9a-f]+}}",
// CHECK-NEXT:     "endOfText": "0x{{[0-9a-f]+}}"
// CHECK-NEXT:   }

// Maybe multiple images after this one

// CHECK:      ],
// CHECK-NEXT: "backtraceTime": {{[0-9]+(\.[0-9]+)?}}

// CHECK-NEXT: }
