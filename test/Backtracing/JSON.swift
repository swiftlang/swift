// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -Onone -g -o %t/json.exe
// RUN: %target-codesign %t/json.exe
// RUN: ! env SWIFT_BACKTRACE=enable=yes,cache=no,format=json,output-to=%t/crash.json %target-run %t/json.exe 2>&1
// RUN: %validate-json %t/crash.json | %FileCheck %s --check-prefixes CHECK,UNSANITIZED,DEMANGLED,IMAGES,OMITTEDIMAGES,SYMBOLICATED,CAPTUREDMEM

// Also check that we generate valid JSON with various different options set.

// RUN: ! env SWIFT_BACKTRACE=enable=yes,cache=no,format=json,output-to=%t/crash2.json,threads=crashed %target-run %t/json.exe 2>&1
// RUN: ! env SWIFT_BACKTRACE=enable=yes,cache=no,format=json,output-to=%t/crash3.json,registers=all %target-run %t/json.exe 2>&1
// RUN: ! env SWIFT_BACKTRACE=enable=yes,cache=no,format=json,output-to=%t/crash4.json,sanitize=yes %target-run %t/json.exe 2>&1
// RUN: ! env SWIFT_BACKTRACE=enable=yes,cache=no,format=json,output-to=%t/crash5.json,images=none %target-run %t/json.exe 2>&1
// RUN: ! env SWIFT_BACKTRACE=enable=yes,cache=no,format=json,output-to=%t/crash6.json,images=all %target-run %t/json.exe 2>&1
// RUN: ! env SWIFT_BACKTRACE=enable=yes,cache=no,format=json,output-to=%t/crash7.json,symbolicate=off %target-run %t/json.exe 2>&1
// RUN: ! env SWIFT_BACKTRACE=enable=yes,cache=no,format=json,output-to=%t/crash8.json,demangle=no %target-run %t/json.exe 2>&1
// RUN: %validate-json %t/crash2.json| %FileCheck %s --check-prefixes CHECK,UNSANITIZED,DEMANGLED,IMAGES,OMITTEDIMAGES,SYMBOLICATED,CAPTUREDMEM
// RUN: %validate-json %t/crash3.json| %FileCheck %s --check-prefixes CHECK,UNSANITIZED,DEMANGLED,IMAGES,OMITTEDIMAGES,SYMBOLICATED,CAPTUREDMEM
// RUN: %validate-json %t/crash4.json| %FileCheck %s --check-prefixes CHECK,SANITIZED,DEMANGLED,IMAGES,OMITTEDIMAGES,SYMBOLICATED
// RUN: %validate-json %t/crash5.json| %FileCheck %s --check-prefixes CHECK,UNSANITIZED,DEMANGLED,SYMBOLICATED,CAPTUREDMEM
// RUN: %validate-json %t/crash6.json| %FileCheck %s --check-prefixes CHECK,UNSANITIZED,DEMANGLED,IMAGES,SYMBOLICATED,CAPTUREDMEM
// RUN: %validate-json %t/crash7.json| %FileCheck %s --check-prefixes CHECK,IMAGES,OMITTEDIMAGES,CAPTUREDMEM
// RUN: %validate-json %t/crash8.json| %FileCheck %s --check-prefixes CHECK,UNSANITIZED,UNDEMANGLED,IMAGES,OMITTEDIMAGES,SYMBOLICATED,CAPTUREDMEM

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: asan

// REQUIRES: executable_test
// REQUIRES: backtracing
// REQUIRES: OS=macosx || OS=linux-gnu || OS=windows-msvc

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

// CHECK-NEXT: "description": "{{(Bad pointer dereference|Access violation)}}",
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
// CHECK-NEXT:         "address": "0x{{[0-9a-f]+}}"
// SYMBOLICATED-NEXT:  "symbol": "{{_?}}$s4json6level5yyF",
// SYMBOLICATED-NEXT:  "offset": [[OFFSET:[0-9]+]],
// DEMANGLED-NEXT:     "description": "level5() + [[OFFSET]]",
// SYMBOLICATED-NEXT:  "image": "json.exe",
// SYMBOLICATED-NEXT:  "sourceLocation": {
// UNSANITIZED-NEXT:     "file": "{{.*(\\\\|/)}}test{{(\\\\|/)}}Backtracing{{(\\\\|/)}}JSON.swift",
// SANITIZED-NEXT:       "file": "{{.*(\\\\|/)}}JSON.swift",
// SYMBOLICATED-NEXT:    "line": 51,
// SYMBOLICATED-NEXT:    "column": 15
// SYMBOLICATED-NEXT:  }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "kind": "returnAddress",
// CHECK-NEXT:         "address": "0x{{[0-9a-f]+}}"
// SYMBOLICATED-NEXT:  "symbol": "{{_?}}$s4json6level4yyF",
// SYMBOLICATED-NEXT:  "offset": [[OFFSET:[0-9]+]],
// DEMANGLED-NEXT:     "description": "level4() + [[OFFSET]]",
// SYMBOLICATED-NEXT:  "image": "json.exe",
// SYMBOLICATED-NEXT:  "sourceLocation": {
// UNSANITIZED-NEXT:     "file": "{{.*(\\\\|/)}}test{{(\\\\|/)}}Backtracing{{(\\\\|/)}}JSON.swift",
// SANITIZED-NEXT:       "file": "{{.*(\\\\|/)}}JSON.swift",
// SYMBOLICATED-NEXT:    "line": 45,
// SYMBOLICATED-NEXT:    "column": 3
// SYMBOLICATED-NEXT:  }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "kind": "returnAddress",
// CHECK-NEXT:         "address": "0x{{[0-9a-f]+}}"
// SYMBOLICATED-NEXT:  "symbol": "{{_?}}$s4json6level3yyF",
// SYMBOLICATED-NEXT:  "offset": [[OFFSET:[0-9]+]],
// DEMANGLED-NEXT:     "description": "level3() + [[OFFSET]]",
// SYMBOLICATED-NEXT:  "image": "json.exe",
// SYMBOLICATED-NEXT:  "sourceLocation": {
// UNSANITIZED-NEXT:     "file": "{{.*(\\\\|/)}}test{{(\\\\|/)}}Backtracing{{(\\\\|/)}}JSON.swift",
// SANITIZED-NEXT:       "file": "{{.*(\\\\|/)}}JSON.swift",
// SYMBOLICATED-NEXT:    "line": 41,
// SYMBOLICATED-NEXT:    "column": 3
// SYMBOLICATED-NEXT:  }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "kind": "returnAddress",
// CHECK-NEXT:         "address": "0x{{[0-9a-f]+}}"
// SYMBOLICATED-NEXT:  "symbol": "{{_?}}$s4json6level2yyF",
// SYMBOLICATED-NEXT:  "offset": [[OFFSET:[0-9]+]],
// DEMANGLED-NEXT:     "description": "level2() + [[OFFSET]]",
// SYMBOLICATED-NEXT:  "image": "json.exe",
// SYMBOLICATED-NEXT:  "sourceLocation": {
// UNSANITIZED-NEXT:     "file": "{{.*(\\\\|/)}}test{{(\\\\|/)}}Backtracing{{(\\\\|/)}}JSON.swift",
// SANITIZED-NEXT:       "file": "{{.*(\\\\|/)}}JSON.swift",
// SYMBOLICATED-NEXT:    "line": 37,
// SYMBOLICATED-NEXT:    "column": 3
// SYMBOLICATED-NEXT:  }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "kind": "returnAddress",
// CHECK-NEXT:         "address": "0x{{[0-9a-f]+}}"
// SYMBOLICATED-NEXT:  "symbol": "{{_?}}$s4json6level1yyF",
// SYMBOLICATED-NEXT:  "offset": [[OFFSET:[0-9]+]],
// DEMANGLED-NEXT:     "description": "level1() + [[OFFSET]]",
// SYMBOLICATED-NEXT:  "image": "json.exe",
// SYMBOLICATED-NEXT:  "sourceLocation": {
// UNSANITIZED-NEXT:     "file": "{{.*(\\\\|/)}}test{{(\\\\|/)}}Backtracing{{(\\\\|/)}}JSON.swift",
// SANITIZED-NEXT:       "file": "{{.*(\\\\|/)}}JSON.swift",
// SYMBOLICATED-NEXT:    "line": 33,
// SYMBOLICATED-NEXT:    "column": 3
// SYMBOLICATED-NEXT:  }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "kind": "returnAddress",
// CHECK-NEXT:         "address": "0x{{[0-9a-f]+}}"
// SYMBOLICATED-NEXT:  "symbol": "{{_?}}$s4json5CrashV4mainyyFZ",
// SYMBOLICATED-NEXT:  "offset": [[OFFSET:[0-9]+]],
// DEMANGLED-NEXT:     "description": "static Crash.main() + [[OFFSET]]",
// SYMBOLICATED-NEXT:  "image": "json.exe",
// SYMBOLICATED-NEXT:  "sourceLocation": {
// UNSANITIZED-NEXT:     "file": "{{.*(\\\\|/)}}test{{(\\\\|/)}}Backtracing{{(\\\\|/)}}JSON.swift",
// SANITIZED-NEXT:       "file": "{{.*(\\\\|/)}}JSON.swift",
// SYMBOLICATED-NEXT:    "line": 57,
// SYMBOLICATED-NEXT:    "column": 5
// SYMBOLICATED-NEXT:  }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "kind": "returnAddress",
// CHECK-NEXT:         "address": "0x{{[0-9a-f]+}}"
// SYMBOLICATED-NEXT:  "system": true,
// SYMBOLICATED-NEXT:  "symbol": "{{_?}}$s4json5CrashV5$mainyyFZ",
// SYMBOLICATED-NEXT:  "offset": [[OFFSET:[0-9]+]],
// DEMANGLED-NEXT:     "description": "static Crash.$main() + [[OFFSET]]",
// SYMBOLICATED-NEXT:  "image": "json.exe",
// SYMBOLICATED-NEXT:  "sourceLocation": {
// SYMBOLICATED-NEXT:    "file": "{{(\\\\|/)*}}<compiler-generated>",
// SYMBOLICATED-NEXT:    "line": 0,
// SYMBOLICATED-NEXT:    "column": 0
// SYMBOLICATED-NEXT:  }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "kind": "returnAddress",
// CHECK-NEXT:         "address": "0x{{[0-9a-f]+}}"
// SYMBOLICATED-NEXT:  "system": true,
// SYMBOLICATED-NEXT:  "symbol": "{{_?main}}",
// SYMBOLICATED-NEXT:  "offset": [[OFFSET:[0-9]+]],
// DEMANGLED-NEXT:     "description": "main + [[OFFSET]]",
// SYMBOLICATED-NEXT:  "image": "json.exe",
// SYMBOLICATED-NEXT:  "sourceLocation": {
// UNSANITIZED-NEXT:     "file": "{{.*(\\\\|/)}}test{{(\\\\|/)}}Backtracing{{(\\\\|/)}}JSON.swift",
// SANITIZED-NEXT:       "file": "{{.*(\\\\|/)}}JSON.swift",
// SYMBOLICATED-NEXT:    "line": 0,
// SYMBOLICATED-NEXT:    "column": 0
// SYMBOLICATED-NEXT:  }
// CHECK-NEXT:       },

// More frames here, but they're system specific

// CHECK:          ]
// CHECK:        }
// CHECK-NEXT: ],
// CAPTUREDMEM-NEXT: "capturedMemory": {
// CAPTUREDMEM-NEXT:   "0x{{[[0-9a-f]+}}": "{{([0-9a-f][0-9a-f])+}}",

// More captures here, but system specific

// CAPTUREDMEM:      },
// OMITTEDIMAGES-NEXT: "omittedImages": {{[0-9]+}},
// IMAGES-NEXT: "images": [
// IMAGES-NEXT:   {

// Maybe multiple images before this one

// IMAGES:          "name": "json.exe",
//                 "buildId": ... is optional
// IMAGES:          "path": "{{.*(\\\\|/)}}json.exe",
// IMAGES-NEXT:     "baseAddress": "0x{{[0-9a-f]+}}",
// IMAGES-NEXT:     "endOfText": "0x{{[0-9a-f]+}}"
// IMAGES-NEXT:   }

// Maybe multiple images after this one

// IMAGES:      ],
// CHECK-NEXT: "backtraceTime": {{[0-9]+(\.[0-9]+)?}}

// CHECK-NEXT: }
