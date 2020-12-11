// RUN: %target-swift-frontend -primary-file %s -o %t.out -emit-module -emit-module-path %t.swiftmodule -frontend-parseable-output 2>&1 | %FileCheck %s

// CHECK: {{[1-9][0-9]*}}
// CHECK-NEXT: {
// CHECK-NEXT:   "kind": "began",
// CHECK-NEXT:   "name": "compile",
// CHECK-NEXT:   "command": "{{.*[\\/]}}swift-frontend{{(\.exe)?}}{{.*}}-primary-file {{.*[\\/]}}parseable_output.swift -o {{.*[\\/]}}parseable_output.swift.tmp.out  -emit-module -emit-module-path {{.*[\\/]}}parseable_output.swift.tmp.swiftmodule -frontend-parseable-output",
// CHECK-NEXT:   "command_executable": "{{.*[\\/]}}swift{{(-frontend|c)?(\.exe)?}}",
// CHECK-NEXT:   "command_arguments": [
// CHECK:          "-primary-file",
// CHECK-NEXT:     "{{.*[\\/]}}parseable_output.swift",
// CHECK:          "-o",
// CHECK-NEXT:     "{{.*[\\/]}}parseable_output.swift.tmp.out",
// CHECK-NEXT:     "-emit-module",
// CHECK-NEXT:     "-emit-module-path",
// CHECK-NEXT:     "{{.*[\\/]}}parseable_output.swift.tmp.swiftmodule",
// CHECK-NEXT:     "-frontend-parseable-output"
// CHECK-NEXT:   ],
// CHECK-NEXT:   "inputs": [
// CHECK-NEXT:       "{{.*[\\/]}}parseable_output.swift"
// CHECK-NEXT:     ],
// CHECK-NEXT:     "outputs": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "type": "image",
// CHECK-NEXT:         "path": "{{.*[\\/]}}parseable_output.swift.tmp.out"
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "type": "swiftmodule",
// CHECK-NEXT:         "path": "{{.*[\\/]}}parseable_output.swift.tmp.swiftmodule"
// CHECK-NEXT:       }
// CHECK-NEXT:     ],
// CHECK-NEXT:     "pid": [[PID:[0-9]*]]
// CHECK-NEXT:     "process": {
// CHECK-NEXT:       "real_pid": [[PID]]
// CHECK-NEXT:     }
// CHECK-NEXT:   }

// CHECK-NEXT: {{[1-9][0-9]*}}
// CHECK-NEXT: {
// CHECK-NEXT:   "kind": "finished",
// CHECK-NEXT:   "name": "compile",
// CHECK-NEXT:   "pid": [[PID]],
// CHECK-NEXT:   "process": {
// CHECK-NEXT:     "real_pid": [[PID]]
// CHECK-NEXT:   },
// CHECK-NEXT:   "exit-status": 0
// CHECK-NEXT: }
