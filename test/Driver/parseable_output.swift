// RUN: %swiftc_driver_plain -emit-executable %s -o %t.out -emit-module -emit-module-path %t.swiftmodule -emit-objc-header-path %t.h -serialize-diagnostics -emit-dependencies -parseable-output -driver-skip-execution 2>&1 | FileCheck %s

// XFAIL: linux

// CHECK: {{[1-9][0-9]*}}
// CHECK-NEXT: {
// CHECK-NEXT:   "kind": "began",
// CHECK-NEXT:   "name": "compile",
// CHECK-NEXT:   "command": "{{.*}}/swift{{c?}} -frontend -c -primary-file {{.*}}/parseable_output.swift {{.*}} -o {{.*}}/parseable_output-[[OUTPUT:.*]].o",
// CHECK-NEXT:   "inputs": [
// CHECK-NEXT:     "{{.*}}/parseable_output.swift"
// CHECK-NEXT:   ],
// CHECK-NEXT:   "outputs": [
// CHECK-NEXT:     {
// CHECK-NEXT:       "type": "object",
// CHECK-NEXT:       "path": "{{.*}}/parseable_output-[[OUTPUT]].o"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "type": "dependencies",
// CHECK-NEXT:       "path": "{{.*}}/parseable_output-[[OUTPUT]].d"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "type": "swiftmodule",
// CHECK-NEXT:       "path": "{{.*}}/parseable_output-[[OUTPUT]].swiftmodule"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "type": "swiftdoc",
// CHECK-NEXT:       "path": "{{.*}}/parseable_output-[[OUTPUT]].swiftdoc"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "type": "diagnostics",
// CHECK-NEXT:       "path": "{{.*}}/parseable_output-[[OUTPUT]].dia"
// CHECK-NEXT:     }
// CHECK-NEXT:   ],
// CHECK-NEXT:   "pid": 1
// CHECK-NEXT: }

// CHECK-NEXT: 113
// CHECK-NEXT: {
// CHECK-NEXT:   "kind": "finished",
// CHECK-NEXT:   "name": "compile",
// CHECK-NEXT:   "pid": 1,
// CHECK-NEXT:   "output": "Output placeholder\n",
// CHECK-NEXT:   "exit-status": 0
// CHECK-NEXT: }

// CHECK-NEXT: {{[1-9][0-9]*}}
// CHECK-NEXT: {
// CHECK-NEXT:   "kind": "began",
// CHECK-NEXT:   "name": "merge-module",
// CHECK-NEXT:   "command": "{{.*}}/swift{{c?}} -frontend -emit-module {{.*}}/parseable_output-[[OUTPUT]].swiftmodule {{.*}} -o {{.*}}/parseable_output.swift.tmp.swiftmodule",
// CHECK-NEXT:   "inputs": [
// CHECK-NEXT:     "{{.*}}/parseable_output-[[OUTPUT]].o"
// CHECK-NEXT:   ],
// CHECK-NEXT:   "outputs": [
// CHECK-NEXT:     {
// CHECK-NEXT:       "type": "swiftmodule",
// CHECK-NEXT:       "path": "{{.*}}/parseable_output.swift.tmp.swiftmodule"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "type": "swiftdoc",
// CHECK-NEXT:       "path": "{{.*}}/parseable_output.swift.tmp.swiftdoc"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "type": "objc-header",
// CHECK-NEXT:       "path": "{{.*}}/parseable_output.swift.tmp.h"
// CHECK-NEXT:     }
// CHECK-NEXT:   ],
// CHECK-NEXT:   "pid": 2
// CHECK-NEXT: }

// CHECK-NEXT: 118
// CHECK-NEXT: {
// CHECK-NEXT:   "kind": "finished",
// CHECK-NEXT:   "name": "merge-module",
// CHECK-NEXT:   "pid": 2,
// CHECK-NEXT:   "output": "Output placeholder\n",
// CHECK-NEXT:   "exit-status": 0
// CHECK-NEXT: }

// CHECK-NEXT: {{[1-9][0-9]*}}
// CHECK-NEXT: {
// CHECK-NEXT:   "kind": "began",
// CHECK-NEXT:   "name": "link",
// CHECK-NEXT:   "command": "{{.*}}/ld{{(\\")?}} {{.*}}/parseable_output-[[OUTPUT]].o {{.*}} -o {{.*}}/parseable_output.swift.tmp.out",
// CHECK-NEXT:   "inputs": [
// CHECK-NEXT:     "{{.*}}/parseable_output-[[OUTPUT]].o"
// CHECK-NEXT:   ],
// CHECK-NEXT:   "outputs": [
// CHECK-NEXT:     {
// CHECK-NEXT:       "type": "image",
// CHECK-NEXT:       "path": "{{.*}}/parseable_output.swift.tmp.out"
// CHECK-NEXT:     }
// CHECK-NEXT:   ],
// CHECK-NEXT:   "pid": 3
// CHECK-NEXT: }

// CHECK-NEXT: 110
// CHECK-NEXT: {
// CHECK-NEXT:   "kind": "finished",
// CHECK-NEXT:   "name": "link",
// CHECK-NEXT:   "pid": 3,
// CHECK-NEXT:   "output": "Output placeholder\n",
// CHECK-NEXT:   "exit-status": 0
// CHECK-NEXT: }
