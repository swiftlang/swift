// RUN: %empty-directory(%t)
// RUN: touch %t/file-01.swift %t/file-02.swift %t/file-03.swift
// RUN: echo 'public func main() {}' >%t/main.swift
//
// RUN: %swiftc_driver -enable-batch-mode -parseable-output -c -emit-module -module-name main -j 2 %t/file-01.swift %t/file-02.swift %t/file-03.swift %t/main.swift 2>&1 | %FileCheck %s
//
//
// CHECK: {{[1-9][0-9]*}}
// CHECK-NEXT: {
// CHECK-NEXT:   "kind": "began",
// CHECK-NEXT:   "name": "compile",
// CHECK-NEXT:   "command": "{{.*}}/swift{{c?}} -frontend -c -primary-file {{.*}}/file-01.swift -primary-file {{.*}}/file-02.swift {{.*}}/file-03.swift {{.*}}/main.swift -emit-module-path {{.*}}/file-01-[[MODULE01:[a-z0-9]+]].swiftmodule -emit-module-path {{.*}}/file-02-[[MODULE02:[a-z0-9]+]].swiftmodule -emit-module-doc-path {{.*}}/file-01-[[SWIFTDOC01:[a-z0-9]+]].swiftdoc -emit-module-doc-path {{.*}}/file-02-[[SWIFTDOC02:[a-z0-9]+]].swiftdoc {{.*}} -module-name main -o {{.*}}/file-01-[[OBJ01:[a-z0-9]+]].o -o {{.*}}/file-02-[[OBJ02:[a-z0-9]+]].o",
// CHECK-NEXT:   "command_executable": "{{.*}}/swift{{c?}}",
// CHECK-NEXT:   "command_arguments": [
// CHECK-NEXT:     "-frontend",
// CHECK-NEXT:     "-c",
// CHECK-NEXT:     "-primary-file",
// CHECK-NEXT:     "{{.*}}/file-01.swift",
// CHECK-NEXT:     "-primary-file",
// CHECK-NEXT:     "{{.*}}/file-02.swift",
// CHECK-NEXT:     "{{.*}}/file-03.swift",
// CHECK-NEXT:     "{{.*}}/main.swift",
// CHECK-NEXT:     "-emit-module-path",
// CHECK-NEXT:     "{{.*}}/file-01-[[MODULE01:[a-z0-9]+]].swiftmodule",
// CHECK-NEXT:     "-emit-module-path",
// CHECK-NEXT:     "{{.*}}/file-02-[[MODULE02:[a-z0-9]+]].swiftmodule",
// CHECK-NEXT:     "-emit-module-doc-path",
// CHECK-NEXT:     "{{.*}}/file-01-[[SWIFTDOC01:[a-z0-9]+]].swiftdoc",
// CHECK-NEXT:     "-emit-module-doc-path",
// CHECK-NEXT:     "{{.*}}/file-02-[[SWIFTDOC02:[a-z0-9]+]].swiftdoc",
// CHECK:          "-module-name",
// CHECK-NEXT:     "main",
// CHECK-NEXT:     "-o",
// CHECK-NEXT:     "{{.*}}/file-01-[[OBJ01:[a-z0-9]+]].o",
// CHECK-NEXT:     "-o",
// CHECK-NEXT:     "{{.*}}/file-02-[[OBJ02:[a-z0-9]+]].o"
// CHECK-NEXT:   ],
// CHECK-NEXT:   "inputs": [
// CHECK-NEXT:     "{{.*}}/file-01.swift",
// CHECK-NEXT:     "{{.*}}/file-02.swift"
// CHECK-NEXT:   ],
// CHECK-NEXT:   "outputs": [
// CHECK-NEXT:     {
// CHECK-NEXT:       "type": "object",
// CHECK-NEXT:       "path": "{{.*}}/file-01-[[OBJ01]].o"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "type": "object",
// CHECK-NEXT:       "path": "{{.*}}/file-02-[[OBJ02]].o"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "type": "swiftmodule",
// CHECK-NEXT:       "path": "{{.*}}/file-01-[[MODULE01]].swiftmodule"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "type": "swiftmodule",
// CHECK-NEXT:       "path": "{{.*}}/file-02-[[MODULE02]].swiftmodule"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "type": "swiftdoc",
// CHECK-NEXT:       "path": "{{.*}}/file-01-[[SWIFTDOC01]].swiftdoc"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "type": "swiftdoc",
// CHECK-NEXT:       "path": "{{.*}}/file-02-[[SWIFTDOC02]].swiftdoc"
// CHECK-NEXT:     }
// CHECK-NEXT:   ],
// CHECK-NEXT:   "pid": {{[1-9][0-9]*}}
// CHECK-NEXT: }
// CHECK-NEXT: {{[1-9][0-9]*}}
// CHECK-NEXT: {
// CHECK-NEXT:   "kind": "began",
// CHECK-NEXT:   "name": "compile",
// CHECK-NEXT:   "command": "{{.*}}/swift{{c?}} -frontend -c {{.*}}/file-01.swift {{.*}}/file-02.swift -primary-file {{.*}}/file-03.swift -primary-file {{.*}}/main.swift -emit-module-path {{.*}}/file-03-[[MODULE03:[a-z0-9]+]].swiftmodule -emit-module-path {{.*}}/main-[[MODULEMAIN:[a-z0-9]+]].swiftmodule -emit-module-doc-path {{.*}}/file-03-[[SWIFTDOC03:[a-z0-9]+]].swiftdoc -emit-module-doc-path {{.*}}/main-[[SWIFTDOCMAIN:[a-z0-9]+]].swiftdoc {{.*}} -module-name main -o {{.*}}/file-03-[[OBJ03:[a-z0-9]+]].o -o {{.*}}/main-[[OBJMAIN:[a-z0-9]+]].o",
// CHECK-NEXT:   "command_executable": "{{.*}}/swift{{c?}}",
// CHECK-NEXT:   "command_arguments": [
// CHECK-NEXT:     "-frontend",
// CHECK-NEXT:     "-c",
// CHECK-NEXT:     "{{.*}}/file-01.swift",
// CHECK-NEXT:     "{{.*}}/file-02.swift",
// CHECK-NEXT:     "-primary-file",
// CHECK-NEXT:     "{{.*}}/file-03.swift",
// CHECK-NEXT:     "-primary-file",
// CHECK-NEXT:     "{{.*}}/main.swift",
// CHECK-NEXT:     "-emit-module-path",
// CHECK-NEXT:     "{{.*}}/file-03-[[MODULE03:[a-z0-9]+]].swiftmodule",
// CHECK-NEXT:     "-emit-module-path",
// CHECK-NEXT:     "{{.*}}/main-[[MODULEMAIN:[a-z0-9]+]].swiftmodule",
// CHECK-NEXT:     "-emit-module-doc-path",
// CHECK-NEXT:     "{{.*}}/file-03-[[SWIFTDOC03:[a-z0-9]+]].swiftdoc",
// CHECK-NEXT:     "-emit-module-doc-path",
// CHECK-NEXT:     "{{.*}}/main-[[SWIFTDOCMAIN:[a-z0-9]+]].swiftdoc",
// CHECK:          "-module-name",
// CHECK-NEXT:     "main",
// CHECK-NEXT:     "-o",
// CHECK-NEXT:     "{{.*}}/file-03-[[OBJ03:[a-z0-9]+]].o",
// CHECK-NEXT:     "-o",
// CHECK-NEXT:     "{{.*}}/main-[[OBJMAIN:[a-z0-9]+]].o"
// CHECK-NEXT:   ],
// CHECK-NEXT:   "inputs": [
// CHECK-NEXT:     "{{.*}}/file-03.swift",
// CHECK-NEXT:     "{{.*}}/main.swift"
// CHECK-NEXT:   ],
// CHECK-NEXT:   "outputs": [
// CHECK-NEXT:     {
// CHECK-NEXT:       "type": "object",
// CHECK-NEXT:       "path": "{{.*}}/file-03-[[OBJ03]].o"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "type": "object",
// CHECK-NEXT:       "path": "{{.*}}/main-[[OBJMAIN]].o"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "type": "swiftmodule",
// CHECK-NEXT:       "path": "{{.*}}/file-03-[[MODULE03]].swiftmodule"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "type": "swiftmodule",
// CHECK-NEXT:       "path": "{{.*}}/main-[[MODULEMAIN]].swiftmodule"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "type": "swiftdoc",
// CHECK-NEXT:       "path": "{{.*}}/file-03-[[SWIFTDOC03]].swiftdoc"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "type": "swiftdoc",
// CHECK-NEXT:       "path": "{{.*}}/main-[[SWIFTDOCMAIN]].swiftdoc"
// CHECK-NEXT:     }
// CHECK-NEXT:   ],
// CHECK-NEXT:   "pid": {{[1-9][0-9]*}}
// CHECK-NEXT: }
// CHECK-NEXT: {{[1-9][0-9]*}}
// CHECK-NEXT: {
// CHECK-NEXT:   "kind": "finished",
// CHECK-NEXT:   "name": "compile",
// CHECK-NEXT:   "pid": {{[1-9][0-9]*}},
// CHECK-NEXT:   "exit-status": 0
// CHECK-NEXT: }
// CHECK-NEXT: {{[1-9][0-9]*}}
// CHECK-NEXT: {
// CHECK-NEXT:   "kind": "finished",
// CHECK-NEXT:   "name": "compile",
// CHECK-NEXT:   "pid": {{[1-9][0-9]*}},
// CHECK-NEXT:   "exit-status": 0
// CHECK-NEXT: }
// CHECK-NEXT: {{[1-9][0-9]*}}
// CHECK-NEXT: {
// CHECK-NEXT:   "kind": "began",
// CHECK-NEXT:   "name": "merge-module",
// CHECK-NEXT:   "command": "{{.*}}/swift{{c?}} -frontend -merge-modules -emit-module {{.*}}/file-01-[[MODULE01]].swiftmodule {{.*}}/file-02-[[MODULE02]].swiftmodule {{.*}}/file-03-[[MODULE03]].swiftmodule {{.*}}/main-[[MODULEMAIN]].swiftmodule {{.*}} -emit-module-doc-path main.swiftdoc -module-name main -o main.swiftmodule",
// CHECK-NEXT:   "command_executable": "{{.*}}/swift{{c?}}",
// CHECK-NEXT:   "command_arguments": [
// CHECK-NEXT:     "-frontend",
// CHECK-NEXT:     "-merge-modules",
// CHECK-NEXT:     "-emit-module",
// CHECK-NEXT:     "{{.*}}/file-01-[[MODULE01]].swiftmodule",
// CHECK-NEXT:     "{{.*}}/file-02-[[MODULE02]].swiftmodule",
// CHECK-NEXT:     "{{.*}}/file-03-[[MODULE03]].swiftmodule",
// CHECK-NEXT:     "{{.*}}/main-[[MODULEMAIN]].swiftmodule",
// CHECK:          "-emit-module-doc-path",
// CHECK-NEXT:     "main.swiftdoc",
// CHECK-NEXT:     "-module-name",
// CHECK-NEXT:     "main",
// CHECK-NEXT:     "-o",
// CHECK-NEXT:     "main.swiftmodule"
// CHECK-NEXT:   ],
// CHECK-NEXT:   "inputs": [
// CHECK-NEXT:     "{{.*}}/file-01-[[OBJ01]].o",
// CHECK-NEXT:     "{{.*}}/file-02-[[OBJ02]].o",
// CHECK-NEXT:     "{{.*}}/file-03-[[OBJ03]].o",
// CHECK-NEXT:     "{{.*}}/main-[[OBJMAIN]].o"
// CHECK-NEXT:   ],
// CHECK-NEXT:   "outputs": [
// CHECK-NEXT:     {
// CHECK-NEXT:       "type": "swiftmodule",
// CHECK-NEXT:       "path": "main.swiftmodule"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "type": "swiftdoc",
// CHECK-NEXT:       "path": "main.swiftdoc"
// CHECK-NEXT:     }
// CHECK-NEXT:   ],
// CHECK-NEXT:   "pid": {{[1-9][0-9]*}}
// CHECK-NEXT: }
// CHECK-NEXT: {{[1-9][0-9]*}}
// CHECK-NEXT: {
// CHECK-NEXT:   "kind": "finished",
// CHECK-NEXT:   "name": "merge-module",
// CHECK-NEXT:   "pid": {{[1-9][0-9]*}},
// CHECK-NEXT:   "exit-status": 0
// CHECK-NEXT: }
