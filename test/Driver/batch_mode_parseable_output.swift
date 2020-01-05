
// First, with coarse-grained-dependencies
//
// RUN: %empty-directory(%t)
// RUN: touch %t/file-01.swift %t/file-02.swift %t/file-03.swift
// RUN: echo 'public func main() {}' >%t/main.swift
//
// RUN: %swiftc_driver -disable-fine-grained-dependencies -enable-batch-mode -parseable-output -driver-skip-execution -c -emit-module -module-name main -j 2 %t/file-01.swift %t/file-02.swift %t/file-03.swift %t/main.swift 2>&1 | %FileCheck -check-prefix CHECK-COARSE %s
//
//
// CHECK-COARSE: {{[1-9][0-9]*}}
// CHECK-COARSE-NEXT: {
// CHECK-COARSE-NEXT:   "kind": "began",
// CHECK-COARSE-NEXT:   "name": "compile",
// CHECK-COARSE-NEXT:   "command": "{{.*[\\/]}}swift{{c?(\.exe)?(\\")?}} -frontend -c -primary-file {{.*}}/file-01.swift{{(\\")?}} {{.*}}file-02.swift{{(\\")?}} {{.*}}file-03.swift{{(\\")?}} {{.*}}main.swift{{(\\")?}} -emit-module-path {{.*}}file-01-[[MODULE01:[a-z0-9]+]].swiftmodule{{(\\")?}} -emit-module-doc-path {{.*}}file-01-[[SWIFTDOC01:[a-z0-9]+]].swiftdoc{{(\\")?}} {{.*}} -disable-fine-grained-dependencies {{.*}} -module-name main -o {{.*}}file-01-[[OBJ01:[a-z0-9]+]].o{{(\\")?}}",
// CHECK-COARSE-NEXT:   "command_executable": "{{.*[\\/]}}swift{{c?(\.exe)?}}",
// CHECK-COARSE-NEXT:   "command_arguments": [
// CHECK-COARSE-NEXT:     "-frontend",
// CHECK-COARSE-NEXT:     "-c",
// CHECK-COARSE-NEXT:     "-primary-file",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-01.swift",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-02.swift",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-03.swift",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}main.swift",
// CHECK-COARSE-NEXT:     "-emit-module-path",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-01-[[MODULE01:[a-z0-9]+]].swiftmodule",
// CHECK-COARSE-NEXT:     "-emit-module-doc-path",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-01-[[SWIFTDOC01:[a-z0-9]+]].swiftdoc",
// CHECK-COARSE:          "-disable-fine-grained-dependencies",
// CHECK-COARSE:          "-module-name",
// CHECK-COARSE-NEXT:     "main",
// CHECK-COARSE-NEXT:     "-o",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-01-[[OBJ01:[a-z0-9]+]].o"
// CHECK-COARSE-NEXT:   ],
// CHECK-COARSE-NEXT:   "inputs": [
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-01.swift"
// CHECK-COARSE-NEXT:   ],
// CHECK-COARSE-NEXT:   "outputs": [
// CHECK-COARSE-NEXT:     {
// CHECK-COARSE-NEXT:       "type": "object",
// CHECK-COARSE-NEXT:       "path": "{{.*[\\/]}}file-01-[[OBJ01]].o"
// CHECK-COARSE-NEXT:     },
// CHECK-COARSE-NEXT:     {
// CHECK-COARSE-NEXT:       "type": "swiftmodule",
// CHECK-COARSE-NEXT:       "path": "{{.*[\\/]}}file-01-[[MODULE01]].swiftmodule"
// CHECK-COARSE-NEXT:     },
// CHECK-COARSE-NEXT:     {
// CHECK-COARSE-NEXT:       "type": "swiftdoc",
// CHECK-COARSE-NEXT:       "path": "{{.*[\\/]}}file-01-[[SWIFTDOC01]].swiftdoc"
// CHECK-COARSE-NEXT:     },
// CHECK-COARSE-NEXT:     {
// CHECK-COARSE-NEXT:       "type": "swiftsourceinfo",
// CHECK-COARSE-NEXT:       "path": "{{.*[\\/]}}file-01-[[MODULE01]].swiftsourceinfo"
// CHECK-COARSE-NEXT:     }
// CHECK-COARSE-NEXT:   ],
// CHECK-COARSE-NEXT:   "pid": -{{[1-9][0-9]*}},
// CHECK-COARSE-NEXT:   "process": {
// CHECK-COARSE-NEXT:   	"real_pid": {{[1-9][0-9]*}}
// CHECK-COARSE-NEXT:   }
// CHECK-COARSE-NEXT: }
// CHECK-COARSE: {{[1-9][0-9]*}}
// CHECK-COARSE-NEXT: {
// CHECK-COARSE-NEXT:   "kind": "began",
// CHECK-COARSE-NEXT:   "name": "compile",
// CHECK-COARSE-NEXT:   "command": "{{.*[\\/]}}swift{{c?(\.exe)?(\\")?}} -frontend -c {{.*}}file-01.swift{{(\\")?}} -primary-file {{.*}}file-02.swift{{(\\")?}} {{.*}}file-03.swift{{(\\")?}} {{.*}}main.swift{{(\\")?}} -emit-module-path {{.*}}file-02-[[MODULE02:[a-z0-9]+]].swiftmodule{{(\\")?}} -emit-module-doc-path {{.*}}file-02-[[SWIFTDOC02:[a-z0-9]+]].swiftdoc{{(\\")?}} {{.*}} -disable-fine-grained-dependencies {{.*}} -module-name main -o {{.*}}file-02-[[OBJ02:[a-z0-9]+]].o{{(\\")?}}",
// CHECK-COARSE-NEXT:   "command_executable": "{{.*[\\/]}}swift{{c?(\.exe)?(\\")?}}",
// CHECK-COARSE-NEXT:   "command_arguments": [
// CHECK-COARSE-NEXT:     "-frontend",
// CHECK-COARSE-NEXT:     "-c",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-01.swift",
// CHECK-COARSE-NEXT:     "-primary-file",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-02.swift",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-03.swift",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}main.swift",
// CHECK-COARSE-NEXT:     "-emit-module-path",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-02-[[MODULE02:[a-z0-9]+]].swiftmodule",
// CHECK-COARSE-NEXT:     "-emit-module-doc-path",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-02-[[SWIFTDOC02:[a-z0-9]+]].swiftdoc",
// CHECK-COARSE:          "-disable-fine-grained-dependencies",
// CHECK-COARSE:          "-module-name",
// CHECK-COARSE-NEXT:     "main",
// CHECK-COARSE-NEXT:     "-o",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-02-[[OBJ02:[a-z0-9]+]].o"
// CHECK-COARSE-NEXT:   ],
// CHECK-COARSE-NEXT:   "inputs": [
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-02.swift"
// CHECK-COARSE-NEXT:   ],
// CHECK-COARSE-NEXT:   "outputs": [
// CHECK-COARSE-NEXT:     {
// CHECK-COARSE-NEXT:       "type": "object",
// CHECK-COARSE-NEXT:       "path": "{{.*[\\/]}}file-02-[[OBJ02]].o"
// CHECK-COARSE-NEXT:     },
// CHECK-COARSE-NEXT:     {
// CHECK-COARSE-NEXT:       "type": "swiftmodule",
// CHECK-COARSE-NEXT:       "path": "{{.*[\\/]}}file-02-[[MODULE02]].swiftmodule"
// CHECK-COARSE-NEXT:     },
// CHECK-COARSE-NEXT:     {
// CHECK-COARSE-NEXT:       "type": "swiftdoc",
// CHECK-COARSE-NEXT:       "path": "{{.*[\\/]}}file-02-[[SWIFTDOC02]].swiftdoc"
// CHECK-COARSE-NEXT:     },
// CHECK-COARSE-NEXT:     {
// CHECK-COARSE-NEXT:       "type": "swiftsourceinfo",
// CHECK-COARSE-NEXT:       "path": "{{.*[\\/]}}file-02-[[MODULE02]].swiftsourceinfo"
// CHECK-COARSE-NEXT:     }
// CHECK-COARSE-NEXT:   ],
// CHECK-COARSE-NEXT:   "pid": -{{[1-9][0-9]*}},
// CHECK-COARSE-NEXT:   "process": {
// CHECK-COARSE-NEXT:   	"real_pid": {{[1-9][0-9]*}}
// CHECK-COARSE-NEXT:   }
// CHECK-COARSE-NEXT: }
// CHECK-COARSE: {{[1-9][0-9]*}}
// CHECK-COARSE-NEXT: {
// CHECK-COARSE-NEXT:   "kind": "began",
// CHECK-COARSE-NEXT:   "name": "compile",
// CHECK-COARSE-NEXT:   "command": "{{.*}}swift{{c?(\.exe)?(\\")?}} -frontend -c {{.*}}file-01.swift{{(\\")?}} {{.*}}file-02.swift{{(\\")?}} -primary-file {{.*}}file-03.swift{{(\\")?}} {{.*}}main.swift{{(\\")?}} -emit-module-path {{.*}}file-03-[[MODULE03:[a-z0-9]+]].swiftmodule{{(\\")?}} -emit-module-doc-path {{.*}}file-03-[[SWIFTDOC03:[a-z0-9]+]].swiftdoc{{(\\")?}} {{.*}} -disable-fine-grained-dependencies {{.*}} -module-name main -o {{.*}}file-03-[[OBJ03:[a-z0-9]+]].o{{(\\")?}}",
// CHECK-COARSE-NEXT:   "command_executable": "{{.*[\\/]}}swift{{c?(\.exe)?}}",
// CHECK-COARSE-NEXT:   "command_arguments": [
// CHECK-COARSE-NEXT:     "-frontend",
// CHECK-COARSE-NEXT:     "-c",
// CHECK-COARSE-NEXT:     "{{.*}}/file-01.swift",
// CHECK-COARSE-NEXT:     "{{.*}}/file-02.swift",
// CHECK-COARSE-NEXT:     "-primary-file",
// CHECK-COARSE-NEXT:     "{{.*}}/file-03.swift",
// CHECK-COARSE-NEXT:     "{{.*}}/main.swift",
// CHECK-COARSE-NEXT:     "-emit-module-path",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-03-[[MODULE03:[a-z0-9]+]].swiftmodule",
// CHECK-COARSE-NEXT:     "-emit-module-doc-path",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-03-[[SWIFTDOC03:[a-z0-9]+]].swiftdoc",
// CHECK-COARSE:          "-disable-fine-grained-dependencies",
// CHECK-COARSE:          "-module-name",
// CHECK-COARSE-NEXT:     "main",
// CHECK-COARSE-NEXT:     "-o",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-03-[[OBJ03:[a-z0-9]+]].o"
// CHECK-COARSE-NEXT:   ],
// CHECK-COARSE-NEXT:   "inputs": [
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-03.swift"
// CHECK-COARSE-NEXT:   ],
// CHECK-COARSE-NEXT:   "outputs": [
// CHECK-COARSE-NEXT:     {
// CHECK-COARSE-NEXT:       "type": "object",
// CHECK-COARSE-NEXT:       "path": "{{.*[\\/]}}file-03-[[OBJ03]].o"
// CHECK-COARSE-NEXT:     },
// CHECK-COARSE-NEXT:     {
// CHECK-COARSE-NEXT:       "type": "swiftmodule",
// CHECK-COARSE-NEXT:       "path": "{{.*[\\/]}}file-03-[[MODULE03]].swiftmodule"
// CHECK-COARSE-NEXT:     },
// CHECK-COARSE-NEXT:     {
// CHECK-COARSE-NEXT:       "type": "swiftdoc",
// CHECK-COARSE-NEXT:       "path": "{{.*[\\/]}}file-03-[[SWIFTDOC03]].swiftdoc"
// CHECK-COARSE-NEXT:     },
// CHECK-COARSE-NEXT:     {
// CHECK-COARSE-NEXT:       "type": "swiftsourceinfo",
// CHECK-COARSE-NEXT:       "path": "{{.*[\\/]}}file-03-[[MODULE03]].swiftsourceinfo"
// CHECK-COARSE-NEXT:     }
// CHECK-COARSE-NEXT:   ],
// CHECK-COARSE-NEXT:   "pid": -{{[1-9][0-9]*}},
// CHECK-COARSE-NEXT:   "process": {
// CHECK-COARSE-NEXT:   	"real_pid": {{[1-9][0-9]*}}
// CHECK-COARSE-NEXT:   }
// CHECK-COARSE-NEXT: }
// CHECK-COARSE: {{[1-9][0-9]*}}
// CHECK-COARSE-NEXT: {
// CHECK-COARSE-NEXT:   "kind": "began",
// CHECK-COARSE-NEXT:   "name": "compile",
// CHECK-COARSE-NEXT:   "command": "{{.*[\\/]}}swift{{c?(\.exe)?(\\")?}} -frontend -c {{.*[\\/]}}file-01.swift{{(\\")?}} {{.*[\\/]}}file-02.swift{{(\\")?}} {{.*[\\/]}}file-03.swift{{(\\")?}} -primary-file {{.*[\\/]}}main.swift{{(\\")?}} -emit-module-path {{.*[\\/]}}main-[[MODULEMAIN:[a-z0-9]+]].swiftmodule{{(\\")?}} -emit-module-doc-path {{.*[\\/]}}main-[[SWIFTDOCMAIN:[a-z0-9]+]].swiftdoc{{(\\")?}} {{.*}} -disable-fine-grained-dependencies {{.*}} -module-name main -o {{.*[\\/]}}main-[[OBJMAIN:[a-z0-9]+]].o{{(\\")?}}",
// CHECK-COARSE-NEXT:   "command_executable": "{{.*[\\/]}}swift{{c?(\.exe)?}}",
// CHECK-COARSE-NEXT:   "command_arguments": [
// CHECK-COARSE-NEXT:     "-frontend",
// CHECK-COARSE-NEXT:     "-c",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-01.swift",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-02.swift",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-03.swift",
// CHECK-COARSE-NEXT:     "-primary-file",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}main.swift",
// CHECK-COARSE-NEXT:     "-emit-module-path",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}main-[[MODULEMAIN:[a-z0-9]+]].swiftmodule",
// CHECK-COARSE-NEXT:     "-emit-module-doc-path",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}main-[[SWIFTDOCMAIN:[a-z0-9]+]].swiftdoc",
// CHECK-COARSE:          "-disable-fine-grained-dependencies",
// CHECK-COARSE:          "-module-name",
// CHECK-COARSE-NEXT:     "main",
// CHECK-COARSE-NEXT:     "-o",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}main-[[OBJMAIN:[a-z0-9]+]].o"
// CHECK-COARSE-NEXT:   ],
// CHECK-COARSE-NEXT:   "inputs": [
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}main.swift"
// CHECK-COARSE-NEXT:   ],
// CHECK-COARSE-NEXT:   "outputs": [
// CHECK-COARSE-NEXT:     {
// CHECK-COARSE-NEXT:       "type": "object",
// CHECK-COARSE-NEXT:       "path": "{{.*[\\/]}}main-[[OBJMAIN]].o"
// CHECK-COARSE-NEXT:     },
// CHECK-COARSE-NEXT:     {
// CHECK-COARSE-NEXT:       "type": "swiftmodule",
// CHECK-COARSE-NEXT:       "path": "{{.*[\\/]}}main-[[MODULEMAIN]].swiftmodule"
// CHECK-COARSE-NEXT:     },
// CHECK-COARSE-NEXT:     {
// CHECK-COARSE-NEXT:       "type": "swiftdoc",
// CHECK-COARSE-NEXT:       "path": "{{.*[\\/]}}main-[[SWIFTDOCMAIN]].swiftdoc"
// CHECK-COARSE-NEXT:     },
// CHECK-COARSE-NEXT:     {
// CHECK-COARSE-NEXT:       "type": "swiftsourceinfo",
// CHECK-COARSE-NEXT:       "path": "{{.*[\\/]}}main-[[MODULEMAIN]].swiftsourceinfo"
// CHECK-COARSE-NEXT:     }
// CHECK-COARSE-NEXT:   ],
// CHECK-COARSE-NEXT:   "pid": -{{[1-9][0-9]*}},
// CHECK-COARSE-NEXT:   "process": {
// CHECK-COARSE-NEXT:   	"real_pid": {{[1-9][0-9]*}}
// CHECK-COARSE-NEXT:   }
// CHECK-COARSE-NEXT: }
// CHECK-COARSE-NEXT: {{[1-9][0-9]*}}
// CHECK-COARSE-NEXT: {
// CHECK-COARSE-NEXT:   "kind": "finished",
// CHECK-COARSE-NEXT:   "name": "compile",
// CHECK-COARSE-NEXT:   "pid": -{{[1-9][0-9]*}},
// CHECK-COARSE-NEXT:   "output": "Output placeholder\n",
// CHECK-COARSE-NEXT:   "process": {
// CHECK-COARSE-NEXT:   	"real_pid": {{[1-9][0-9]*}}
// CHECK-COARSE-NEXT:   },
// CHECK-COARSE-NEXT:   "exit-status": 0
// CHECK-COARSE-NEXT: }
// CHECK-COARSE-NEXT: {{[1-9][0-9]*}}
// CHECK-COARSE-NEXT: {
// CHECK-COARSE-NEXT:   "kind": "finished",
// CHECK-COARSE-NEXT:   "name": "compile",
// CHECK-COARSE-NEXT:   "pid": -{{[1-9][0-9]*}},
// CHECK-COARSE-NEXT:   "output": "Output placeholder\n",
// CHECK-COARSE-NEXT:   "process": {
// CHECK-COARSE-NEXT:   	"real_pid": {{[1-9][0-9]*}}
// CHECK-COARSE-NEXT:   },
// CHECK-COARSE-NEXT:   "exit-status": 0
// CHECK-COARSE-NEXT: }
// CHECK-COARSE-NEXT: {{[1-9][0-9]*}}
// CHECK-COARSE-NEXT: {
// CHECK-COARSE-NEXT:   "kind": "finished",
// CHECK-COARSE-NEXT:   "name": "compile",
// CHECK-COARSE-NEXT:   "pid": -{{[1-9][0-9]*}},
// CHECK-COARSE-NEXT:   "output": "Output placeholder\n",
// CHECK-COARSE-NEXT:   "process": {
// CHECK-COARSE-NEXT:   	"real_pid": {{[1-9][0-9]*}}
// CHECK-COARSE-NEXT:   },
// CHECK-COARSE-NEXT:   "exit-status": 0
// CHECK-COARSE-NEXT: }
// CHECK-COARSE-NEXT: {{[1-9][0-9]*}}
// CHECK-COARSE-NEXT: {
// CHECK-COARSE-NEXT:   "kind": "finished",
// CHECK-COARSE-NEXT:   "name": "compile",
// CHECK-COARSE-NEXT:   "pid": -{{[1-9][0-9]*}},
// CHECK-COARSE-NEXT:   "output": "Output placeholder\n",
// CHECK-COARSE-NEXT:   "process": {
// CHECK-COARSE-NEXT:   	"real_pid": {{[1-9][0-9]*}}
// CHECK-COARSE-NEXT:   },
// CHECK-COARSE-NEXT:   "exit-status": 0
// CHECK-COARSE-NEXT: }
// CHECK-COARSE-NEXT: {{[1-9][0-9]*}}
// CHECK-COARSE-NEXT: {
// CHECK-COARSE-NEXT:   "kind": "began",
// CHECK-COARSE-NEXT:   "name": "merge-module",
// CHECK-COARSE-NEXT:   "command": "{{.*[\\/]}}swift{{c?(\.exe)?(\\")?}} -frontend -merge-modules -emit-module {{.*[\\/]}}file-01-[[MODULE01]].swiftmodule{{(\\")?}} {{.*[\\/]}}file-02-[[MODULE02]].swiftmodule{{(\\")?}} {{.*[\\/]}}file-03-[[MODULE03]].swiftmodule{{(\\")?}} {{.*[\\/]}}main-[[MODULEMAIN]].swiftmodule{{(\\")?}} {{.*}} -emit-module-doc-path main.swiftdoc -emit-module-source-info-path main.swiftsourceinfo -module-name main -o main.swiftmodule",
// CHECK-COARSE-NEXT:   "command_executable": "{{.*[\\/]}}swift{{c?(\.exe)?}}",
// CHECK-COARSE-NEXT:   "command_arguments": [
// CHECK-COARSE-NEXT:     "-frontend",
// CHECK-COARSE-NEXT:     "-merge-modules",
// CHECK-COARSE-NEXT:     "-emit-module",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-01-[[MODULE01]].swiftmodule",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-02-[[MODULE02]].swiftmodule",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-03-[[MODULE03]].swiftmodule",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}main-[[MODULEMAIN]].swiftmodule",
// CHECK-COARSE:          "-emit-module-doc-path",
// CHECK-COARSE-NEXT:     "main.swiftdoc",
// CHECK-COARSE:          "-emit-module-source-info-path",
// CHECK-COARSE-NEXT:     "main.swiftsourceinfo",
// CHECK-COARSE-NEXT:     "-module-name",
// CHECK-COARSE-NEXT:     "main",
// CHECK-COARSE-NEXT:     "-o",
// CHECK-COARSE-NEXT:     "main.swiftmodule"
// CHECK-COARSE-NEXT:   ],
// CHECK-COARSE-NEXT:   "inputs": [
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-01-[[OBJ01]].o",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-02-[[OBJ02]].o",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}file-03-[[OBJ03]].o",
// CHECK-COARSE-NEXT:     "{{.*[\\/]}}main-[[OBJMAIN]].o"
// CHECK-COARSE-NEXT:   ],
// CHECK-COARSE-NEXT:   "outputs": [
// CHECK-COARSE-NEXT:     {
// CHECK-COARSE-NEXT:       "type": "swiftmodule",
// CHECK-COARSE-NEXT:       "path": "main.swiftmodule"
// CHECK-COARSE-NEXT:     },
// CHECK-COARSE-NEXT:     {
// CHECK-COARSE-NEXT:       "type": "swiftdoc",
// CHECK-COARSE-NEXT:       "path": "main.swiftdoc"
// CHECK-COARSE-NEXT:     },
// CHECK-COARSE-NEXT:     {
// CHECK-COARSE-NEXT:       "type": "swiftsourceinfo",
// CHECK-COARSE-NEXT:       "path": "main.swiftsourceinfo"
// CHECK-COARSE-NEXT:     }
// CHECK-COARSE-NEXT:   ],
// CHECK-COARSE-NEXT:   "pid": {{[1-9][0-9]*}},
// CHECK-COARSE-NEXT:   "process": {
// CHECK-COARSE-NEXT:   	"real_pid": {{[1-9][0-9]*}}
// CHECK-COARSE-NEXT:   }
// CHECK-COARSE-NEXT: }
// CHECK-COARSE-NEXT: {{[1-9][0-9]*}}
// CHECK-COARSE-NEXT: {
// CHECK-COARSE-NEXT:   "kind": "finished",
// CHECK-COARSE-NEXT:   "name": "merge-module",
// CHECK-COARSE-NEXT:   "pid": {{[1-9][0-9]*}},
// CHECK-COARSE-NEXT:   "output": "Output placeholder\n",
// CHECK-COARSE-NEXT:   "process": {
// CHECK-COARSE-NEXT:   	"real_pid": {{[1-9][0-9]*}}
// CHECK-COARSE-NEXT:   },
// CHECK-COARSE-NEXT:   "exit-status": 0
// CHECK-COARSE-NEXT: }


// With fine-grained-dependencies
//
// RUN: %empty-directory(%t)
// RUN: touch %t/file-01.swift %t/file-02.swift %t/file-03.swift
// RUN: echo 'public func main() {}' >%t/main.swift
//
// RUN: %swiftc_driver -enable-fine-grained-dependencies -enable-batch-mode -parseable-output -driver-skip-execution -c -emit-module -module-name main -j 2 %t/file-01.swift %t/file-02.swift %t/file-03.swift %t/main.swift 2>&1 | %FileCheck -check-prefix CHECK-FINE %s
//
//
// CHECK-FINE: {{[1-9][0-9]*}}
// CHECK-FINE-NEXT: {
// CHECK-FINE-NEXT:   "kind": "began",
// CHECK-FINE-NEXT:   "name": "compile",
// CHECK-FINE-NEXT:   "command": "{{.*[\\/]}}swift{{c?(\.exe)?(\\")?}} -frontend -c -primary-file {{.*}}/file-01.swift{{(\\")?}} {{.*}}file-02.swift{{(\\")?}} {{.*}}file-03.swift{{(\\")?}} {{.*}}main.swift{{(\\")?}} -emit-module-path {{.*}}file-01-[[MODULE01:[a-z0-9]+]].swiftmodule{{(\\")?}} -emit-module-doc-path {{.*}}file-01-[[SWIFTDOC01:[a-z0-9]+]].swiftdoc{{(\\")?}} {{.*}} -enable-fine-grained-dependencies {{.*}} -module-name main -o {{.*}}file-01-[[OBJ01:[a-z0-9]+]].o{{(\\")?}}",
// CHECK-FINE-NEXT:   "command_executable": "{{.*[\\/]}}swift{{c?(\.exe)?}}",
// CHECK-FINE-NEXT:   "command_arguments": [
// CHECK-FINE-NEXT:     "-frontend",
// CHECK-FINE-NEXT:     "-c",
// CHECK-FINE-NEXT:     "-primary-file",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-01.swift",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-02.swift",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-03.swift",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}main.swift",
// CHECK-FINE-NEXT:     "-emit-module-path",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-01-[[MODULE01:[a-z0-9]+]].swiftmodule",
// CHECK-FINE-NEXT:     "-emit-module-doc-path",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-01-[[SWIFTDOC01:[a-z0-9]+]].swiftdoc",
// CHECK-FINE:          "-enable-fine-grained-dependencies",
// CHECK-FINE:          "-module-name",
// CHECK-FINE-NEXT:     "main",
// CHECK-FINE-NEXT:     "-o",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-01-[[OBJ01:[a-z0-9]+]].o"
// CHECK-FINE-NEXT:   ],
// CHECK-FINE-NEXT:   "inputs": [
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-01.swift"
// CHECK-FINE-NEXT:   ],
// CHECK-FINE-NEXT:   "outputs": [
// CHECK-FINE-NEXT:     {
// CHECK-FINE-NEXT:       "type": "object",
// CHECK-FINE-NEXT:       "path": "{{.*[\\/]}}file-01-[[OBJ01]].o"
// CHECK-FINE-NEXT:     },
// CHECK-FINE-NEXT:     {
// CHECK-FINE-NEXT:       "type": "swiftmodule",
// CHECK-FINE-NEXT:       "path": "{{.*[\\/]}}file-01-[[MODULE01]].swiftmodule"
// CHECK-FINE-NEXT:     },
// CHECK-FINE-NEXT:     {
// CHECK-FINE-NEXT:       "type": "swiftdoc",
// CHECK-FINE-NEXT:       "path": "{{.*[\\/]}}file-01-[[SWIFTDOC01]].swiftdoc"
// CHECK-FINE-NEXT:     },
// CHECK-FINE-NEXT:     {
// CHECK-FINE-NEXT:       "type": "swiftsourceinfo",
// CHECK-FINE-NEXT:       "path": "{{.*[\\/]}}file-01-[[MODULE01]].swiftsourceinfo"
// CHECK-FINE-NEXT:     }
// CHECK-FINE-NEXT:   ],
// CHECK-FINE-NEXT:   "pid": -{{[1-9][0-9]*}},
// CHECK-FINE-NEXT:   "process": {
// CHECK-FINE-NEXT:     "real_pid": {{[1-9][0-9]*}}
// CHECK-FINE-NEXT:   }
// CHECK-FINE-NEXT: }
// CHECK-FINE: {{[1-9][0-9]*}}
// CHECK-FINE-NEXT: {
// CHECK-FINE-NEXT:   "kind": "began",
// CHECK-FINE-NEXT:   "name": "compile",
// CHECK-FINE-NEXT:   "command": "{{.*[\\/]}}swift{{c?(\.exe)?(\\")?}} -frontend -c {{.*}}file-01.swift{{(\\")?}} -primary-file {{.*}}file-02.swift{{(\\")?}} {{.*}}file-03.swift{{(\\")?}} {{.*}}main.swift{{(\\")?}} -emit-module-path {{.*}}file-02-[[MODULE02:[a-z0-9]+]].swiftmodule{{(\\")?}} -emit-module-doc-path {{.*}}file-02-[[SWIFTDOC02:[a-z0-9]+]].swiftdoc{{(\\")?}} {{.*}} -enable-fine-grained-dependencies {{.*}} -module-name main -o {{.*}}file-02-[[OBJ02:[a-z0-9]+]].o{{(\\")?}}",
// CHECK-FINE-NEXT:   "command_executable": "{{.*[\\/]}}swift{{c?(\.exe)?(\\")?}}",
// CHECK-FINE-NEXT:   "command_arguments": [
// CHECK-FINE-NEXT:     "-frontend",
// CHECK-FINE-NEXT:     "-c",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-01.swift",
// CHECK-FINE-NEXT:     "-primary-file",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-02.swift",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-03.swift",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}main.swift",
// CHECK-FINE-NEXT:     "-emit-module-path",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-02-[[MODULE02:[a-z0-9]+]].swiftmodule",
// CHECK-FINE-NEXT:     "-emit-module-doc-path",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-02-[[SWIFTDOC02:[a-z0-9]+]].swiftdoc",
// CHECK-FINE:          "-enable-fine-grained-dependencies",
// CHECK-FINE:          "-module-name",
// CHECK-FINE-NEXT:     "main",
// CHECK-FINE-NEXT:     "-o",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-02-[[OBJ02:[a-z0-9]+]].o"
// CHECK-FINE-NEXT:   ],
// CHECK-FINE-NEXT:   "inputs": [
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-02.swift"
// CHECK-FINE-NEXT:   ],
// CHECK-FINE-NEXT:   "outputs": [
// CHECK-FINE-NEXT:     {
// CHECK-FINE-NEXT:       "type": "object",
// CHECK-FINE-NEXT:       "path": "{{.*[\\/]}}file-02-[[OBJ02]].o"
// CHECK-FINE-NEXT:     },
// CHECK-FINE-NEXT:     {
// CHECK-FINE-NEXT:       "type": "swiftmodule",
// CHECK-FINE-NEXT:       "path": "{{.*[\\/]}}file-02-[[MODULE02]].swiftmodule"
// CHECK-FINE-NEXT:     },
// CHECK-FINE-NEXT:     {
// CHECK-FINE-NEXT:       "type": "swiftdoc",
// CHECK-FINE-NEXT:       "path": "{{.*[\\/]}}file-02-[[SWIFTDOC02]].swiftdoc"
// CHECK-FINE-NEXT:     },
// CHECK-FINE-NEXT:     {
// CHECK-FINE-NEXT:       "type": "swiftsourceinfo",
// CHECK-FINE-NEXT:       "path": "{{.*[\\/]}}file-02-[[MODULE02]].swiftsourceinfo"
// CHECK-FINE-NEXT:     }
// CHECK-FINE-NEXT:   ],
// CHECK-FINE-NEXT:   "pid": -{{[1-9][0-9]*}},
// CHECK-FINE-NEXT:   "process": {
// CHECK-FINE-NEXT:     "real_pid": {{[1-9][0-9]*}}
// CHECK-FINE-NEXT:   }
// CHECK-FINE-NEXT: }
// CHECK-FINE: {{[1-9][0-9]*}}
// CHECK-FINE-NEXT: {
// CHECK-FINE-NEXT:   "kind": "began",
// CHECK-FINE-NEXT:   "name": "compile",
// CHECK-FINE-NEXT:   "command": "{{.*}}swift{{c?(\.exe)?(\\")?}} -frontend -c {{.*}}file-01.swift{{(\\")?}} {{.*}}file-02.swift{{(\\")?}} -primary-file {{.*}}file-03.swift{{(\\")?}} {{.*}}main.swift{{(\\")?}} -emit-module-path {{.*}}file-03-[[MODULE03:[a-z0-9]+]].swiftmodule{{(\\")?}} -emit-module-doc-path {{.*}}file-03-[[SWIFTDOC03:[a-z0-9]+]].swiftdoc{{(\\")?}} {{.*}} -enable-fine-grained-dependencies {{.*}} -module-name main -o {{.*}}file-03-[[OBJ03:[a-z0-9]+]].o{{(\\")?}}",
// CHECK-FINE-NEXT:   "command_executable": "{{.*[\\/]}}swift{{c?(\.exe)?}}",
// CHECK-FINE-NEXT:   "command_arguments": [
// CHECK-FINE-NEXT:     "-frontend",
// CHECK-FINE-NEXT:     "-c",
// CHECK-FINE-NEXT:     "{{.*}}/file-01.swift",
// CHECK-FINE-NEXT:     "{{.*}}/file-02.swift",
// CHECK-FINE-NEXT:     "-primary-file",
// CHECK-FINE-NEXT:     "{{.*}}/file-03.swift",
// CHECK-FINE-NEXT:     "{{.*}}/main.swift",
// CHECK-FINE-NEXT:     "-emit-module-path",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-03-[[MODULE03:[a-z0-9]+]].swiftmodule",
// CHECK-FINE-NEXT:     "-emit-module-doc-path",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-03-[[SWIFTDOC03:[a-z0-9]+]].swiftdoc",
// CHECK-FINE:          "-enable-fine-grained-dependencies",
// CHECK-FINE:          "-module-name",
// CHECK-FINE-NEXT:     "main",
// CHECK-FINE-NEXT:     "-o",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-03-[[OBJ03:[a-z0-9]+]].o"
// CHECK-FINE-NEXT:   ],
// CHECK-FINE-NEXT:   "inputs": [
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-03.swift"
// CHECK-FINE-NEXT:   ],
// CHECK-FINE-NEXT:   "outputs": [
// CHECK-FINE-NEXT:     {
// CHECK-FINE-NEXT:       "type": "object",
// CHECK-FINE-NEXT:       "path": "{{.*[\\/]}}file-03-[[OBJ03]].o"
// CHECK-FINE-NEXT:     },
// CHECK-FINE-NEXT:     {
// CHECK-FINE-NEXT:       "type": "swiftmodule",
// CHECK-FINE-NEXT:       "path": "{{.*[\\/]}}file-03-[[MODULE03]].swiftmodule"
// CHECK-FINE-NEXT:     },
// CHECK-FINE-NEXT:     {
// CHECK-FINE-NEXT:       "type": "swiftdoc",
// CHECK-FINE-NEXT:       "path": "{{.*[\\/]}}file-03-[[SWIFTDOC03]].swiftdoc"
// CHECK-FINE-NEXT:     },
// CHECK-FINE-NEXT:     {
// CHECK-FINE-NEXT:       "type": "swiftsourceinfo",
// CHECK-FINE-NEXT:       "path": "{{.*[\\/]}}file-03-[[MODULE03]].swiftsourceinfo"
// CHECK-FINE-NEXT:     }
// CHECK-FINE-NEXT:   ],
// CHECK-FINE-NEXT:   "pid": -{{[1-9][0-9]*}},
// CHECK-FINE-NEXT:   "process": {
// CHECK-FINE-NEXT:     "real_pid": {{[1-9][0-9]*}}
// CHECK-FINE-NEXT:   }
// CHECK-FINE-NEXT: }
// CHECK-FINE: {{[1-9][0-9]*}}
// CHECK-FINE-NEXT: {
// CHECK-FINE-NEXT:   "kind": "began",
// CHECK-FINE-NEXT:   "name": "compile",
// CHECK-FINE-NEXT:   "command": "{{.*[\\/]}}swift{{c?(\.exe)?(\\")?}} -frontend -c {{.*[\\/]}}file-01.swift{{(\\")?}} {{.*[\\/]}}file-02.swift{{(\\")?}} {{.*[\\/]}}file-03.swift{{(\\")?}} -primary-file {{.*[\\/]}}main.swift{{(\\")?}} -emit-module-path {{.*[\\/]}}main-[[MODULEMAIN:[a-z0-9]+]].swiftmodule{{(\\")?}} -emit-module-doc-path {{.*[\\/]}}main-[[SWIFTDOCMAIN:[a-z0-9]+]].swiftdoc{{(\\")?}} {{.*}} -enable-fine-grained-dependencies {{.*}} -module-name main -o {{.*[\\/]}}main-[[OBJMAIN:[a-z0-9]+]].o{{(\\")?}}",
// CHECK-FINE-NEXT:   "command_executable": "{{.*[\\/]}}swift{{c?(\.exe)?}}",
// CHECK-FINE-NEXT:   "command_arguments": [
// CHECK-FINE-NEXT:     "-frontend",
// CHECK-FINE-NEXT:     "-c",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-01.swift",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-02.swift",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-03.swift",
// CHECK-FINE-NEXT:     "-primary-file",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}main.swift",
// CHECK-FINE-NEXT:     "-emit-module-path",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}main-[[MODULEMAIN:[a-z0-9]+]].swiftmodule",
// CHECK-FINE-NEXT:     "-emit-module-doc-path",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}main-[[SWIFTDOCMAIN:[a-z0-9]+]].swiftdoc",
// CHECK-FINE:          "-enable-fine-grained-dependencies",
// CHECK-FINE:          "-module-name",
// CHECK-FINE-NEXT:     "main",
// CHECK-FINE-NEXT:     "-o",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}main-[[OBJMAIN:[a-z0-9]+]].o"
// CHECK-FINE-NEXT:   ],
// CHECK-FINE-NEXT:   "inputs": [
// CHECK-FINE-NEXT:     "{{.*[\\/]}}main.swift"
// CHECK-FINE-NEXT:   ],
// CHECK-FINE-NEXT:   "outputs": [
// CHECK-FINE-NEXT:     {
// CHECK-FINE-NEXT:       "type": "object",
// CHECK-FINE-NEXT:       "path": "{{.*[\\/]}}main-[[OBJMAIN]].o"
// CHECK-FINE-NEXT:     },
// CHECK-FINE-NEXT:     {
// CHECK-FINE-NEXT:       "type": "swiftmodule",
// CHECK-FINE-NEXT:       "path": "{{.*[\\/]}}main-[[MODULEMAIN]].swiftmodule"
// CHECK-FINE-NEXT:     },
// CHECK-FINE-NEXT:     {
// CHECK-FINE-NEXT:       "type": "swiftdoc",
// CHECK-FINE-NEXT:       "path": "{{.*[\\/]}}main-[[SWIFTDOCMAIN]].swiftdoc"
// CHECK-FINE-NEXT:     },
// CHECK-FINE-NEXT:     {
// CHECK-FINE-NEXT:       "type": "swiftsourceinfo",
// CHECK-FINE-NEXT:       "path": "{{.*[\\/]}}main-[[MODULEMAIN]].swiftsourceinfo"
// CHECK-FINE-NEXT:     }
// CHECK-FINE-NEXT:   ],
// CHECK-FINE-NEXT:   "pid": -{{[1-9][0-9]*}},
// CHECK-FINE-NEXT:   "process": {
// CHECK-FINE-NEXT:     "real_pid": {{[1-9][0-9]*}}
// CHECK-FINE-NEXT:   }
// CHECK-FINE-NEXT: }
// CHECK-FINE-NEXT: {{[1-9][0-9]*}}
// CHECK-FINE-NEXT: {
// CHECK-FINE-NEXT:   "kind": "finished",
// CHECK-FINE-NEXT:   "name": "compile",
// CHECK-FINE-NEXT:   "pid": -{{[1-9][0-9]*}},
// CHECK-FINE-NEXT:   "output": "Output placeholder\n",
// CHECK-FINE-NEXT:   "process": {
// CHECK-FINE-NEXT:     "real_pid": {{[1-9][0-9]*}}
// CHECK-FINE-NEXT:   },
// CHECK-FINE-NEXT:   "exit-status": 0
// CHECK-FINE-NEXT: }
// CHECK-FINE-NEXT: {{[1-9][0-9]*}}
// CHECK-FINE-NEXT: {
// CHECK-FINE-NEXT:   "kind": "finished",
// CHECK-FINE-NEXT:   "name": "compile",
// CHECK-FINE-NEXT:   "pid": -{{[1-9][0-9]*}},
// CHECK-FINE-NEXT:   "output": "Output placeholder\n",
// CHECK-FINE-NEXT:   "process": {
// CHECK-FINE-NEXT:     "real_pid": {{[1-9][0-9]*}}
// CHECK-FINE-NEXT:   },
// CHECK-FINE-NEXT:   "exit-status": 0
// CHECK-FINE-NEXT: }
// CHECK-FINE-NEXT: {{[1-9][0-9]*}}
// CHECK-FINE-NEXT: {
// CHECK-FINE-NEXT:   "kind": "finished",
// CHECK-FINE-NEXT:   "name": "compile",
// CHECK-FINE-NEXT:   "pid": -{{[1-9][0-9]*}},
// CHECK-FINE-NEXT:   "output": "Output placeholder\n",
// CHECK-FINE-NEXT:   "process": {
// CHECK-FINE-NEXT:     "real_pid": {{[1-9][0-9]*}}
// CHECK-FINE-NEXT:   },
// CHECK-FINE-NEXT:   "exit-status": 0
// CHECK-FINE-NEXT: }
// CHECK-FINE-NEXT: {{[1-9][0-9]*}}
// CHECK-FINE-NEXT: {
// CHECK-FINE-NEXT:   "kind": "finished",
// CHECK-FINE-NEXT:   "name": "compile",
// CHECK-FINE-NEXT:   "pid": -{{[1-9][0-9]*}},
// CHECK-FINE-NEXT:   "output": "Output placeholder\n",
// CHECK-FINE-NEXT:   "process": {
// CHECK-FINE-NEXT:     "real_pid": {{[1-9][0-9]*}}
// CHECK-FINE-NEXT:   },
// CHECK-FINE-NEXT:   "exit-status": 0
// CHECK-FINE-NEXT: }
// CHECK-FINE-NEXT: {{[1-9][0-9]*}}
// CHECK-FINE-NEXT: {
// CHECK-FINE-NEXT:   "kind": "began",
// CHECK-FINE-NEXT:   "name": "merge-module",
// CHECK-FINE-NEXT:   "command": "{{.*[\\/]}}swift{{c?(\.exe)?(\\")?}} -frontend -merge-modules -emit-module {{.*[\\/]}}file-01-[[MODULE01]].swiftmodule{{(\\")?}} {{.*[\\/]}}file-02-[[MODULE02]].swiftmodule{{(\\")?}} {{.*[\\/]}}file-03-[[MODULE03]].swiftmodule{{(\\")?}} {{.*[\\/]}}main-[[MODULEMAIN]].swiftmodule{{(\\")?}} {{.*}} -emit-module-doc-path main.swiftdoc -emit-module-source-info-path main.swiftsourceinfo -module-name main -o main.swiftmodule",
// CHECK-FINE-NEXT:   "command_executable": "{{.*[\\/]}}swift{{c?(\.exe)?}}",
// CHECK-FINE-NEXT:   "command_arguments": [
// CHECK-FINE-NEXT:     "-frontend",
// CHECK-FINE-NEXT:     "-merge-modules",
// CHECK-FINE-NEXT:     "-emit-module",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-01-[[MODULE01]].swiftmodule",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-02-[[MODULE02]].swiftmodule",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-03-[[MODULE03]].swiftmodule",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}main-[[MODULEMAIN]].swiftmodule",
// CHECK-FINE:          "-emit-module-doc-path",
// CHECK-FINE-NEXT:     "main.swiftdoc",
// CHECK-FINE:          "-emit-module-source-info-path",
// CHECK-FINE-NEXT:     "main.swiftsourceinfo",
// CHECK-FINE-NEXT:     "-module-name",
// CHECK-FINE-NEXT:     "main",
// CHECK-FINE-NEXT:     "-o",
// CHECK-FINE-NEXT:     "main.swiftmodule"
// CHECK-FINE-NEXT:   ],
// CHECK-FINE-NEXT:   "inputs": [
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-01-[[OBJ01]].o",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-02-[[OBJ02]].o",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}file-03-[[OBJ03]].o",
// CHECK-FINE-NEXT:     "{{.*[\\/]}}main-[[OBJMAIN]].o"
// CHECK-FINE-NEXT:   ],
// CHECK-FINE-NEXT:   "outputs": [
// CHECK-FINE-NEXT:     {
// CHECK-FINE-NEXT:       "type": "swiftmodule",
// CHECK-FINE-NEXT:       "path": "main.swiftmodule"
// CHECK-FINE-NEXT:     },
// CHECK-FINE-NEXT:     {
// CHECK-FINE-NEXT:       "type": "swiftdoc",
// CHECK-FINE-NEXT:       "path": "main.swiftdoc"
// CHECK-FINE-NEXT:     },
// CHECK-FINE-NEXT:     {
// CHECK-FINE-NEXT:       "type": "swiftsourceinfo",
// CHECK-FINE-NEXT:       "path": "main.swiftsourceinfo"
// CHECK-FINE-NEXT:     }
// CHECK-FINE-NEXT:   ],
// CHECK-FINE-NEXT:   "pid": {{[1-9][0-9]*}},
// CHECK-FINE-NEXT:   "process": {
// CHECK-FINE-NEXT:     "real_pid": {{[1-9][0-9]*}}
// CHECK-FINE-NEXT:   }
// CHECK-FINE-NEXT: }
// CHECK-FINE-NEXT: {{[1-9][0-9]*}}
// CHECK-FINE-NEXT: {
// CHECK-FINE-NEXT:   "kind": "finished",
// CHECK-FINE-NEXT:   "name": "merge-module",
// CHECK-FINE-NEXT:   "pid": {{[1-9][0-9]*}},
// CHECK-FINE-NEXT:   "output": "Output placeholder\n",
// CHECK-FINE-NEXT:   "process": {
// CHECK-FINE-NEXT:     "real_pid": {{[1-9][0-9]*}}
// CHECK-FINE-NEXT:   },
// CHECK-FINE-NEXT:   "exit-status": 0
// CHECK-FINE-NEXT: }
