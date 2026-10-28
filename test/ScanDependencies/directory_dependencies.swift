// Test that Clang modules which enumerate a directory report it in
// `directoryDependencies`, and that other modules omit the field.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test \
// RUN:   -module-cache-path %t/module-cache -parse-stdlib \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/test.swift -I %t/include -o %t/deps.json
// RUN: %validate-json %t/deps.json &>/dev/null
// RUN: %FileCheck %s --check-prefix=UMB < %t/deps.json
// RUN: %FileCheck %s --check-prefix=PLAIN < %t/deps.json

// Every `commandLine` element ends in `",`, so the first `],` closes it.
// UMB-LABEL: "modulePath": "{{.*}}UmbDir-{{.*}}.pcm",
// UMB:       "commandLine": [
// UMB:       ],
// UMB-NEXT:  "directoryDependencies": [
// UMB-NEXT:    "{{.*}}include{{[/\\]+}}sub"
// UMB-NEXT:  ]

// Without any optional trailing fields, `commandLine` closes with a bare `]`.
// PLAIN-LABEL: "modulePath": "{{.*}}Plain-{{.*}}.pcm",
// PLAIN:       "commandLine": [
// PLAIN-NOT:   directoryDependencies
// PLAIN:       {{^ *}}]{{$}}
// PLAIN-NEXT:  {{^ *}}}{{$}}

//--- test.swift
import UmbDir
import Plain

//--- include/module.modulemap
module UmbDir {
  umbrella "sub"
  module * { export * }
}

module Plain {
  header "plain.h"
  export *
}

//--- include/sub/a.h
void a(void);

//--- include/plain.h
void p(void);
