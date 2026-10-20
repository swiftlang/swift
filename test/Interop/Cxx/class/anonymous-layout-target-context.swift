// REQUIRES: OS=macosx
// REQUIRES: sdk_min_version_15

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: mkdir %t/low %t/high
// RUN: %target-swift-frontend -scan-dependencies -module-name Producer -target %target-cpu-apple-macosx13.0 -clang-target %target-cpu-apple-macosx13.0 -cxx-interoperability-mode=default -I %t -module-cache-path %t/cache -experimental-clang-codegen-dependency AnonymousHeader %t/producer.swift -o %t/low.json
// RUN: %target-swift-frontend -scan-dependencies -module-name Producer -target %target-cpu-apple-macosx13.0 -clang-target %target-cpu-apple-macosx15.0 -cxx-interoperability-mode=default -I %t -module-cache-path %t/cache -experimental-clang-codegen-dependency AnonymousHeader %t/producer.swift -o %t/high.json
// RUN: %{python} %t/contexts.py %t
// RUN: %target-swift-frontend @%t/low-pcm.rsp
// RUN: %target-swift-frontend @%t/high-pcm.rsp

// A record without a tag name still has a layout through its typedef.
// A producer using the deployment target remains valid in the same context.
// RUN: %target-swift-frontend -emit-module -experimental-clang-target-context %target-cpu-apple-macosx13.0 -module-name Producer -target %target-cpu-apple-macosx13.0 -clang-target %target-cpu-apple-macosx13.0 -cxx-interoperability-mode=default -I %t -module-cache-path %t/cache @%t/low-import.rsp %t/producer.swift -o %t/low/Producer.swiftmodule
// RUN: %target-swift-frontend -emit-ir -Onone -target %target-cpu-apple-macosx13.0 -clang-target %target-cpu-apple-macosx15.0 -cxx-interoperability-mode=default -I %t -module-cache-path %t/cache -swift-module-file=Producer=%t/low/Producer.swiftmodule @%t/consumer.rsp %t/consumer.swift -o %t/low.ll
// RUN: %FileCheck %s --check-prefix=LOW < %t/low.ll

// An SDK producer using the other layout must be diagnosed through the alias,
// even though the underlying Clang RecordDecl has no identifier.
// RUN: %target-swift-frontend -emit-module -experimental-clang-target-context %target-cpu-apple-macosx13.0 -module-name Producer -target %target-cpu-apple-macosx13.0 -clang-target %target-cpu-apple-macosx15.0 -cxx-interoperability-mode=default -I %t -module-cache-path %t/cache @%t/high-import.rsp %t/producer.swift -o %t/high/Producer.swiftmodule
// RUN: not %target-swift-frontend -emit-ir -target %target-cpu-apple-macosx13.0 -clang-target %target-cpu-apple-macosx15.0 -cxx-interoperability-mode=default -I %t -module-cache-path %t/cache -swift-module-file=Producer=%t/high/Producer.swiftmodule @%t/consumer.rsp %t/consumer.swift -o %t/high.ll 2>&1 | %FileCheck %s --check-prefix=HIGH --implicit-check-not="Please submit a bug report" --implicit-check-not="Program aborted"

// LOW-LABEL: define {{.*}}readAnonymous
// LOW: call swiftcc i32 {{.*}}produced
// HIGH: error: cannot map Clang AST for deployment-target code generation: type differs between SDK and deployment targets

//--- module.modulemap
module AnonymousHeader {
  requires cplusplus
  header "AnonymousHeader.h"
  export *
}

//--- AnonymousHeader.h
#pragma once
typedef struct {
  int value;
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 150000
  long long extra;
#endif
} AnonymousLayout;

inline AnonymousLayout makeAnonymous() { return {}; }

//--- producer.swift
import AnonymousHeader

@available(macOS 15.0, *)
public func producedAnonymous() -> AnonymousLayout {
  makeAnonymous()
}

//--- consumer.swift
import Producer

public func readAnonymous() -> Int32 {
  if #available(macOS 15.0, *) {
    return producedAnonymous().value
  }
  return 0
}

//--- contexts.py
import json
from pathlib import Path
import sys

root = Path(sys.argv[1])

def read_context(name):
    graph = json.loads((root / (name + ".json")).read_text())
    modules = list(zip(graph["modules"][::2], graph["modules"][1::2]))
    clang = next(info for module, info in modules if module == {"clang": "AnonymousHeader"})
    swift = next(info for module, info in modules if module == {"swift": graph["mainModuleName"]})
    details = swift["details"]["swift"]
    descriptor = details["clangTargetContext"]
    assert len(descriptor.split("|")) == 3 and descriptor.split("|")[2]
    assert details["clangContextArguments"][0] == "-cc1"
    return clang, descriptor, details["clangContextArguments"]

def response(name, arguments):
    quoted = ['"' + arg.replace("\\", "\\\\").replace('"', '\\"') + '"' for arg in arguments]
    (root / (name + ".rsp")).write_text("\n".join(quoted))

low, low_context, low_arguments = read_context("low")
high, sdk_context, sdk_arguments = read_context("high")
assert low_context != sdk_context
for name, module in [("low", low), ("high", high)]:
    Path(module["modulePath"]).parent.mkdir(parents=True, exist_ok=True)
    command = module["details"]["clang"]["commandLine"]
    response(name + "-pcm", [arg for arg in command if arg != "-frontend"])
    response(name + "-import", ["-Xcc", "-fmodule-file=AnonymousHeader=" + module["modulePath"]])
arguments = [
    "-Xcc", "-fmodule-file=AnonymousHeader=" + high["modulePath"],
    "-experimental-clang-codegen-module-file", "AnonymousHeader=" + low["modulePath"],
    "-experimental-clang-sdk-context", sdk_context,
    "-experimental-clang-producer-module-file", low_context + "=AnonymousHeader=" + low["modulePath"],
]
for context, saved_arguments in [(low_context, low_arguments), (sdk_context, sdk_arguments)]:
    for argument in saved_arguments:
        arguments += ["-experimental-clang-producer-context-argument", context + "=" + argument]
response("consumer", arguments)
