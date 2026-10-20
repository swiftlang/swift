// REQUIRES: OS=macosx
// REQUIRES: sdk_min_version_15

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -scan-dependencies -module-name Producer -target %target-cpu-apple-macosx13.0 -clang-target %target-cpu-apple-macosx13.0 -cxx-interoperability-mode=default -I %t -module-cache-path %t/cache -experimental-clang-codegen-dependency VirtualHeader %t/producer.swift -o %t/low.json
// RUN: %target-swift-frontend -scan-dependencies -module-name Producer -target %target-cpu-apple-macosx13.0 -clang-target %target-cpu-apple-macosx15.0 -cxx-interoperability-mode=default -I %t -module-cache-path %t/cache -experimental-clang-codegen-dependency VirtualHeader %t/producer.swift -o %t/high.json
// RUN: %{python} %t/contexts.py %t
// RUN: %target-swift-frontend @%t/low-pcm.rsp
// RUN: %target-swift-frontend @%t/high-pcm.rsp
// The Swift producer and consumer both target 13. The producer imports the
// Clang 15 definition, so the consumer must validate that context explicitly.
// RUN: %target-swift-frontend -emit-module -experimental-clang-target-context %target-cpu-apple-macosx13.0 -module-name Producer -target %target-cpu-apple-macosx13.0 -clang-target %target-cpu-apple-macosx15.0 -cxx-interoperability-mode=default -I %t -module-cache-path %t/cache @%t/high-import.rsp %t/producer.swift -o %t/Producer.swiftmodule
// RUN: %target-swift-frontend -emit-ir -Onone -target %target-cpu-apple-macosx13.0 -clang-target %target-cpu-apple-macosx15.0 -cxx-interoperability-mode=default -I %t -module-cache-path %t/cache -swift-module-file=Producer=%t/Producer.swiftmodule @%t/consumer.rsp %t/stable.swift -o %t/stable.ll
// RUN: %FileCheck %s --check-prefix=STABLE < %t/stable.ll
// A shifted virtual slot or changed final annotation is incompatible even
// when the record size and alignment are unchanged.
// RUN: not %target-swift-frontend -emit-ir -Onone -target %target-cpu-apple-macosx13.0 -clang-target %target-cpu-apple-macosx15.0 -cxx-interoperability-mode=default -I %t -module-cache-path %t/cache -swift-module-file=Producer=%t/Producer.swiftmodule @%t/consumer.rsp %t/changed.swift -o %t/changed.ll 2>&1 | %FileCheck %s --check-prefix=CHANGED --implicit-check-not="Please submit a bug report" --implicit-check-not="Program aborted"
// RUN: not %target-swift-frontend -emit-ir -Onone -target %target-cpu-apple-macosx13.0 -clang-target %target-cpu-apple-macosx15.0 -cxx-interoperability-mode=default -I %t -module-cache-path %t/cache -swift-module-file=Producer=%t/Producer.swiftmodule @%t/consumer.rsp %t/derived.swift -o %t/derived.ll 2>&1 | %FileCheck %s --check-prefix=CHANGED --implicit-check-not="Please submit a bug report" --implicit-check-not="Program aborted"
// RUN: not %target-swift-frontend -emit-ir -Onone -target %target-cpu-apple-macosx13.0 -clang-target %target-cpu-apple-macosx15.0 -cxx-interoperability-mode=default -I %t -module-cache-path %t/cache -swift-module-file=Producer=%t/Producer.swiftmodule @%t/consumer.rsp %t/final.swift -o %t/final.ll 2>&1 | %FileCheck %s --check-prefix=FINAL --implicit-check-not="Please submit a bug report" --implicit-check-not="Program aborted"

// STABLE: call swiftcc void {{.*}}makeStable
// CHANGED: C++ virtual methods differ between SDK and deployment targets
// FINAL: C++ record ABI differs between SDK and deployment targets

//--- module.modulemap
module VirtualHeader {
  requires cplusplus
  header "VirtualHeader.h"
  export *
}

//--- VirtualHeader.h
#pragma once
struct VirtualBox {
  VirtualBox() = default;
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 150000
  virtual int newMethod() const;
#endif
  virtual int existingMethod() const;
  virtual ~VirtualBox();
};
struct DerivedVirtualBox : VirtualBox {};
struct StableVirtualBox {
  StableVirtualBox() = default;
  virtual int existingMethod() const;
  virtual ~StableVirtualBox();
};
struct FinalVirtualBox
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ < 150000
final
#endif
{
  virtual int existingMethod() const;
  virtual ~FinalVirtualBox();
};
FinalVirtualBox *getFinalBox();
inline int invokeExisting(const VirtualBox &box) { return box.existingMethod(); }
inline int invokeStable(const StableVirtualBox &box) { return box.existingMethod(); }

//--- producer.swift
import VirtualHeader
public func makeBox() -> VirtualBox { VirtualBox() }
public func makeDerived() -> DerivedVirtualBox { DerivedVirtualBox() }
public func makeStable() -> StableVirtualBox { StableVirtualBox() }
public func makeFinal() -> UnsafeMutablePointer<FinalVirtualBox> { getFinalBox()! }

//--- stable.swift
import Producer
import VirtualHeader
public func callStable() -> Int32 { invokeStable(makeStable()) }

//--- changed.swift
import Producer
import VirtualHeader
public func callExisting() -> Int32 { invokeExisting(makeBox()) }

//--- derived.swift
import Producer
import VirtualHeader
public func callDerived() { _ = makeDerived() }

//--- final.swift
import Producer
import VirtualHeader
public func callFinal() { _ = makeFinal() }

//--- contexts.py
import json
from pathlib import Path
import sys

root = Path(sys.argv[1])

def read_context(name):
    graph = json.loads((root / (name + ".json")).read_text())
    modules = list(zip(graph["modules"][::2], graph["modules"][1::2]))
    clang = next(info for module, info in modules if module == {"clang": "VirtualHeader"})
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
    response(name + "-import", ["-Xcc", "-fmodule-file=VirtualHeader=" + module["modulePath"]])
arguments = [
    "-Xcc", "-fmodule-file=VirtualHeader=" + high["modulePath"],
    "-experimental-clang-codegen-module-file", "VirtualHeader=" + low["modulePath"],
    "-experimental-clang-sdk-context", sdk_context,
    "-experimental-clang-producer-module-file", low_context + "=VirtualHeader=" + low["modulePath"],
]
for context, saved_arguments in [(low_context, low_arguments), (sdk_context, sdk_arguments)]:
    for argument in saved_arguments:
        arguments += ["-experimental-clang-producer-context-argument", context + "=" + argument]
response("consumer", arguments)
