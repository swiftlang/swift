// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: mkdir -p %t/Context.swiftmodule %t/clang-search
// RUN: mv %t/Context.swiftinterface %t/Context.swiftmodule/arm64-apple-macos.swiftinterface
// RUN: %target-swift-frontend -emit-module -parse-stdlib %t/Empty.swift -module-name Context -target arm64-apple-macosx14.0 -o %t/Context.swiftmodule/arm64-apple-macos.swiftmodule
// RUN: %target-swift-frontend -scan-dependencies -parse-stdlib %t/Main.swift -I %t -target arm64-apple-macosx13.0 -clang-target arm64-apple-macosx15.0 -module-load-mode prefer-serialized -no-scanner-module-validation -o %t/ordinary.json
// RUN: %target-swift-frontend -scan-dependencies -parse-stdlib %t/Main.swift -I %t -Xcc -I%t/clang-search -Xcc -DCONTEXT=1 -target arm64-apple-macosx13.0 -target-cpu apple-m1 -clang-target arm64-apple-macosx15.0 -module-load-mode prefer-serialized -no-scanner-module-validation -experimental-clang-target-context arm64-apple-macosx13.0 -o %t/context.json
// RUN: %target-swift-frontend -scan-dependencies -parse-stdlib %t/Main.swift -I %t -Xcc -I%t/clang-search -Xcc -DCONTEXT=1 -target arm64-apple-macosx13.0 -target-cpu apple-m2 -clang-target arm64-apple-macosx15.0 -module-load-mode prefer-serialized -no-scanner-module-validation -experimental-clang-target-context arm64-apple-macosx13.0 -o %t/other-cpu.json
// RUN: %target-swift-frontend -scan-dependencies -parse-stdlib %t/Main.swift -I %t -Xcc -I%t/clang-search -Xcc -DCONTEXT=1 -target arm64-apple-macosx13.0 -target-cpu apple-m1 -clang-target arm64-apple-macosx16.0 -module-load-mode prefer-serialized -no-scanner-module-validation -experimental-clang-target-context arm64-apple-macosx13.0 -o %t/other-sdk.json
// RUN: %{python} %t/check.py %t/ordinary.json %t/context.json %t/other-sdk.json %t/other-cpu.json
// RUN: not %target-swift-frontend -scan-dependencies %t/Main.swift -experimental-clang-context-argument -DINVALID=1 2>&1 | %FileCheck %s --check-prefix=INVALID
// RUN: not %target-swift-frontend -typecheck %t/Main.swift -experimental-clang-codegen-dependency Context -experimental-clang-context-argument -DINVALID=1 2>&1 | %FileCheck %s --check-prefix=INVALID
// INVALID: requires a named Clang dependency scan
// RUN: %target-swift-frontend -scan-dependencies -parse-stdlib %t/Empty.swift -I %t -cxx-interoperability-mode=default -target arm64-apple-macosx13.0 -clang-target arm64-apple-macosx15.0 -experimental-clang-target-context arm64-apple-macosx13.0 -experimental-clang-codegen-dependency Header -o %t/pcm13.json
// RUN: %target-swift-frontend -scan-dependencies -parse-stdlib %t/Empty.swift -I %t -cxx-interoperability-mode=default -target arm64-apple-macosx15.0 -clang-target arm64-apple-macosx15.0 -experimental-clang-target-context arm64-apple-macosx15.0 -experimental-clang-codegen-dependency Header -o %t/pcm15.json
// RUN: mkdir -p %t/other-search
// RUN: %target-swift-frontend -scan-dependencies -parse-stdlib %t/Empty.swift -I %t -Xcc -I%t/other-search -cxx-interoperability-mode=default -target arm64-apple-macosx15.0 -clang-target arm64-apple-macosx15.0 -experimental-clang-target-context arm64-apple-macosx15.0 -experimental-clang-codegen-dependency Header -o %t/other-search.json
// RUN: %target-swift-frontend -scan-dependencies -parse-stdlib %t/Empty.swift -I %t -cxx-interoperability-mode=default -swift-version 5 -target arm64-apple-macosx15.0 -experimental-clang-target-context arm64-apple-macosx15.0 -experimental-clang-codegen-dependency Header -o %t/language5.json
// RUN: %target-swift-frontend -scan-dependencies -parse-stdlib %t/Empty.swift -I %t -cxx-interoperability-mode=default -swift-version 6 -target arm64-apple-macosx15.0 -experimental-clang-target-context arm64-apple-macosx15.0 -experimental-clang-codegen-dependency Header -o %t/language6.json
// RUN: %{python} %t/check-pcm.py %t/pcm13.json %t/pcm15.json %t/language5.json %t/language6.json %t/other-search.json
// RUN: %target-swift-frontend -scan-dependencies -parse-stdlib %t/Empty.swift -I %t -cxx-interoperability-mode=default -O -g -target arm64-apple-macosx15.0 -experimental-clang-codegen-dependency Header -o %t/optimized.json
// RUN: %{python} %t/check-snapshot.py %t/pcm15.json %t/optimized.json %t/replay.rsp
// RUN: %target-swift-frontend -scan-dependencies -parse-stdlib %t/Empty.swift -I %t -cxx-interoperability-mode=default -Onone -target arm64-apple-macosx15.0 -experimental-clang-target-context arm64-apple-macosx15.0 -experimental-clang-codegen-dependency Header @%t/replay.rsp -o %t/replayed.json
// RUN: %{python} %t/check-replay.py %t/optimized.json %t/replayed.json
// RUN: mkdir -p %t/other-sdk
// RUN: %target-swift-frontend -scan-dependencies -parse-stdlib %t/Empty.swift -I %t -cxx-interoperability-mode=default -Onone -target arm64-apple-macosx15.0 -experimental-clang-target-context arm64-apple-macosx15.0 -experimental-clang-codegen-dependency Header @%t/replay.rsp -sdk "" -o %t/replayed-no-sdk.json
// RUN: %target-swift-frontend -scan-dependencies -parse-stdlib %t/Empty.swift -I %t -cxx-interoperability-mode=default -Onone -target arm64-apple-macosx15.0 -experimental-clang-target-context arm64-apple-macosx15.0 -experimental-clang-codegen-dependency Header @%t/replay.rsp -sdk %t/other-sdk -o %t/replayed-other-sdk.json
// RUN: %{python} %t/check-replay.py %t/replayed.json %t/replayed-no-sdk.json
// RUN: %{python} %t/check-replay.py %t/replayed.json %t/replayed-other-sdk.json
// RUN: %target-swift-frontend -scan-dependencies -parse-stdlib %t/Empty.swift -I %t -cxx-interoperability-mode=default -Osize -Xcc -Oz -g -target arm64-apple-macosx15.0 -experimental-clang-codegen-dependency Header -o %t/size-optimized.json
// RUN: %{python} %t/check-snapshot.py %t/pcm15.json %t/size-optimized.json %t/size-replay.rsp
// RUN: %target-swift-frontend -scan-dependencies -parse-stdlib %t/Empty.swift -I %t -cxx-interoperability-mode=default -Onone -target arm64-apple-macosx15.0 -experimental-clang-codegen-dependency Header @%t/size-replay.rsp -o %t/size-replayed.json
// RUN: %{python} %t/check-replay.py %t/size-optimized.json %t/size-replayed.json

//--- Main.swift
import Context

//--- Empty.swift
public func empty() {}

//--- Context.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name Context -target arm64-apple-macosx14.0 -enable-library-evolution
public func value() {}

//--- module.modulemap
module Header { requires cplusplus header "Header.h" export * }
module OptimizedSupport { header "OptimizedSupport.h" export * }
module SizeOptimizedSupport { header "SizeOptimizedSupport.h" export * }

//--- Header.h
#ifdef __OPTIMIZE__
#include "OptimizedSupport.h"
#endif
#ifdef __OPTIMIZE_SIZE__
#include "SizeOptimizedSupport.h"
#endif
struct HeaderValue { int value; };

//--- OptimizedSupport.h
int optimizedOnly(void);

//--- SizeOptimizedSupport.h
int sizeOptimizedOnly(void);

//--- check-pcm.py
import json
import sys

def header(path):
    graph = json.load(open(path))
    for name, info in zip(graph["modules"][::2], graph["modules"][1::2]):
        if name == {"clang": "Header"}:
            return info
    raise AssertionError("missing Header module")

first, second = [header(path) for path in sys.argv[1:3]]
assert first["modulePath"] == second["modulePath"]
assert first["details"]["clang"]["commandLine"] == second["details"]["clang"]["commandLine"]
command = first["details"]["clang"]["commandLine"]
assert command[command.index("-target") + 1] == "arm64-apple-macosx15.0.0"
assert command[command.index("-clang-target") + 1] == "arm64-apple-macosx15.0.0"

def native_context(path):
    graph = json.load(open(path))
    for name, info in zip(graph["modules"][::2], graph["modules"][1::2]):
        if name == {"swift": graph["mainModuleName"]}:
            details = info["details"]["swift"]
            descriptor = details["clangTargetContext"].split("|")
            assert len(descriptor) == 3 and descriptor[2]
            assert descriptor[:2] == ["arm64-apple-macosx15.0.0", ""]
            assert details["clangContextArguments"][0] == "-cc1"
            assert "-fmodules-strict-context-hash" in details["clangContextArguments"]
            return descriptor, details["clangContextArguments"]
    raise AssertionError("missing named scan invocation")

assert native_context(sys.argv[1])[0] == native_context(sys.argv[2])[0]
language5, language6 = [native_context(path) for path in sys.argv[3:5]]
assert language5[0][2] != language6[0][2]
assert "-fapinotes-swift-version=5" in language5[1]
assert "-fapinotes-swift-version=6" in language6[1]
assert header(sys.argv[3])["modulePath"] != header(sys.argv[4])["modulePath"]
assert native_context(sys.argv[2])[0] != native_context(sys.argv[5])[0]

//--- check-snapshot.py
import json
import sys

def main_details(graph):
    for name, info in zip(graph["modules"][::2], graph["modules"][1::2]):
        if name == {"swift": graph["mainModuleName"]}:
            return info["details"]["swift"]
    raise AssertionError("missing main module")

plain, optimized = [json.load(open(path)) for path in sys.argv[1:3]]
plain_details, optimized_details = map(main_details, (plain, optimized))
assert plain_details["clangTargetContext"] != optimized_details["clangTargetContext"]
arguments = optimized_details["clangContextArguments"]
assert arguments[0] == "-cc1"
assert "-fmodules-strict-context-hash" in arguments
assert "-O3" in arguments
assert "-Os" in arguments or "-Oz" in arguments
assert {"clang": "OptimizedSupport"} in optimized["modules"][::2]
assert {"clang": "OptimizedSupport"} not in plain["modules"][::2]
assert {"clang": "SizeOptimizedSupport"} in optimized["modules"][::2]
assert {"clang": "SizeOptimizedSupport"} not in plain["modules"][::2]
# The saved invocation includes Swift's resolved optimization/debug policy.
# Replaying it under -Onone must not replace that policy with the consumer's.
with open(sys.argv[3], "w") as response:
    response.write("-clang-target\n")
    response.write(optimized_details["clangTargetContext"].split("|")[0] + "\n")
    for argument in arguments:
        response.write("-experimental-clang-context-argument\n")
        quoted = argument.replace("\\", "\\\\").replace('"', '\\"')
        response.write('"' + quoted + '"\n')

//--- check-replay.py
import json
import sys

def modules(path):
    graph = json.load(open(path))
    return {
        tuple(name.items()): info
        for name, info in zip(graph["modules"][::2], graph["modules"][1::2])
    }

original, replay = map(modules, sys.argv[1:])
original_main = next(info for name, info in original.items() if name[0][0] == "swift")
replayed_main = next(info for name, info in replay.items() if name[0][0] == "swift")
assert original_main["details"]["swift"]["clangTargetContext"] == replayed_main["details"]["swift"]["clangTargetContext"]
for name, info in original.items():
    if name[0][0] == "clang":
        assert info["modulePath"] == replay[name]["modulePath"]
        assert info["details"]["clang"]["commandLine"] == replay[name]["details"]["clang"]["commandLine"]

//--- check.py
import json
import sys

def context(path):
    graph = json.load(open(path))
    for name, info in zip(graph["modules"][::2], graph["modules"][1::2]):
        if name == {"swift": "Context"}:
            return info
    raise AssertionError("missing textual Context module")

ordinary, corrected, other_sdk, other_cpu = [context(path) for path in sys.argv[1:]]
assert len({item["modulePath"] for item in (ordinary, corrected, other_sdk, other_cpu)}) == 4
assert ordinary["details"]["swift"]["compiledModuleCandidates"]
for item in (corrected, other_sdk, other_cpu):
    details = item["details"]["swift"]
    assert not details.get("compiledModuleCandidates")
    command = details["commandLine"]
    assert "-candidate-module-file" not in command
    descriptor = command.index("-experimental-clang-target-context")
    assert command[descriptor + 1] == "arm64-apple-macosx14.0"
    assert "-DCONTEXT=1" in command
    assert any(command[index - 1] == "-Xcc" and
               arg.startswith("-I") and arg.endswith("/clang-search")
               for index, arg in enumerate(command) if index > 0)
