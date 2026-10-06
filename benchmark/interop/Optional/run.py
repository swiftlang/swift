#!/usr/bin/env python3
# This source file is part of the Swift.org open source project
# Copyright (c) 2026 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

"""Compare swift::Optional performance with two Swift compiler builds."""

import argparse
import json
import os
from pathlib import Path
import platform
import statistics
import subprocess


def positive_int(value):
    number = int(value)
    if number <= 0:
        raise argparse.ArgumentTypeError("must be positive")
    return number


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--before", required=True, type=Path, help="baseline swiftc")
    parser.add_argument("--after", required=True, type=Path, help="candidate swiftc")
    parser.add_argument("--clangxx", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--sdk", help="SDK path; defaults to xcrun's SDK on macOS")
    parser.add_argument("--target", help="Swift and Clang target triple")
    parser.add_argument("--runtime-library-path", action="append", default=[],
                        help="use the same runtime for both builds (repeatable)")
    parser.add_argument("--iterations", type=positive_int, default=200000)
    parser.add_argument("--repetitions", type=positive_int, default=9)
    modes = parser.add_mutually_exclusive_group()
    modes.add_argument("--build-only", action="store_true")
    modes.add_argument("--skip-build", action="store_true")
    args = parser.parse_args()
    source = Path(__file__).resolve().parent
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=True)
    commands = []

    def run(command, **kwargs):
        command = [str(arg) for arg in command]
        commands.append(command)
        return subprocess.run(command, check=True, text=True,
                              capture_output=True, **kwargs)

    sdk = args.sdk
    if not sdk and platform.system() == "Darwin":
        sdk = run(["xcrun", "--show-sdk-path"]).stdout.strip()
    swift_flags = ["-cxx-interoperability-mode=default", "-module-name", "OptionalBench"]
    if sdk:
        swift_flags += ["-sdk", sdk]
    if args.target:
        swift_flags += ["-target", args.target]
    metadata = {"platform": platform.platform(), "arguments": vars(args).copy(),
                "clang_version": run([args.clangxx, "--version"]).stdout}
    environments = {}
    for label in ("before", "after"):
        swiftc = getattr(args, label).absolute()
        frontend = swiftc.with_name("swift-frontend")
        # Build trees can symlink swiftc to an external driver. Select the
        # frontend and resources beside the supplied path, not the symlink target.
        compiler = [swiftc, "-driver-use-frontend-path", frontend,
                    "-resource-dir", swiftc.parent.parent / "lib" / "swift"]
        directory = output / label
        directory.mkdir(exist_ok=True)
        target = json.loads(run(compiler + ["-print-target-info"] + swift_flags).stdout)
        metadata[label] = {"version": run([frontend, "--version"]).stdout,
                           "target_info": target}
        resource = target["paths"]["runtimeResourcePath"]
        paths = args.runtime_library_path or target["paths"]["runtimeLibraryPaths"]
        variable = "DYLD_LIBRARY_PATH" if platform.system() == "Darwin" else "LD_LIBRARY_PATH"
        environment = os.environ.copy()
        inherited = environment.get(variable)
        environment[variable] = os.pathsep.join(paths + ([inherited] if inherited else []))
        environments[label] = environment
        if args.skip_build:
            continue
        run(compiler + ["-typecheck", source / "OptionalBench.swift"] + swift_flags +
            ["-emit-clang-header-path", directory / "OptionalBench.h"])
        for mode in ("timing", "allocations"):
            cxx_flags = ["-O3", "-DNDEBUG", "-std=c++17", "-fno-exceptions",
                         "-target", target["target"]["triple"], "-I", resource,
                         "-I", directory]
            if sdk:
                cxx_flags += ["-isysroot", sdk]
            if mode == "allocations":
                cxx_flags += ["-DCOUNT_ALLOCATIONS"]
            obj = directory / (mode + ".o")
            run([args.clangxx] + cxx_flags +
                ["-c", source / "OptionalBench.cpp", "-o", obj])
            run(compiler + ["-O", source / "OptionalBench.swift"] + swift_flags +
                ["-Xfrontend", "-entry-point-function-name", "-Xfrontend", "swiftMain",
                 "-Xlinker", obj, "-o", directory / mode])
    metadata["commands"] = commands
    if not args.skip_build:
        (output / "build.json").write_text(json.dumps(metadata, indent=2, default=str) + "\n")
    if args.build_only:
        return

    results = []

    def measure(label, mode, repetition):
        process = run([output / label / mode, args.iterations], env=environments[label])
        rows = dict((name, float(value)) for name, value in
                    (line.split(",") for line in process.stdout.splitlines()))
        results.append({"variant": label, "mode": mode, "repetition": repetition,
                        "rows": rows, "layout": process.stderr})
        return rows

    allocations = {label: measure(label, "allocations", 0) for label in ("before", "after")}
    for repetition in range(args.repetitions):
        labels = ("before", "after") if repetition % 2 == 0 else ("after", "before")
        for label in labels:
            measure(label, "timing", repetition)
    (output / "run.json").write_text(json.dumps(metadata, indent=2, default=str) + "\n")
    (output / "results.json").write_text(json.dumps(results, indent=2) + "\n")
    summary = []
    print("benchmark,before_ns,after_ns,speedup,before_wrapper_allocs,after_wrapper_allocs")
    for name in allocations["before"]:
        medians = {}
        ranges = {}
        for label in ("before", "after"):
            samples = [row["rows"][name] for row in results
                       if row["variant"] == label and row["mode"] == "timing"]
            medians[label] = statistics.median(samples)
            ranges[label] = [min(samples), max(samples)]
        speedup = medians["before"] / medians["after"]
        summary.append({"name": name, "before_ns": medians["before"],
                        "after_ns": medians["after"], "speedup": speedup,
                        "process_median_ranges_ns": ranges,
                        "before_wrapper_allocs": allocations["before"][name],
                        "after_wrapper_allocs": allocations["after"][name]})
        print(f"{name},{medians['before']:.3f},{medians['after']:.3f},{speedup:.3f},"
              f"{allocations['before'][name]:.3f},{allocations['after'][name]:.3f}")
    (output / "summary.json").write_text(json.dumps(summary, indent=2) + "\n")


if __name__ == "__main__":
    try:
        main()
    except subprocess.CalledProcessError as error:
        print(error.stdout, end="")
        print(error.stderr, end="")
        raise
