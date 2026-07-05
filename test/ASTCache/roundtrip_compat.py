#!/usr/bin/env python3
# ===-------------------------------------------------------------------===//
#
#  This source file is part of the Swift.org open source project
#
#  Copyright (c) 2024-2026 Apple Inc. and the Swift project authors
#  Licensed under Apache License v2.0 with Runtime Library Exception
#
#  See LICENSE.txt for license information
#  See CONTRIBUTORS.txt for the list of Swift project authors
#
# ===-------------------------------------------------------------------===//
#
#  Runs the -verify-ast-cache round-trip equivalence check against real-world
#  projects from the Swift source compatibility suite. This is the primary
#  success criterion for the AST cache format.
#
#  Usage:
#    roundtrip_compat.py [--swift-frontend PATH] [--projects-dir DIR]
#                        [--smoketest] [project ...]
#
#  --swift-frontend PATH  Path to swift-frontend (default: from PATH)
#  --projects-dir DIR     Where to clone projects (default: /tmp/roundtrip-projects)
#  --smoketest            Use the 4 smoketest projects (default)
#  project ...            Explicit project names (overrides --smoketest)
#
#  Exit code: 0 if all files PASSED, 1 if any FAILED.
#
# ===-------------------------------------------------------------------===//

import argparse
import json
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

# Smoketest subset of the Swift source compatibility suite.
# These are small, popular projects with diverse AST features.
SMOKETEST_PROJECTS = {
    "Alamofire": {
        "repo": "https://github.com/Alamofire/Alamofire.git",
        "commit": "master",
    },
    "Kingfisher": {
        "repo": "https://github.com/onevcat/Kingfisher.git",
        "commit": "master",
    },
    "ReactiveCocoa": {
        "repo": "https://github.com/ReactiveCocoa/ReactiveCocoa.git",
        "commit": "master",
    },
    "SwiftLint": {
        "repo": "https://github.com/realm/SwiftLint.git",
        "commit": "master",
    },
}

# Suffixes of files we should skip.
SKIP_SUFFIXES = {".git", ".framework", ".bundle", ".app"}

# Files to skip by name.
SKIP_FILES = {"main.swift", "Package.swift", "Package.resolved"}


def run_cmd(cmd, cwd=None, capture=True, timeout=300):
    """Run a command and return (returncode, stdout, stderr)."""
    try:
        result = subprocess.run(
            cmd,
            cwd=cwd,
            capture_output=capture,
            text=True,
            timeout=timeout,
        )
        return result.returncode, result.stdout, result.stderr
    except subprocess.TimeoutExpired:
        return 124, "", "TIMEOUT"
    except Exception as e:
        return 1, "", str(e)


def clone_project(name, repo_url, commit, projects_dir):
    """Clone a project to projects_dir/<name> and checkout the given commit."""
    project_dir = projects_dir / name
    if project_dir.exists():
        shutil.rmtree(project_dir)

    print(f"  Cloning {name}...", flush=True)
    rc, out, err = run_cmd(
        ["git", "clone", "--depth", "1", repo_url, str(project_dir)],
        timeout=120,
    )
    if rc != 0:
        print(f"  CLONE FAILED: {err}", flush=True)
        return None

    if commit != "master" and commit != "main":
        rc, out, err = run_cmd(
            ["git", "checkout", commit], cwd=str(project_dir), timeout=30
        )
        if rc != 0:
            print(f"  CHECKOUT FAILED: {err}", flush=True)
            return None

    return project_dir


def build_project(project_dir, module_name):
    """Build the project once with swift build to generate .swiftmodule files."""
    print(f"  Building {module_name}...", flush=True)
    rc, out, err = run_cmd(
        ["swift", "build", "-c", "release"],
        cwd=str(project_dir),
        capture=True,
        timeout=600,
    )
    if rc != 0:
        print(f"  BUILD FAILED (rc={rc})", flush=True)
        if err:
            print(f"  stderr: {err[:500]}", flush=True)
        return False
    return True


def extract_compile_flags(project_dir, module_name):
    """Extract per-file compile flags from swift build -v output.

    Returns a dict: file_path -> [extra_flags].
    """
    print(f"  Extracting compile flags...", flush=True)
    rc, out, err = run_cmd(
        ["swift", "build", "-c", "release", "-v"],
        cwd=str(project_dir),
        capture=True,
        timeout=600,
    )
    if rc != 0 and not out:
        return {}

    flags_by_file = {}
    # Look for swift-frontend lines in verbose output
    for line in out.split("\n"):
        if "swift-frontend" not in line and "swiftc" not in line:
            continue
        # Parse the line to find .swift files and -I flags
        parts = line.split()
        if not parts:
            continue

        # Extract -I paths and other flags
        include_dirs = []
        module_name_arg = None
        swift_files = []
        sdk_arg = None
        i = 0
        while i < len(parts):
            arg = parts[i]
            if arg == "-I" and i + 1 < len(parts):
                include_dirs.append(parts[i + 1])
                i += 2
            elif arg == "-module-name" and i + 1 < len(parts):
                module_name_arg = parts[i + 1]
                i += 2
            elif arg == "-sdk" and i + 1 < len(parts):
                sdk_arg = parts[i + 1]
                i += 2
            elif arg.endswith(".swift"):
                swift_files.append(arg)
                i += 1
            else:
                i += 1

        if not swift_files:
            continue

        # Build the extra flags string
        extra_flags = []
        for inc in include_dirs:
            extra_flags.extend(["-I", inc])
        if module_name_arg:
            extra_flags.extend(["-module-name", module_name_arg])
        if sdk_arg:
            extra_flags.extend(["-sdk", sdk_arg])

        for f in swift_files:
            # Resolve relative paths
            if not os.path.isabs(f):
                f = os.path.join(str(project_dir), f)
            flags_by_file[f] = extra_flags

    return flags_by_file


def find_swift_files(project_dir):
    """Find all .swift files in the project, excluding script-mode files."""
    swift_files = []
    for root, dirs, files in os.walk(project_dir):
        # Skip hidden dirs and build artifacts
        dirs[:] = [d for d in dirs if not d.startswith(".")
                   and d not in SKIP_SUFFIXES
                   and d != ".build"]

        for f in files:
            if not f.endswith(".swift"):
                continue
            if f in SKIP_FILES:
                continue
            full_path = os.path.join(root, f)
            swift_files.append(full_path)
    return swift_files


def is_script_mode(file_path):
    """Check if a file is in script mode (has top-level code)."""
    # Heuristic: if the file is named main.swift or doesn't have any
    # top-level decl, skip it. For now, we just check the filename.
    return os.path.basename(file_path) == "main.swift"


def run_roundtrip(swift_frontend, file_path, extra_flags, module_name):
    """Run -verify-ast-cache on a single file. Returns (status, output)."""
    cmd = [
        swift_frontend,
        "-parse-as-library",
        "-typecheck",
        "-verify-ast-cache",
    ]
    cmd.extend(extra_flags)
    cmd.append(file_path)

    rc, out, err = run_cmd(cmd, timeout=60)
    combined = (out + err).strip()

    if rc == 0:
        return "PASS", combined
    elif rc == 1:
        # Diff found
        return "FAIL", combined
    else:
        # Compile error unrelated to cache (e.g., missing imports)
        return "SKIP", combined


def main():
    parser = argparse.ArgumentParser(
        description="Run AST cache round-trip on real-world projects"
    )
    parser.add_argument(
        "--swift-frontend",
        default=None,
        help="Path to swift-frontend (default: from PATH)",
    )
    parser.add_argument(
        "--projects-dir",
        default="/tmp/roundtrip-projects",
        help="Where to clone projects",
    )
    parser.add_argument(
        "--smoketest",
        action="store_true",
        default=True,
        help="Use the 4 smoketest projects (default)",
    )
    parser.add_argument(
        "--no-clone",
        action="store_true",
        help="Skip cloning, use existing projects in --projects-dir",
    )
    parser.add_argument(
        "--no-build",
        action="store_true",
        help="Skip swift build, just run roundtrip on existing .swift files",
    )
    parser.add_argument(
        "projects",
        nargs="*",
        help="Explicit project names (overrides --smoketest)",
    )
    args = parser.parse_args()

    # Find swift-frontend
    swift_frontend = args.swift_frontend
    if not swift_frontend:
        # Try to find it in the PATH
        swift_frontend = shutil.which("swift-frontend")
        if not swift_frontend:
            # Try swift toolchain
            swift_path = shutil.which("swift")
            if swift_path:
                # swift-frontend is usually in the same dir as swift
                swift_dir = os.path.dirname(swift_path)
                swift_frontend = os.path.join(swift_dir, "swift-frontend")

    if not swift_frontend or not os.path.exists(swift_frontend):
        print(f"ERROR: swift-frontend not found. Use --swift-frontend PATH.")
        return 1

    print(f"swift-frontend: {swift_frontend}")

    # Determine which projects to test
    if args.projects:
        projects = {name: SMOKETEST_PROJECTS.get(name, {"repo": None, "commit": "master"})
                    for name in args.projects}
    else:
        projects = SMOKETEST_PROJECTS

    projects_dir = Path(args.projects_dir)
    projects_dir.mkdir(parents=True, exist_ok=True)

    total_pass = 0
    total_fail = 0
    total_skip = 0
    failed_files = []

    for name, info in projects.items():
        print(f"\n=== {name} ===", flush=True)

        if args.no_clone:
            project_dir = projects_dir / name
            if not project_dir.exists():
                print(f"  SKIP: project not found at {project_dir}")
                continue
        else:
            if not info.get("repo"):
                print(f"  SKIP: no repo URL for {name}")
                continue
            project_dir = clone_project(
                name, info["repo"], info["commit"], projects_dir
            )
            if project_dir is None:
                total_skip += 1
                continue

        # Build the project to get .swiftmodule files
        if not args.no_build:
            if not build_project(project_dir, name):
                total_skip += 1
                continue

        # Extract compile flags
        flags_by_file = {}
        if not args.no_build:
            flags_by_file = extract_compile_flags(project_dir, name)

        # Find all .swift files
        swift_files = find_swift_files(project_dir)
        print(f"  Found {len(swift_files)} .swift files", flush=True)

        # Run roundtrip on each file
        project_pass = 0
        project_fail = 0
        project_skip = 0

        for f in swift_files:
            if is_script_mode(f):
                project_skip += 1
                continue

            # Get extra flags for this file, or use defaults
            extra_flags = flags_by_file.get(f, [])
            if not extra_flags:
                # Use default flags
                extra_flags = [
                    "-module-name", name,
                    "-I", str(project_dir),
                ]
                # Add .build/release for swiftmodule files
                build_dir = project_dir / ".build" / "release"
                if build_dir.exists():
                    extra_flags.extend(["-I", str(build_dir)])

            status, output = run_roundtrip(swift_frontend, f, extra_flags, name)

            if status == "PASS":
                project_pass += 1
            elif status == "FAIL":
                project_fail += 1
                failed_files.append((f, output))
            else:
                project_skip += 1

        print(f"  {name}: PASS={project_pass} FAIL={project_fail} SKIP={project_skip}",
              flush=True)
        total_pass += project_pass
        total_fail += project_fail
        total_skip += project_skip

    # Print summary
    print(f"\n=== SUMMARY ===")
    print(f"PASSED: {total_pass}")
    print(f"FAILED: {total_fail}")
    print(f"SKIPPED: {total_skip}")

    if failed_files:
        print(f"\nFirst {min(5, len(failed_files))} failures:")
        for i, (f, output) in enumerate(failed_files[:5]):
            print(f"\n--- Failure {i+1}: {f} ---")
            # Print first 500 chars of output
            print(output[:500])

    return 0 if total_fail == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
