# -----------------------------------------------------------------------------
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2026 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# -----------------------------------------------------------------------------
#
# Inspect the PE import graph of a staged Windows toolchain, associate each
# executable with the tool package whose WiX authoring references it, and
# validate the feature groups in the private runtime MSI.
#
# Example:
#
#   py utils\windows-sxs-feature-plan.py
#
# For a toolchain not on PATH or a nonstandard source layout:
#
#   py utils\windows-sxs-feature-plan.py ^
#     --toolchain-root C:\Swift\Toolchains\0.0.0+Asserts ^
#     --runtime-dir C:\Swift\Runtimes\0.0.0\usr\bin
#
# -----------------------------------------------------------------------------

import argparse
import re
import shutil
import subprocess
import sys
import xml.etree.ElementTree as ET
from collections import defaultdict, deque
from dataclasses import dataclass
from fnmatch import fnmatchcase
from pathlib import Path
from typing import Dict, Iterable, List, Mapping, Optional, Sequence
from typing import Set, Tuple


# Data model


@dataclass(frozen=True)
class BinaryPlan:
    binary: str
    package: str
    runtime_roots: Tuple[str, ...]
    runtime_closure: Tuple[str, ...]


@dataclass(frozen=True)
class PackagePlan:
    package: str
    required: Tuple[str, ...]
    authored: Tuple[str, ...]
    missing: Tuple[str, ...]
    extra: Tuple[str, ...]


@dataclass(frozen=True)
class WixPackage:
    name: str
    executables: Dict[str, str]
    authored_runtime_assemblies: Set[str]


@dataclass(frozen=True)
class Plan:
    binaries: Tuple[BinaryPlan, ...]
    packages: Tuple[PackagePlan, ...]
    missing_referenced_tool_binaries: Tuple[str, ...]
    unmapped_binaries: Tuple[str, ...]
    unused_runtime_assemblies: Tuple[str, ...]


# WiX authoring

WIX_NAMESPACES = {"wix": "http://wixtoolset.org/schemas/v4/wxs"}
WIX_PACKAGE_XPATH = "./wix:Package"
WIX_SOURCE_FILES_XPATH = ".//wix:File[@Source]"
WIX_COMPONENT_GROUP_XPATH = ".//wix:ComponentGroup"
WIX_FEATURE_XPATH = ".//wix:Feature"
WIX_COMPONENT_GROUP_REF_XPATH = "./wix:ComponentGroupRef"
TOOLCHAIN_EXE_RE = re.compile(r"^\$\(ToolchainRoot\)[\\/]usr[\\/]bin[\\/]([^\\/]+\.exe)$",
                              re.IGNORECASE)
SXS_DLL_RE = re.compile(r"^\$\(ToolchainRoot\)[\\/]usr[\\/]bin[\\/]"
                        r"([^\\/]+)[\\/]([^\\/]+\.dll)$",
                        re.IGNORECASE)
DISABLED_WIX_BLOCK_RE = re.compile(r"<\?if\s+True\s*==\s*False\s*\?>.*?<\?endif\s*\?>",
                                   re.IGNORECASE | re.DOTALL)


def _read_package_executables(path: Path) -> Dict[str, str]:
    """Read installed executables from a tool MSI."""
    # Ignore authoring explicitly compiled out in every configuration.  Other
    # preprocessor conditions describe real variants and remain in the scan.
    text = path.read_text(encoding="utf-8-sig")
    root = ET.fromstring(DISABLED_WIX_BLOCK_RE.sub("", text))
    package = root.find(WIX_PACKAGE_XPATH, WIX_NAMESPACES)
    if package is None:
        return {}

    executables = {}
    for element in package.findall(WIX_SOURCE_FILES_XPATH, WIX_NAMESPACES):
        source = element.attrib["Source"]
        match = TOOLCHAIN_EXE_RE.match(source)
        if not match:
            continue
        source_name = match.group(1)
        install_name = element.get("Name", source_name)
        executables[install_name.casefold()] = install_name
    return executables


def _read_prt_features(path: Path) -> Dict[str, Set[str]]:
    text = path.read_text(encoding="utf-8-sig")
    root = ET.fromstring(DISABLED_WIX_BLOCK_RE.sub("", text))
    package = root.find(WIX_PACKAGE_XPATH, WIX_NAMESPACES)
    if package is None:
        raise RuntimeError("WiX Package not found in {!s}".format(path))

    component_groups = {}
    for group in package.findall(WIX_COMPONENT_GROUP_XPATH, WIX_NAMESPACES):
        assemblies = set()
        for element in group.findall(WIX_SOURCE_FILES_XPATH, WIX_NAMESPACES):
            match = SXS_DLL_RE.match(element.attrib["Source"])
            if match and Path(match.group(2)).stem.casefold() == match.group(1).casefold():
                assemblies.add(match.group(1))
        component_groups[group.attrib["Id"]] = assemblies

    features = {}
    for feature in package.findall(WIX_FEATURE_XPATH, WIX_NAMESPACES):
        assemblies = set()
        for reference in feature.findall(WIX_COMPONENT_GROUP_REF_XPATH, WIX_NAMESPACES):
            assemblies.update(component_groups.get(reference.attrib["Id"], ()))
        features[feature.attrib["Id"]] = assemblies
    return features


def _discover_packages(installer_root: Path) -> Tuple[WixPackage, ...]:
    discovered = {}
    paths = sorted(installer_root.glob("*/*.wxi"), key=lambda path: str(path).casefold())
    for path in paths:
        executables = _read_package_executables(path)
        if not executables:
            continue
        name = path.parent.name
        if name in discovered:
            message = "multiple WiX packages discovered for {}: {!s} and {!s}".format(name,
                                                                                      discovered[name][0],
                                                                                      path)
            raise RuntimeError(message)
        discovered[name] = (path, executables)

    prt_features = _read_prt_features(installer_root / "prt" / "prt.wxi")
    packages = []
    for name, (_, executables) in discovered.items():
        feature_name = _runtime_feature_name(name)
        if feature_name not in prt_features:
            raise RuntimeError("PRT feature {} not found".format(feature_name))
        packages.append(WixPackage(name=name,
                                   executables=executables,
                                   authored_runtime_assemblies=prt_features[feature_name]))
    return tuple(packages)


# PE import processing

DEFAULT_EXCLUDED_RUNTIME_DLLS = ("concrt140",
                                 "msvcp140*",
                                 "testing",
                                 "_testing_foundation",
                                 "_testing_winsdk",
                                 "_testinginterop",
                                 "vccorlib140",
                                 "vcruntime140*",
                                 "xctest")
IMPORT_RE = re.compile(r"^\s*Name:\s*(\S+)\s*$")


def _casefold_map(paths: Iterable[Path]) -> Dict[str, Path]:
    result = {}
    for path in paths:
        key = path.name.casefold()
        if key in result:
            message = "case-insensitive filename collision: {!s} and {!s}".format(result[key],
                                                                                  path)
            raise RuntimeError(message)
        result[key] = path
    return result


class ImportReader:
    def __init__(self, llvm_readobj: Path) -> None:
        if not llvm_readobj.is_file():
            message = "toolchain llvm-readobj not found: {!s}".format(llvm_readobj)
            raise RuntimeError(message)
        self.llvm_readobj = llvm_readobj
        self.cache = {}  # type: Dict[Path, Tuple[str, ...]]

    def imports(self, path: Path) -> Tuple[str, ...]:
        resolved = path.resolve()
        if resolved in self.cache:
            return self.cache[resolved]

        process = subprocess.run([str(self.llvm_readobj), "--coff-imports", str(path)],
                                 stdout=subprocess.PIPE,
                                 stderr=subprocess.PIPE,
                                 universal_newlines=True,
                                 check=False)
        if process.returncode:
            detail = process.stderr.strip() or "exit {}".format(process.returncode)
            raise RuntimeError("llvm-readobj failed for {!s}: {}".format(path, detail))

        imports = sorted({
                             match.group(1)
                             for line in process.stdout.splitlines()
                             for match in [IMPORT_RE.match(line)]
                             if match
                         },
                         key=str.casefold)
        self.cache[resolved] = tuple(imports)
        return self.cache[resolved]


def _direct_runtime_imports(path: Path,
                            reader: ImportReader,
                            runtime_names: Mapping[str, str]) -> Set[str]:
    result = set()
    for imported in reader.imports(path):
        name = Path(imported).stem
        canonical = runtime_names.get(name.casefold())
        if canonical:
            result.add(canonical)
    return result


def _runtime_roots(path: Path,
                   binary_files: Mapping[str, Path],
                   reader: ImportReader,
                   runtime_names: Mapping[str, str]) -> Set[str]:
    """Follow local non-runtime DLLs and return the runtime roots."""
    roots = set()
    visited = set()
    queue = deque([path])

    while queue:
        current = queue.popleft().resolve()
        if current in visited:
            continue
        visited.add(current)

        for imported in reader.imports(current):
            filename = Path(imported).name
            base = Path(filename).stem
            canonical = runtime_names.get(base.casefold())
            if canonical:
                roots.add(canonical)
                continue
            local_dll = binary_files.get(filename.casefold())
            if local_dll:
                queue.append(local_dll)

    return roots


def _runtime_closure(roots: Iterable[str],
                     runtime_graph: Mapping[str, Set[str]]) -> Set[str]:
    result = set()
    queue = deque(roots)
    while queue:
        current = queue.popleft()
        if current in result:
            continue
        result.add(current)
        queue.extend(runtime_graph.get(current, ()))
    return result


# Runtime feature planning

def _runtime_feature_name(package: str) -> str:
    return "SxSSwiftRuntime{}".format(package.upper())


def make_plan(toolchain_root: Path,
              runtime_dir: Path,
              packages: Tuple[WixPackage, ...],
              excluded_runtime_dlls: Iterable[str]) -> Plan:
    binary_dir = toolchain_root / "usr" / "bin"
    if not binary_dir.is_dir():
        raise RuntimeError("toolchain binary directory not found: {!s}".format(binary_dir))
    if not runtime_dir.is_dir():
        raise RuntimeError("runtime directory not found: {!s}".format(runtime_dir))

    package_executables = {}  # type: Dict[str, str]
    for package in packages:
        for folded_name, install_name in package.executables.items():
            previous = package_executables.get(folded_name)
            if previous and previous != package.name:
                message = "{} is referenced by both {} and {}".format(install_name,
                                                                      previous,
                                                                      package.name)
                raise RuntimeError(message)
            package_executables[folded_name] = package.name

    excluded_runtime_patterns = tuple(Path(name).stem.casefold()
                                      for name in excluded_runtime_dlls)
    runtime_paths = []
    for path in runtime_dir.glob("*.dll"):
        if any(fnmatchcase(path.stem.casefold(), pattern)
               for pattern in excluded_runtime_patterns):
            continue
        runtime_paths.append(path)
    if not runtime_paths:
        raise RuntimeError("no runtime DLLs found in {!s}".format(runtime_dir))

    runtime_files = _casefold_map(runtime_paths)
    runtime_names = {key.rsplit(".dll", 1)[0]: path.stem for key, path in runtime_files.items()}
    binary_files = _casefold_map(path for path in binary_dir.glob("*.dll"))
    installed_executables = _casefold_map(path for path in binary_dir.glob("*.exe"))
    reader = ImportReader(binary_dir / "llvm-readobj.exe")

    runtime_graph = {}
    for path in runtime_paths:
        runtime_graph[path.stem] = _direct_runtime_imports(path, reader, runtime_names)

    binary_plans = []
    missing_referenced_tools = []
    package_required = defaultdict(set)  # type: Dict[str, Set[str]]
    for folded_name, package in sorted(package_executables.items()):
        path = installed_executables.get(folded_name)
        if not path:
            missing_referenced_tools.append(folded_name)
            continue
        roots = _runtime_roots(path, binary_files, reader, runtime_names)
        closure = _runtime_closure(roots, runtime_graph)
        package_required[package].update(closure)
        binary_plans.append(BinaryPlan(binary=path.name,
                                       package=package,
                                       runtime_roots=tuple(sorted(roots,
                                                                  key=str.casefold)),
                                       runtime_closure=tuple(sorted(closure,
                                                                    key=str.casefold))))

    unmapped = []
    for folded_name, path in sorted(installed_executables.items()):
        if folded_name in package_executables:
            continue
        roots = _runtime_roots(path, binary_files, reader, runtime_names)
        if roots:
            unmapped.append(path.name)

    package_plans = []
    used_runtime_assemblies = set()
    for package in packages:
        required = package_required[package.name]
        authored = package.authored_runtime_assemblies
        used_runtime_assemblies.update(required)
        package_plans.append(PackagePlan(package=package.name,
                                         required=tuple(sorted(required,
                                                               key=str.casefold)),
                                         authored=tuple(sorted(authored,
                                                               key=str.casefold)),
                                         missing=tuple(sorted(required - authored,
                                                              key=str.casefold)),
                                         extra=tuple(sorted(authored - required,
                                                            key=str.casefold))))

    package_order = {package.name: index for index, package in enumerate(packages)}
    unused = sorted(set(runtime_names.values()) - used_runtime_assemblies,
                    key=str.casefold)

    return Plan(binaries=tuple(sorted(binary_plans,
                                      key=lambda plan: (package_order[plan.package],
                                                        plan.binary.casefold()))),
                packages=tuple(package_plans),
                missing_referenced_tool_binaries=tuple(sorted(missing_referenced_tools)),
                unmapped_binaries=tuple(sorted(unmapped, key=str.casefold)),
                unused_runtime_assemblies=tuple(unused))


# Report emission

def _append_list(lines: List[str], indentation: str, name: str,
                 values: Iterable[str]) -> None:
    values = tuple(values)
    if not values:
        lines.append("{}{}: []".format(indentation, name))
        return
    lines.append("{}{}:".format(indentation, name))
    for value in values:
        lines.append("{}  - {}".format(indentation, value))


def emit_report(plan: Plan) -> str:
    lines = []
    lines.append("binaries:")
    current_package = None
    for binary in plan.binaries:
        if not binary.runtime_closure:
            continue
        if binary.package != current_package:
            current_package = binary.package
            lines.append("  {}:".format(current_package))
        lines.append("    {}:".format(binary.binary))
        _append_list(lines, "      ", "runtime-roots", binary.runtime_roots)
        _append_list(lines, "      ", "runtime-closure", binary.runtime_closure)

    lines.append("")
    lines.append("features:")
    for package in plan.packages:
        lines.append("  {}:".format(_runtime_feature_name(package.package)))
        lines.append("    package: {}".format(package.package))
        _append_list(lines, "    ", "required", package.required)
        _append_list(lines, "    ", "authored", package.authored)
        _append_list(lines, "    ", "missing", package.missing)
        _append_list(lines, "    ", "extra", package.extra)

    lines.append("")
    lines.append("diagnostics:")
    _append_list(lines, "  ", "missing-referenced-tool-binaries",
                 plan.missing_referenced_tool_binaries)
    _append_list(lines, "  ", "unmapped-runtime-dependent-binaries",
                 plan.unmapped_binaries)
    _append_list(lines, "  ", "unused-runtime-assemblies",
                 plan.unused_runtime_assemblies)
    return "\n".join(lines) + "\n"


# Input discovery

def _is_toolchain_root(path: Path) -> bool:
    return (path / "usr" / "bin" / "llvm-readobj.exe").is_file()


def _find_toolchain_root(explicit: Optional[Path]) -> Path:
    candidates = []
    if explicit:
        candidates.append(explicit)
    swift = shutil.which("swift.exe") or shutil.which("swift")
    if swift:
        swift_path = Path(swift).resolve()
        if (swift_path.parent.name.casefold() == "bin"
                and swift_path.parent.parent.name.casefold() == "usr"):
            candidates.append(swift_path.parent.parent.parent)

    for candidate in candidates:
        candidate = candidate.expanduser().resolve()
        if _is_toolchain_root(candidate):
            return candidate
    if explicit:
        raise RuntimeError("not a Swift toolchain root: {!s}".format(explicit))
    raise RuntimeError("unable to locate a Swift toolchain; pass "
                       "--toolchain-root or put its usr\\bin directory on PATH")


def _find_runtime_dir(explicit: Optional[Path], toolchain_root: Path) -> Path:
    candidates = []
    if explicit:
        candidates.append(explicit)

    # Installed layout:
    #   <root>/Toolchains/<version>+<variant>
    #   <root>/Runtimes/<version>/usr/bin
    if toolchain_root.parent.name.casefold() == "toolchains":
        install_root = toolchain_root.parent.parent
        version = toolchain_root.name.split("+", 1)[0]
        candidates.append(install_root / "Runtimes" / version / "usr" / "bin")
        runtimes_root = install_root / "Runtimes"
        if runtimes_root.is_dir():
            candidates.extend(path / "usr" / "bin"
                              for path in sorted(runtimes_root.iterdir())
                              if path.is_dir())

    for candidate in candidates:
        candidate = candidate.expanduser().resolve()
        if candidate.is_dir() and any(candidate.glob("*.dll")):
            return candidate
    if explicit:
        raise RuntimeError("runtime DLL directory not found: {!s}".format(explicit))
    raise RuntimeError("unable to locate the flat runtime for {!s}; pass "
                       "--runtime-dir".format(toolchain_root))


def _find_installer_root() -> Path:
    script_checkout = Path(__file__).resolve().parent.parent
    installer_root = (script_checkout.parent / "swift-installer-scripts" /
                      "platforms" / "Windows")
    if not (installer_root / "prt" / "prt.wxi").is_file():
        raise RuntimeError("adjacent swift-installer-scripts checkout not found: "
                           "{!s}".format(installer_root))
    return installer_root


# Command-line interface

def main(argv: Sequence[str]) -> int:
    parser = argparse.ArgumentParser(description=("Read executables referenced by the tool "
                                                  "installer authoring; calculate their runtime "
                                                  "dependencies; and validate the private "
                                                  "runtime MSI features."))
    parser.add_argument("--toolchain-root",
                        type=Path,
                        help="toolchain to inspect (default: infer from swift on PATH)")
    parser.add_argument("--runtime-dir",
                        type=Path,
                        help="flat runtime DLL directory (default: infer from installed toolchain)")
    parser.add_argument("--exclude-runtime-dll",
                        action="append",
                        default=list(DEFAULT_EXCLUDED_RUNTIME_DLLS),
                        metavar="PATTERN",
                        help=("add a runtime DLL name or pattern to the default exclusions; "
                              "repeat as needed"))
    args = parser.parse_args(argv)

    try:
        toolchain_root = _find_toolchain_root(args.toolchain_root)
        runtime_dir = _find_runtime_dir(args.runtime_dir, toolchain_root)
        installer_root = _find_installer_root()
        packages = _discover_packages(installer_root)
        if not packages:
            message = "no tool MSI packages were discovered below {!s}".format(installer_root)
            raise RuntimeError(message)
        plan = make_plan(toolchain_root=toolchain_root,
                         runtime_dir=runtime_dir,
                         packages=packages,
                         excluded_runtime_dlls=args.exclude_runtime_dll)
        sys.stdout.write(emit_report(plan))
        return 0
    except (OSError, RuntimeError, ValueError, ET.ParseError) as error:
        print("error: {}".format(error), file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
