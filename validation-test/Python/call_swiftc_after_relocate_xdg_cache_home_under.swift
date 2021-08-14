// REQUIRES: OS=linux-gnu

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t
// RUN: split-file %s %t
//
// RUN: PYTHONPATH=%utils %{python} %t/run_swiftc_with_relocated_xdg_cache_home.py %t/.cache %swiftc_driver_plain %t/hello.swift
// RUN: ls %t/.cache/clang/ModuleCache

//--- run_swiftc_with_relocated_xdg_cache_home.py
import sys
import subprocess
from swift_build_support.swift_build_support import workspace

workspace.relocate_xdg_cache_home_under(sys.argv[1])
subprocess.run([sys.argv[2], sys.argv[3]])

//--- hello.swift
print("hello")
