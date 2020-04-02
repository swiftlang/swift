#!/usr/bin/python
# RUN: ${python} %s %target-swiftmodule-name %platform-sdk-overlay-dir \
# RUN:     %target-sil-opt -sdk %sdk -enable-sil-verify-all \
# RUN:       -F %sdk/System/Library/PrivateFrameworks \
# RUN:       -F "%xcode-extra-frameworks-dir"

# REQUIRES: long_test
# REQUIRES: nonexecutable_test


from __future__ import print_function

import os
import subprocess
import sys

target_swiftmodule_name = sys.argv[1]
sdk_overlay_dir = sys.argv[2]
sil_opt_invocation = sys.argv[3:]

for module_file in os.listdir(sdk_overlay_dir):
    module_name, ext = os.path.splitext(module_file)
    if ext != ".swiftmodule":
        continue
    # Skip the standard library because it's tested elsewhere.
    if module_name == "Swift":
        continue
    # TODO(TF-1229): Fix the "_Differentiation" module.
    if module_name == "_Differentiation":
        continue
    print("# " + module_name)

    module_path = os.path.join(sdk_overlay_dir, module_file)
    if os.path.isdir(module_path):
        module_path = os.path.join(module_path, target_swiftmodule_name)

    # llvm-bcanalyzer | not grep Unknown
    bcanalyzer_output = subprocess.check_output(["llvm-bcanalyzer",
                                                 module_path])
    if "Unknown" in bcanalyzer_output:
        print(bcanalyzer_output)
        sys.exit(1)

    # sil-opt
    # We are deliberately discarding the output here; we're just making sure
    # it can be generated.
    subprocess.check_output(sil_opt_invocation +
                            [module_path, "-module-name", module_name])
