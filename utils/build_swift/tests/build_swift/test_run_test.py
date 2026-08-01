# This source file is part of the Swift.org open source project
#
# Copyright (c) 2026 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

from swift_build_support.swift_build_support.targets import (
    StdlibDeploymentTarget,
)


_RUN_TEST = Path(__file__).resolve().parents[3] / "run-test"
_HOST_TARGET = StdlibDeploymentTarget.host_target().name


class RunTestTestCase(unittest.TestCase):
    def test_build_variant_flags_find_the_matching_build_directory(self):
        variants = {
            "-d": "Ninja-DebugAssert",
            "-r": "Ninja-RelWithDebInfoAssert",
            "-R": "Ninja-ReleaseAssert",
            "--min-size-release": "Ninja-MinSizeRelAssert",
        }

        for flag, build_subdir in variants.items():
            with self.subTest(flag=flag), tempfile.TemporaryDirectory() as root:
                root_path = Path(root)
                swift_build_dir = root_path / build_subdir / f"swift-{_HOST_TARGET}"
                swift_build_dir.mkdir(parents=True)
                (swift_build_dir / "CMakeCache.txt").touch()
                (swift_build_dir / f"test-{_HOST_TARGET}").mkdir()
                fake_lit = root_path / "lit.py"
                fake_lit.write_text("import sys\nsys.exit(0)\n")

                environment = os.environ.copy()
                environment["SWIFT_BUILD_ROOT"] = root
                result = subprocess.run(
                    [
                        sys.executable,
                        str(_RUN_TEST),
                        flag,
                        "--build",
                        "skip",
                        "--color",
                        "false",
                        "--lit",
                        str(fake_lit),
                    ],
                    capture_output=True,
                    env=environment,
                    text=True,
                )

                self.assertEqual(result.returncode, 0, result.stderr)


if __name__ == "__main__":
    unittest.main()
