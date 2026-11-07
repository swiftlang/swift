# tests/products/test_build_script_helper_paths.py -----------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2026 Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
# ----------------------------------------------------------------------------

import importlib.util
from argparse import Namespace
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

from swift_build_support.products import benchmarks, swiftinspect


SWIFT_ROOT = Path(__file__).resolve().parents[4]


def load_helper(name, path):
    spec = importlib.util.spec_from_file_location(name, SWIFT_ROOT / path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class BuildScriptHelperPathTests(unittest.TestCase):
    def check_product_path(self, module, host, prefix, expected,
                           cross_compile=False):
        args = Namespace(
            install_destdir='/stage', install_prefix=prefix,
            cross_compile_hosts=[host] if cross_compile else [],
            cross_compile_append_host_target_to_destdir=cross_compile,
            host_target='linux-x86_64' if cross_compile else host)
        product = module.Benchmarks if module is benchmarks else module.SwiftInspect
        product = product(args, None, '/src/benchmarks', '/build/product')
        with patch.object(module.shell, 'call') as call:
            module.run_build_script_helper(host, product, args)
        command = call.call_args.args[0]
        self.assertEqual(command[command.index('--toolchain') + 1], expected)

    def test_nonstandard_linux_install_prefix(self):
        for module in (benchmarks, swiftinspect):
            with self.subTest(module=module.__name__):
                self.check_product_path(module, 'linux-x86_64', '/opt/swift',
                                        '/stage/opt/swift')

    def test_default_linux_install_prefix(self):
        for module in (benchmarks, swiftinspect):
            with self.subTest(module=module.__name__):
                self.check_product_path(module, 'linux-x86_64', '/usr',
                                        '/stage/usr')

    def test_darwin_toolchain_path_has_one_prefix(self):
        prefix = '/Applications/Swift.xctoolchain/usr'
        with patch('swift_build_support.targets.platform.system',
                   return_value='Darwin'):
            for module in (benchmarks, swiftinspect):
                with self.subTest(module=module.__name__):
                    self.check_product_path(
                        module, 'macosx-arm64', prefix,
                        '/stage/Applications/Swift.xctoolchain/usr')

    def test_darwin_benchmark_runtime_uses_toolchain_lib(self):
        args = Namespace(
            install_destdir='/stage',
            install_prefix='/Applications/Swift.xctoolchain/usr',
            cross_compile_hosts=[], cross_compile_append_host_target_to_destdir=False)
        product = benchmarks.Benchmarks(args, None, '/src/benchmarks', '/build')
        with patch('swift_build_support.targets.platform.system',
                   return_value='Darwin'):
            environment = product._get_test_environment('macosx-arm64')
        self.assertEqual(
            environment['DYLD_LIBRARY_PATH'],
            '/stage/Applications/Swift.xctoolchain/usr/lib/swift/macosx')

    def test_linux_cross_compile_keeps_each_install_directory(self):
        self.check_product_path(
            benchmarks, 'linux-aarch64', '/opt/swift',
            '/stage/linux-aarch64/opt/swift', cross_compile=True)
        self.check_product_path(
            swiftinspect, 'linux-aarch64', '/opt/swift',
            '/stage/opt/swift', cross_compile=True)

    def test_darwin_cross_compile_keeps_each_install_directory(self):
        prefix = '/Applications/Swift.xctoolchain/usr'
        with patch('swift_build_support.targets.platform.system',
                   return_value='Darwin'):
            self.check_product_path(
                benchmarks, 'macosx-arm64', prefix,
                '/build/intermediate-install/macosx-arm64'
                '/Applications/Swift.xctoolchain/usr', cross_compile=True)
            self.check_product_path(
                swiftinspect, 'macosx-arm64', prefix,
                '/stage/Applications/Swift.xctoolchain/usr',
                cross_compile=True)

    def test_helpers_use_toolchain_bin_directory(self):
        for name, path in (
            ('benchmark_helper', 'benchmark/scripts/build_script_helper.py'),
            ('swiftinspect_helper', 'tools/swift-inspect/build_script_helper.py'),
        ):
            with self.subTest(helper=name):
                helper = load_helper(name, path)
                argv = [str(SWIFT_ROOT / path), '--package-path', '/src',
                        '--build-path', '/build', '--toolchain', '/stage/opt/swift']
                with patch.object(sys, 'argv', argv), \
                     patch.object(helper.os.path, 'isdir', return_value=True), \
                     patch.object(helper, 'perform_build') as build:
                    helper.main()
                self.assertEqual(build.call_args.args[1],
                                 '/stage/opt/swift/bin/swift-build')

    @unittest.skipUnless(os.name == 'posix', 'requires an executable shell script')
    def test_helpers_run_swift_build_from_custom_prefix(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            toolchain = root / 'stage' / 'opt' / 'swift'
            toolchain_bin = toolchain / 'bin'
            toolchain_bin.mkdir(parents=True)
            fake_bin = root / 'package-bin'
            fake_bin.mkdir()
            (fake_bin / 'SwiftBench').write_text('benchmark')

            swift_build = toolchain_bin / 'swift-build'
            swift_build.write_text(
                '#!/bin/sh\n'
                'for arg in "$@"; do\n'
                '  if [ "$arg" = "--show-bin-path" ]; then\n'
                '    printf "%s\\n" "$TEST_BIN_DIR"\n'
                '    exit 0\n'
                '  fi\n'
                'done\n'
                'printf "called\\n" >> "$TEST_MARKER"\n')
            swift_build.chmod(0o755)

            for name, helper_path, calls in (
                ('benchmark', 'benchmark/scripts/build_script_helper.py', 3),
                ('swiftinspect', 'tools/swift-inspect/build_script_helper.py', 1),
            ):
                with self.subTest(helper=name):
                    marker = root / (name + '-called')
                    build_path = root / (name + '-build')
                    environment = dict(
                        os.environ, TEST_BIN_DIR=str(fake_bin),
                        TEST_MARKER=str(marker))
                    subprocess.run(
                        [sys.executable, str(SWIFT_ROOT / helper_path),
                         '--package-path', '/src', '--build-path', str(build_path),
                         '--toolchain', str(toolchain)],
                        check=True, capture_output=True, text=True,
                        env=environment)
                    self.assertEqual(marker.read_text().splitlines(),
                                     ['called'] * calls)
                    if name == 'benchmark':
                        self.assertEqual(
                            sorted(path.name for path in
                                   (build_path / 'bin').iterdir()),
                            ['Benchmark_O', 'Benchmark_Onone', 'Benchmark_Osize'])


if __name__ == '__main__':
    unittest.main()
