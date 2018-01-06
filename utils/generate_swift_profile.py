#! /usr/bin/env python


import argparse
import glob
import os
import subprocess
import sys
from contextlib import contextmanager


def abspath_join(*paths):
    return os.path.abspath(os.path.join(*paths))


UTILS_DIR = abspath_join(__file__, os.pardir)
SWIFT_DIR = abspath_join(UTILS_DIR, os.pardir)
WORKSPACE_DIR = abspath_join(SWIFT_DIR, os.pardir)
BUILD_DIR = abspath_join(WORKSPACE_DIR, 'build')

SOURCE_COMPAT_SUITE_URL = 'https://github.com/apple/swift-source-compat-suite'
SOURCE_COMPAT_SUITE_DIR = abspath_join(WORKSPACE_DIR,
                                       'swift-source-compat-suite')


def write_command(command, stdout=sys.stdout):
    command = ' '.join(command)
    stdout.write('>>> ' + command + '\n')


def call(command, **kwargs):
    write_command(command)
    p = subprocess.Popen(command, **kwargs)

    try:
        p.wait()
    except Exception:
        p.kill()
        p.wait()
        raise

    if p.returncode:
        raise subprocess.CalledProcessError(p.returncode, command)

    return p


@contextmanager
def pushd(dir):
    previous_dir = os.getcwd()
    current_dir = os.path.realpath(dir)

    write_command(['pushd', current_dir])
    os.chdir(current_dir)

    yield

    write_command(['popd'])
    os.chdir(previous_dir)


class SwiftBuildConfig(object):

    BUILD_SUBDIR = 'swift-profile-gen'
    INSTALL_PREFIX = os.path.join(os.sep, 'toolchain', 'usr')

    def __init__(self, **kwargs):
        self.assertions = kwargs.get('assertions', False)
        self.release = kwargs.get('release', False)

        self.build_subdir = SwiftBuildConfig.BUILD_SUBDIR
        self.install_prefix = SwiftBuildConfig.INSTALL_PREFIX

        self.build_dir = os.path.join(BUILD_DIR, self.build_subdir)
        self.profiles_dir = os.path.join(self.build_dir, 'profiles')
        self.install_dir = os.path.join(self.build_dir, 'install')
        self.symroot_dir = os.path.join(self.build_dir, 'symroot')

        self.root_path = os.path.join(self.build_dir, 'root.tar.gz')
        self.root_symbols_path = os.path.join(self.build_dir,
                                              'root-symbols.tar.gz')

    @staticmethod
    def from_args(args):
        return SwiftBuildConfig(**{
            'assertions': args.assertions,
            'release': args.release,
        })


def build_argument_parser():
    parser = argparse.ArgumentParser()

    parser.set_defaults(assertions=False, release=False)
    parser.add_argument('--assertions', action='store_true',
                        help='Enable assertions')
    parser.add_argument('--release', action='store_true',
                        help='Build Swift in release mode')

    return parser


def build_swift(config):
    def cmake_list(items):
        return ';'.join(items)

    llvm_install_components = cmake_list([
        'libclang',
        'libclang-headers',
        'llvm-profdata',
    ])

    swift_install_components = cmake_list([
        'clang-builtin-headers',
        'compiler',
        'license',
        'sdk-overlay',
        'sourcekit-xpc-service',
        'stdlib',
        'swift-remote-mirror',
        'swift-remote-mirror-headers',
    ])

    swift_cmake_options = ','.join([
        '-DSWIFT_ENABLE_PROFILE_GENERATION:BOOL=ON',
        '-DSWIFT_ENABLE_IR_PROFILE_GENERATION:BOOL=ON',
        '-DSWIFT_PROFILE_OUTPUT_DIR={}'.format(config.profiles_dir),
    ])

    build_script_options = [
        '--reconfigure',
        '--verbose-build',
        '--release' if config.release else '--debug',
        '--assertions' if config.assertions else '--no-assertions',
        '--build-subdir={}'.format(config.build_subdir),
        '--build-ninja',
        '--llbuild',
        '--swiftpm',
        '--ios',
        '--tvos',
        '--watchos',
        '--skip-build-benchmarks',

        '--darwin-install-extract-symbols',
        '--darwin-toolchain-alias=swift',
        '--darwin-toolchain-bundle-identifier=org.swift.compat-macos',
        '--darwin-toolchain-display-name-short="Swift Development Snapshot"'
        '--darwin-toolchain-display-name="Swift Development Snapshot"',
        '--darwin-toolchain-name=swift-DEVELOPMENT-SNAPSHOT',
        '--darwin-toolchain-version=4.999.999',

        '--install-llbuild',
        '--install-swift',
        '--install-swiftpm',
        '--install-prefix={}'.format(config.install_prefix),
        '--install-destdir={}'.format(config.install_dir),
        '--install-symroot={}'.format(config.symroot_dir),
        '--installable-package={}'.format(config.root_path),
        '--symbols-package={}'.format(config.root_symbols_path),
        '--llvm-install-components={}'.format(llvm_install_components),
        '--swift-install-components={}'.format(swift_install_components),
        '--extra-cmake-options={}'.format(swift_cmake_options),
    ]

    with pushd(SWIFT_DIR):
        return call(['./utils/build-script'] + build_script_options)


def main():
    parser = build_argument_parser()
    args = parser.parse_args()
    config = SwiftBuildConfig.from_args(args)

    bin_dir = os.path.join(config.install_dir, 'toolchain', 'usr', 'bin')
    profdata_bin = os.path.join(bin_dir, 'llvm-profdata')
    swiftc_bin = os.path.join(bin_dir, 'swiftc')

    def merge_profraw_files(output_file, output_dir=WORKSPACE_DIR):
        output_path = os.path.join(output_dir, output_file)

        with pushd(config.profiles_dir):
            profraw_files = glob.glob('*.profraw')

            call([profdata_bin, 'merge', '-o', output_path] + profraw_files)
            call(['rm', '-f'] + profraw_files)

    build_swift(config)

    # Merge and remove *.profraw files from building the stdlib
    merge_profraw_files('stdlib.profdata')

    # Generate profiles using the source compat suite
    if not os.exists(SOURCE_COMPAT_SUITE_DIR):
        call(['git', 'clone', SOURCE_COMPAT_SUITE_URL,
              SOURCE_COMPAT_SUITE_DIR])
    else:
        with pushd(SOURCE_COMPAT_SUITE_DIR):
            call(['git', 'clean', '-ffxd'])
            call(['git', 'fetch', '--all'])
            call(['git', 'pull'])

    projects_file = os.path.join(UTILS_DIR, 'swift-profile-projects.json')
    with pushd(SOURCE_COMPAT_SUITE_DIR):
        call([
            './runner.py',
            '--swiftc', swiftc_bin,
            '--projects', projects_file,
        ])

    # Merge and remove *.profraw files from building the source-compat-suite
    merge_profraw_files('source-compat-suite.profdata')


if __name__ == '__main__':
    main()
