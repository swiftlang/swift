
import os

from .. import shell
from ..utils import printf
from ..exceptions import BuildError

class Foundation(object):

    source_dir = None

    @classmethod
    def prepare(cls, workspace):
        cls.source_dir = workspace.subdir('swift-corelibs-foundation')
        if cls.source_dir is None:
            raise BuildError("Couldn't find Foundation source directory.")

    def __init__(self,
                 deployment_target,
                 target_build_dir,
                 target_install_destdir,
                 llvm_build,
                 swift_build,
                 libdispatch_build,
                 ninja_build,
                 args):
        self.deployment_target = deployment_target
        self.build_dir = target_build_dir
        self.install_destdir = target_install_destdir

        self.llvm_build = llvm_build
        self.swift_build = swift_build
        self.libdispatch_build = libdispatch_build
        self.ninja_build = ninja_build
        self.xctest_build = None  # To be populated later.

        self.args = args

    @property
    def foundation_path(self):
        return os.path.join(self.build_dir, 'Foundation')

    def _ninja_bin(self):
        ninja_bin = 'ninja'
        if self.ninja_build is not None:
            ninja_bin = self.ninja_build.ninja_bin_path
        return ninja_bin

    def configure(self):

        # FIXME: Foundation always require re-configuration because
        #        `configure` script pollutes it's source directory, that might
        #        cause cross compilation failure.

        # The configuration script requires knowing about XCTest's
        # location for building and running the tests. Note that XCTest
        # is build *after* foundation
        assert self.xctest_build is not None

        env = [
            ('SWIFTC', self.swift_build.swiftc_bin_path),
            ('CLANG', self.llvm_build.clang_bin_path),
            ('SWIFT', self.swift_build.swift_bin_path),
            ('SDKROOT', self.swift_build.build_dir),
            ('BUILD_DIR', self.build_dir),
            ('DSTROOT', self.args.install_destdir),
            ('PREFIX', self.args.install_prefix)]

        command = ['./configure', self.args.foundation_build_type]
        command += [
            '-DXCTEST_BUILD_DIR=' +  self.xctest_build.build_dir]
        if self.libdispatch_build is not None:
            libdispatch_source_dir = self.libdispatch_build.source_dir
            libdispatch_build_dir = self.libdispatch_build.build_dir
            command += [
                '-DLIBDISPATCH_SOURCE_DIR=' + libdispatch_source_dir,
                '-DLIBDISPATCH_BUILD_DIR=%s' + libdispatch_build_dir]

        with shell.pushd(self.source_dir):
            shell.invoke(command, env=env)

    def build(self):
        with shell.pushd(self.source_dir):
            shell.invoke([self._ninja_bin(), ])

    def test(self):
        if self.args.skip_test_foundation:
            return

        def ld_library_path():
            libpath = [
                os.path.join(self.args.install_destdir,
                             self.args.install_prefix.lstrip('/'),
                             'lib', 'swift'),
                os.path.join(self.build_dir,
                             'Foundation'),
                self.xctest_build.build_dir]
            if 'LD_LIBRARY_PATH' in os.environ:
                libpath += [os.environ['LD_LIBRARY_PATH'], ]
            return os.pathsep.join(libpath)
        env = [
            ('LD_LIBRARY_PATH', ld_library_path()),]

        test_executable = os.path.join(
            self.build_dir, "TestFoundation", "TestFoundation")

        printf("--- Running tests for Foundation ---")
        with shell.pushd(self.source_dir):
            shell.invoke([self._ninja_bin(), "TestFoundation"])
            shell.invoke([test_executable, ], env=env)
        printf("--- Finished tests for Foundation ---")

    def install(self):
        if not self.args.install_foundation:
            return

        printf("--- Installing Foundation ---")
        with shell.pushd(self.source_dir):
            shell.invoke([self._ninja_bin(), 'install'])
