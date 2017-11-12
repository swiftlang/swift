# swift_build_support/products/swift.py -------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

from . import product


class Swift(product.Product):

    def __init__(self, args, toolchain, source_dir, build_dir):
        product.Product.__init__(self, args, toolchain, source_dir,
                                 build_dir)
        # Add any runtime sanitizer arguments.
        self.cmake_options.extend(self._runtime_sanitizer_flags)

        # Add any compiler vendor cmake flags.
        self.cmake_options.extend(self._compiler_vendor_flags)

        # Add any swift version related cmake flags.
        self.cmake_options.extend(self._version_flags)

        # Add benchmark specific flags.
        self.cmake_options.extend(self._benchmark_flags)

        # Add any sil ownership flags.
        self.cmake_options.extend(self._sil_ownership_flags)

        # Generate the compile db.
        self.cmake_options.extend(self._compile_db_flags)

        # Add the flag if we are supposed to force the typechecker to compile
        # with optimization.
        self.cmake_options.extend(self._force_optimized_typechecker_flags)

    @property
    def _runtime_sanitizer_flags(self):
        sanitizer_list = []
        if self.args.enable_tsan_runtime:
            sanitizer_list += ['Thread']
        if len(sanitizer_list) == 0:
            return []
        return ["-DSWIFT_RUNTIME_USE_SANITIZERS=%s" %
                ";".join(sanitizer_list)]

    @property
    def _compiler_vendor_flags(self):
        if self.args.compiler_vendor == "none":
            return []

        if self.args.compiler_vendor != "apple":
            raise RuntimeError("Unknown compiler vendor?! Was build-script \
updated without updating swift.py?")

        swift_compiler_version = ""
        if self.args.swift_compiler_version is not None:
            swift_compiler_version = self.args.swift_compiler_version

        return [
            "-DSWIFT_VENDOR=Apple",
            "-DSWIFT_VENDOR_UTI=com.apple.compilers.llvm.swift",

            # This has a default of 3.0, so it should be safe to use here.
            "-DSWIFT_VERSION={}".format(self.args.swift_user_visible_version),

            # FIXME: We are matching build-script-impl here. But it seems like
            # bit rot since this flag is specified in another place with the
            # exact same value in build-script-impl.
            "-DSWIFT_COMPILER_VERSION={}".format(swift_compiler_version),
        ]

    @property
    def _version_flags(self):
        r = []
        if self.args.swift_compiler_version is not None:
            swift_compiler_version = self.args.swift_compiler_version
            r.append(
                "-DSWIFT_COMPILER_VERSION={}".format(swift_compiler_version)
            )
        if self.args.clang_compiler_version is not None:
            clang_compiler_version = self.args.clang_compiler_version
            r.append(
                "-DCLANG_COMPILER_VERSION={}".format(clang_compiler_version)
            )
        return r

    @property
    def _benchmark_flags(self):
        if not self.args.benchmark:
            return []

        onone_iters = self.args.benchmark_num_onone_iterations
        o_iters = self.args.benchmark_num_o_iterations
        return [
            "-DSWIFT_BENCHMARK_NUM_ONONE_ITERATIONS={}".format(onone_iters),
            "-DSWIFT_BENCHMARK_NUM_O_ITERATIONS={}".format(o_iters)
        ]

    @property
    def _sil_ownership_flags(self):
        if not self.args.enable_sil_ownership:
            return ["-DSWIFT_STDLIB_ENABLE_SIL_OWNERSHIP=FALSE"]
        return ["-DSWIFT_STDLIB_ENABLE_SIL_OWNERSHIP=TRUE"]

    @property
    def _compile_db_flags(self):
        return ['-DCMAKE_EXPORT_COMPILE_COMMANDS=TRUE']

    @property
    def _force_optimized_typechecker_flags(self):
        if not self.args.force_optimized_typechecker:
            return ['-DSWIFT_FORCE_OPTIMIZED_TYPECHECKER=FALSE']
        return ['-DSWIFT_FORCE_OPTIMIZED_TYPECHECKER=TRUE']
