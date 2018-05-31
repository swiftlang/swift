#===--- postbuild.py -----------------------------------------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https:#swift.org/LICENSE.txt for license information
# See https:#swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===----------------------------------------------------------------------===#

import os
import platform
import shutil

import swift_build_support.swift_build_support.products as products
import swift_build_support.swift_build_support.tar as tar
import swift_build_support.swift_build_support.shell as shell
from swift_build_support.swift_build_support.targets \
    import StdlibDeploymentTarget

def create_symbols_package(args):
    assert args.symbols_package
    print('--- Creating symbols package ---')
    print('-- Package file: {} --'.format(args.symbols_package))

    if platform.system() == 'Darwin':
        prefix = targets.darwin_toolchain_prefix(args.install_prefix)
    else:
        prefix = args.install_prefix

    # As a security measure, `tar` normally strips leading '/' from paths
    # it is archiving. To stay safe, we change working directories, then
    # run `tar` without the leading '/' (we remove it ourselves to keep
    # `tar` from emitting a warning).
    with shell.pushd(args.install_symroot):
        tar.tar(source=prefix.lstrip('/'),
                destination=args.symbols_package)

def build_benchmarks_linux(invocation):
    # Perform some sanity checking.
    args = invocation.args

    # For Linux, we need a full install root.
    assert args.install_destdir, "Can only benchmark if we compile a dest root?!"
    # We also need to have built and installed Foundation into the destdir.
    assert args.install_foundation, "Must build/install foundation to do benchmarks."

    install_destdir = os.path.join(args.install_destdir,
                                   args.install_prefix.lstrip('/'))

    workspace = invocation.workspace
    llvm = products.LLVM
    swift = products.Swift
    host_target = StdlibDeploymentTarget.host_target().name

    swift_src_dir = workspace.source_dir(swift.product_source_name())
    benchmark_src_dir = os.path.join(swift_src_dir, 'benchmark')
    llvm_build_dir = workspace.build_dir(host_target,
                                         llvm.product_source_name())
    benchmark_build_dir = workspace.build_dir(host_target, "swiftbench")

    # Clean the directory. Since this is an external build, we will not get
    # proper dependencies from swift's cmake.
    if os.path.isdir(benchmark_build_dir):
        shutil.rmtree(benchmark_build_dir)
    os.makedirs(benchmark_build_dir)

    cmake = invocation.toolchain.cmake
    # We use the just built clang
    clang = os.path.join(llvm_build_dir, 'bin', 'clang')

    swift_exec = os.path.join(install_destdir, 'bin', 'swiftc')
    swift_lib_path = os.path.join(install_destdir, 'lib', 'swift')
    num_o_iters = args.benchmark_num_o_iterations
    num_onone_iters = args.benchmark_num_onone_iterations
    configureInvocation = [
        cmake,
        '-GNinja',
        '-DSWIFT_EXEC={}'.format(swift_exec),
        '-DCLANG_EXEC={}'.format(clang),
        '-DSWIFT_LIBRARY_PATH={}'.format(swift_lib_path),
        '-DSWIFT_BENCHMARK_NUM_O_ITERATIONS={}'.format(num_o_iters),
        '-DSWIFT_BENCHMARK_NUM_ONONE_ITERATIONS={}'.format(num_onone_iters),
        '{}'.format(benchmark_src_dir)
    ]

    buildInvocation = [
        cmake, '--build', benchmark_build_dir, '--',
        'swift-benchmark-{}'.format(host_target)
    ]

    runInvocation = [
        cmake, '--build', benchmark_build_dir, '--',
        'run-swift-benchmark-{}'.format(host_target)
    ]

    with shell.pushd(benchmark_build_dir):
        shell.call(configureInvocation)
        shell.call(buildInvocation)
        shell.call(runInvocation)

# This is the main entrypoint for post build actions.
def execute(invocation):
    args = invocation.args
    if args.symbols_package:
        create_symbols_package(args)

    # If we were asked to benchmark and we are on Linux, build the benchmark
    # suite.
    #
    # TODO: Build Darwin the same way.
    if args.benchmark and platform.system() == 'Linux':
        build_benchmarks_linux(invocation)
