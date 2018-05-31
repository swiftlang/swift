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

def create_symbols_package(args):
    assert(args.symbols_package)
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

# This is the main entrypoint for post build actions.
def execute(invocation):
    args = invocation.args
    if args.symbols_package:
        create_symbols_package(args)
