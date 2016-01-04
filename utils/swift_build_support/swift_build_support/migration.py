# swift_build_support/migration.py - Migrating build-script -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------
#
# utils/build-script takes arguments for its argument parser, as well as
# arguments that are meant to be passed directly to utils/build-script-impl.
# In order to gradually migrate away from build-script-impl, this module
# provides tools to handle parsing of these args.
#
# ----------------------------------------------------------------------------


def migrate_impl_args(argv, migrate_args):
    """
    Given a list of arguments of the form:

        --foo --bar=baz -- --flim=flam

    And a list of arguments to migrate, return a list in which the arguments
    to migrate come before the '--' separator. For example, were we to migrate
    '--flim', we would return:

        --foo --bar=baz --flim=flam --

    Note that we do not attempt to remove the '--' separator if it is no longer
    necessary, nor do we replace '--flim' if it already appears before the
    separator. In these cases, argparse "does the right thing": it can handle
    a trailing separator, and when options that are specified twice argparse
    uses the second value.
    """
    try:
        split_index = argv.index('--')
    except ValueError:
        # If there is no separator, then we have nothing to migrate.
        return argv

    args = argv[:split_index]
    impl_args = argv[split_index:]
    impl_args_to_remove = []
    for index, impl_arg in enumerate(impl_args):
        if impl_arg.split('=')[0] in migrate_args:
            args.append(impl_arg)
            impl_args_to_remove.append(impl_arg)

    for impl_arg_to_remove in impl_args_to_remove:
        impl_args.remove(impl_arg_to_remove)

    return args + impl_args
