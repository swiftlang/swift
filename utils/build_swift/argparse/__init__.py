# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


from argparse import (ArgumentDefaultsHelpFormatter, ArgumentError,
                      ArgumentTypeError, FileType, HelpFormatter,
                      Namespace, RawDescriptionHelpFormatter,
                      RawTextHelpFormatter)

from argparse import SUPPRESS, OPTIONAL, ZERO_OR_MORE, ONE_OR_MORE

from .actions import Action, Nargs
from .parser import ArgumentParser
from .types import PathType, RegexType, ClangVersionType, SwiftVersionType


__all__ = [
    'Action',
    'ArgumentDefaultsHelpFormatter',
    'ArgumentError',
    'ArgumentParser',
    'ArgumentTypeError',
    'HelpFormatter',
    'Namespace',
    'Nargs',
    'RawDescriptionHelpFormatter',
    'RawTextHelpFormatter',

    'FileType',
    'PathType',
    'RegexType',
    'ClangVersionType',
    'SwiftVersionType',

    'SUPPRESS',
    'OPTIONAL',
    'ZERO_OR_MORE',
    'ONE_OR_MORE',
]
