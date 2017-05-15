# build_script/products/__init__.py -----------------------------*- python -*-
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
"""
Products package
"""
# ----------------------------------------------------------------------------

from __future__ import absolute_import

from .ninja import Ninja
from .cmark import CMark
from .llvm import LLVM
from .lldb import LLDB
from .llbuild import LLBuild
from .swift import Swift
from .swiftpm import SwiftPM
from .libdispatch import LibDispatch
from .foundation import Foundation
from .xctest import XCTest

__all__ = [
    'Ninja',
    'CMark',
    'LLVM',
    'LLDB',
    'LLBuild',
    'Swift',
    'SwiftPM',
    'LibDispatch',
    'Foundation',
    'XCTest',
]
