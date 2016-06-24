# swift_build_support/products/__init__.py ----------------------*- python -*-
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

from .cmark import CMark
from .foundation import Foundation
from .libdispatch import LibDispatch
from .llbuild import LLBuild
from .lldb import LLDB
from .llvm import LLVM
from .ninja import Ninja
from .swift import Swift
from .swiftpm import SwiftPM
from .xctest import XCTest

known_product_classes = [
    CMark,
    Foundation,
    LibDispatch,
    LLBuild,
    LLDB,
    LLVM,
    Ninja,
    Swift,
    SwiftPM,
    XCTest,
]

__all__ = [cls.__class__.__name__
           for cls in known_product_classes]
