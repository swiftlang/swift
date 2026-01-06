# swift_build_support/products/__init__.py ----------------------*- python -*-
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

from .benchmarks import Benchmarks
from .cmark import CMark
from .curl import LibCurl
from .earlyswiftdriver import EarlySwiftDriver
from .foundation import Foundation
from .foundationtests import FoundationTests
from .indexstoredb import IndexStoreDB
from .libcxx import LibCXX
from .libdispatch import LibDispatch
from .libxml2 import LibXML2
from .llbuild import LLBuild
from .lldb import LLDB
from .llvm import LLVM
from .minimalstdlib import MinimalStdlib
from .ninja import Ninja
from .playgroundsupport import PlaygroundSupport
from .skstresstester import SKStressTester
from .sourcekitlsp import SourceKitLSP
from .staticswiftlinux import StaticSwiftLinuxConfig
from .stdlib_docs import StdlibDocs
from .swift import Swift
from .swift_testing import SwiftTesting
from .swift_testing_macros import SwiftTestingMacros
from .swiftdocc import SwiftDocC
from .swiftdoccrender import SwiftDocCRender
from .swiftdriver import SwiftDriver
from .swiftformat import SwiftFormat
from .swiftfoundationtests import SwiftFoundationTests
from .swiftinspect import SwiftInspect
from .swiftpm import SwiftPM
from .swiftsyntax import SwiftSyntax
from .tsan_libdispatch import TSanLibDispatch
from .wasisysroot import WASILibc, WasmLLVMRuntimeLibs
from .wasmkit import WasmKit
from .wasmstdlib import WasmStdlib, WasmThreadsStdlib
from .wasmswiftsdk import WasmSwiftSDK
from .xctest import XCTest
from .zlib import Zlib

__all__ = [
    'CMark',
    'Foundation',
    'FoundationTests',
    'SwiftFoundationTests',
    'LibCXX',
    'LibDispatch',
    'LibXML2',
    'Zlib',
    'LibCurl',
    'LLBuild',
    'LLDB',
    'LLVM',
    'MinimalStdlib',
    'Ninja',
    'PlaygroundSupport',
    'StaticSwiftLinuxConfig',
    'StdlibDocs',
    'Swift',
    'SwiftFormat',
    'SwiftInspect',
    'SwiftPM',
    'SwiftDriver',
    'SwiftTesting',
    'SwiftTestingMacros',
    'EarlySwiftDriver',
    'XCTest',
    'SwiftSyntax',
    'SKStressTester',
    'IndexStoreDB',
    'SourceKitLSP',
    'Benchmarks',
    'TSanLibDispatch',
    'SwiftDocC',
    'SwiftDocCRender',
    'WASILibc',
    'WasmLLVMRuntimeLibs',
    'WasmKit',
    'WasmStdlib',
    'WasmThreadsStdlib',
    'WasmSwiftSDK',
]
