# swift_build_support/products/stdlib_docs.py -------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

import os

from . import product
from . import swiftdocc
from . import swiftdoccrender
from .. import shell

from build_swift.build_swift.constants import SWIFT_SOURCE_ROOT

# Modules whose symbol graphs must not reach the standard library docs.
# This is a deny-list, so a newly added module appears in the docs by default.
EXCLUDED_SYMBOL_GRAPH_MODULES = frozenset([
    # Test support and private helpers.
    'StdlibUnittest',
    'StdlibUnittestFoundationExtras',
    'RuntimeUnittest',
    'SwiftReflectionTest',
    'OSLogTestHelper',
    'SwiftPrivate',
    'SwiftPrivateThreadExtras',
    'SwiftPrivateLibcExtras',
    'DifferentiationUnittest',
    # Not part of the public documentation.
    'SwiftOnoneSupport',
    # Implementation-only. Its extensions of Swift (Unicode.Script and
    # friends) are not visible to clients.
    '_RegexParser',
    # Not emitted today (they crash SymbolGraphGen), but never document them.
    'Darwin',
])

_SYMBOL_GRAPH_SUFFIX = '.symbols.json'


def symbol_graph_module_name(filename):
    """Return the module a symbol graph file describes, or None.

    `Foo.symbols.json` describes `Foo`. `Foo@Bar.symbols.json` describes
    extensions that module `Foo` makes to `Bar`, so it also belongs to `Foo`.
    """
    if not filename.endswith(_SYMBOL_GRAPH_SUFFIX):
        return None
    stem = filename[:-len(_SYMBOL_GRAPH_SUFFIX)]
    return stem.split('@', 1)[0]


def remove_excluded_symbol_graphs(symbol_graph_dir):
    """Delete symbol graphs of excluded modules from symbol_graph_dir.

    This edits the directory in place. Anything that later runs docc against
    it, including scripts outside this repository, sees only the kept graphs.
    """
    for root, _, files in os.walk(symbol_graph_dir):
        for name in files:
            module = symbol_graph_module_name(name)
            if module in EXCLUDED_SYMBOL_GRAPH_MODULES:
                os.remove(os.path.join(root, name))


class StdlibDocs(product.Product):
    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False

    @classmethod
    def is_swiftpm_unified_build_product(cls):
        return False

    def should_build(self, host_target):
        return self.args.build_stdlib_docs

    def build(self, host_target):
        toolchain_path = self.install_toolchain_path(host_target)
        docc_path = os.path.join(toolchain_path, "bin", "docc")

        swift_build_dir = os.path.join(
            os.path.dirname(self.build_dir),
            f'swift-{host_target}'
        )

        docc_catalog_path = os.path.join(
            SWIFT_SOURCE_ROOT,
            "swift",
            "stdlib",
            "stdlib.docc"
        )

        symbol_graph_dir = os.path.join(swift_build_dir, "lib", "symbol-graph")
        remove_excluded_symbol_graphs(symbol_graph_dir)
        output_path = os.path.join(swift_build_dir, "Swift.doccarchive")

        docc_action = 'preview' if self.args.preview_stdlib_docs else 'convert'
        static_hosting_option = '--transform-for-static-hosting' if self.args.stdlib_docs_static_hosting else ''
        hosting_base_path = self.args.stdlib_docs_hosting_base_path

        docc_cmd = [
            docc_path,
            docc_action,
            docc_catalog_path,
            "--additional-symbol-graph-dir",
            symbol_graph_dir,
            "--output-path",
            output_path,
            "--default-code-listing-language",
            "swift",
            "--fallback-display-name",
            "Swift",
            "--fallback-bundle-identifier",
            "org.swift.swift",
            static_hosting_option,
            "--hosting-base-path",
            hosting_base_path
        ]

        shell.call(docc_cmd)

    def should_test(self, host_target):
        return False

    def should_install(self, host_target):
        return False

    @classmethod
    def get_dependencies(cls):
        """Return a list of products that this product depends upon"""
        return [
            swiftdocc.SwiftDocC,
            swiftdoccrender.SwiftDocCRender
        ]
