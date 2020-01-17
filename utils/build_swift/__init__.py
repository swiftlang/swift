# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


from __future__ import absolute_import, unicode_literals

import sys
from importlib import import_module as _import_module

from .build_swift import __name__ as _build_swift__name__


_SUBMODULES = [
    'argparse',
    'class_utils',
    'defaults',
    'driver_arguments',
    'migration',
    'presets',
    'shell',
    'versions',
    'wrappers',
]


class _UnloadedModule(object):
    """Class used to delay loading modules until users explicitly import them.
    """

    def __init__(self, import_name):
        self._import_name = import_name

    def load(self):
        return _import_module(self._import_name)


class _ModuleImporter(object):
    """Module importer used to re-export nested submodules.

    Implements the importer protocol specified in PEP302.
        https://www.python.org/dev/peps/pep-0302/

    NOTE: The `six` package uses a similar trick to pull off the super nifty
    `six.moves` module namespace. Our use-case is much less sophisticated, we
    just want to expose a flat module structure for `build_swift`.
    """

    def __init__(self, name):
        self._name = name
        self._known_modules = {}

    def _add_module(self, fullname, module):
        self._known_modules['{}.{}'.format(self._name, fullname)] = module

    def _get_module(self, fullname):
        if fullname not in self._known_modules:
            raise ImportError('No module named {}'.format(fullname))

        module = self._known_modules[fullname]
        if isinstance(module, _UnloadedModule):
            self._known_modules[fullname] = module.load()

        return self._known_modules[fullname]

    def find_module(self, fullname, path=None):
        if fullname in self._known_modules:
            return self

        return None

    def load_module(self, fullname):
        if fullname in sys.modules:
            return sys.modules[fullname]

        module = self._get_module(fullname)
        sys.modules[fullname] = module
        return module


_importer = _ModuleImporter(__name__)

for name in _SUBMODULES:
    module = _UnloadedModule('{}.{}'.format(_build_swift__name__, name))
    _importer._add_module(name, module)

sys.meta_path.append(_importer)

del _build_swift__name__
del _importer
del _SUBMODULES
