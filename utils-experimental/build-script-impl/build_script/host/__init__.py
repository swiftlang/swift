# swift_build/host/__init__.py ----------------------------------*- python -*-
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
Provide singleton object that represents host machine
"""
# ----------------------------------------------------------------------------

from __future__ import absolute_import

import platform

from . import host_target

__all__ = [
    'host'
]

_system = platform.system()
if _system == 'Darwin':
    from .darwin import MacOSX as Host
elif _system == 'Linux':
    from .unix import Linux as Host
elif _system == 'FreeBSD':
    from .unix import FreeBSD as Host
elif _system.startswith('CYGWIN'):
    from .unix import Cygwin as Host

host = Host(
        os_type=_system,
        deployment_target=host_target.host_target())
'''
Singleton Host
'''
