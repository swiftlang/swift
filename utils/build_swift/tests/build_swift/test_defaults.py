# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


import unittest

from build_swift import defaults
from build_swift import shell

from .. import utils


try:
    # Python 3.3
    from unittest import mock
    from unittest.mock import patch, MagicMock
except ImportError:
    mock = None

    class MagicMock(object):
        def __init__(self, *args, **kwargs):
            pass

    def patch(*args, **kwargs):
        return lambda func: func


# ----------------------------------------------------------------------------
# Constants

_SYSCTL_HW_MEMSIZE = 17179869184
_SYSCTL_HW_MEMSIZE_OUTPUT = 'hw.memsize: {}'.format(_SYSCTL_HW_MEMSIZE)

# Safe upper bound to soundness check the LTO link job heuristics.
_LTO_LINK_JOBS_UPPER_BOUND = 100


# ----------------------------------------------------------------------------

class TestDefaults(unittest.TestCase):
    """Unit tests for the defaults module in build_swift.
    """

    # ------------------------------------------------------------------------
    # _system_memory

    @utils.requires_module('unittest.mock')
    @patch('platform.platform', MagicMock(return_value='Darwin'))
    def test_system_memory_darwin_platform(self):
        with mock.patch.object(shell, 'check_output') as mock_check_output:
            mock_check_output.return_value = _SYSCTL_HW_MEMSIZE_OUTPUT

            self.assertEqual(
                defaults._system_memory(), _SYSCTL_HW_MEMSIZE)

    @utils.requires_module('unittest.mock')
    @patch('platform.platform', MagicMock(return_value='Darwin'))
    def test_system_memory_darwin_platform_when_sysctl_fails(self):
        with mock.patch.object(shell, 'check_output') as mock_check_output:
            mock_check_output.side_effect = shell.CalledProcessError(
                returncode=1,
                cmd=['sysctl', 'hw.memsize'])

            self.assertIsNone(defaults._system_memory())

    @utils.requires_module('unittest.mock')
    @patch('platform.platform', MagicMock(return_value='Linux'))
    def test_system_memory_linux_platform(self):
        self.assertIsNone(defaults._system_memory())

    @utils.requires_module('unittest.mock')
    @patch('platform.platform', MagicMock(return_value='Windows'))
    def test_system_memory_windows_platform(self):
        self.assertIsNone(defaults._system_memory())

    # ------------------------------------------------------------------------
    # _default_llvm_lto_link_jobs

    @utils.requires_module('unittest.mock')
    def test_default_llvm_lto_link_jobs(self):
        with mock.patch.object(defaults, '_system_memory') as mock_memory:
            mock_memory.return_value = _SYSCTL_HW_MEMSIZE

            lto_link_jobs = defaults._default_llvm_lto_link_jobs()

            self.assertIsNotNone(lto_link_jobs)
            self.assertLess(lto_link_jobs, _LTO_LINK_JOBS_UPPER_BOUND)

    @utils.requires_module('unittest.mock')
    def test_default_llvm_lto_link_jobs_with_unknown_system_memory(self):
        with mock.patch.object(defaults, '_system_memory') as mock_memory:
            mock_memory.return_value = None

            self.assertIsNone(defaults._default_llvm_lto_link_jobs())

    # ------------------------------------------------------------------------
    # _default_swift_lto_link_jobs

    @utils.requires_module('unittest.mock')
    def test_default_swift_lto_link_jobs(self):
        with mock.patch.object(defaults, '_system_memory') as mock_memory:
            mock_memory.return_value = _SYSCTL_HW_MEMSIZE

            lto_link_jobs = defaults._default_swift_lto_link_jobs()

            self.assertIsNotNone(lto_link_jobs)
            self.assertLess(lto_link_jobs, _LTO_LINK_JOBS_UPPER_BOUND)

    @utils.requires_module('unittest.mock')
    def test_default_swift_lto_link_jobs_with_unknown_system_memory(self):
        with mock.patch.object(defaults, '_system_memory') as mock_memory:
            mock_memory.return_value = None

            self.assertIsNone(defaults._default_llvm_lto_link_jobs())
