# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


from __future__ import absolute_import, unicode_literals

import os.path
import unittest

from build_swift import shell
from build_swift.versions import Version
from build_swift.wrappers import _xcrun

from ... import utils


try:
    # Python 3
    from unittest import mock
except ImportError:
    pass


# -----------------------------------------------------------------------------
# Constants

_UNKNOWN_TOOL = 'gcc'
_KNOWN_TOOL = 'clang'
_KNOWN_TOOL_PATH = os.path.join(os.sep, 'usr', 'bin', _KNOWN_TOOL)

_UNKNOWN_SDK = 'phoneyos'
_KNOWN_SDK = 'macosx'
_KNOWN_SDK_PATH = os.path.join(os.sep, 'path', 'to', 'sdk')
_KNOWN_SDK_VERSION = '10.15'
_KNOWN_SDK_BUILD_VERSION = '10Z42b'
_KNOWN_SDK_PLATFORM_PATH = os.path.join(_KNOWN_SDK_PATH, 'platform')
_KNOWN_SDK_PLATFORM_VERSION = '10.15'


# -----------------------------------------------------------------------------

class TestXcrunHelpers(unittest.TestCase):
    """Unit tests for the helper functions and decorators defined in
    build_swift.wrappers.xcrun.
    """

    # -------------------------------------------------------------------------
    # _catch_return_none

    def test_catch_return_none_raises_returns_none(self):
        @_xcrun._catch_return_none(Exception)
        def raises():
            raise Exception

        self.assertIsNone(raises())

    def test_catch_return_none_success_return_value(self):
        @_xcrun._catch_return_none(Exception)
        def succeeds():
            return True

        self.assertTrue(succeeds())

    # -------------------------------------------------------------------------
    # _prepend_sdk_and_toolchain

    def test_prepend_sdk_and_toolchain(self):
        class Test(object):
            @_xcrun._prepend_sdk_and_toolchain
            def method(self, args, **kwargs):
                return args

        obj = Test()

        self.assertEqual(obj.method([]), [])

        self.assertEqual(
            obj.method([], sdk='some-sdk'),
            ['--sdk', 'some-sdk'])

        self.assertEqual(
            obj.method([], toolchain='some-toolchain'),
            ['--toolchain', 'some-toolchain'])

        self.assertEqual(
            obj.method([], sdk='some-sdk', toolchain='some-toolchain'),
            ['--sdk', 'some-sdk', '--toolchain', 'some-toolchain'])


class TestXcrunWrapper(unittest.TestCase):
    """Unit tests for the XcrunWrapper defined in build_swift.wrappers.xcrun.
    """

    def setUp(self):
        self.xcrun = _xcrun.XcrunWrapper()

    # -------------------------------------------------------------------------
    # version

    @utils.requires_module('unittest.mock')
    def test_version_error_returns_none(self):
        with mock.patch('build_swift.shell.check_output') as check_output:
            check_output.side_effect = shell.CalledProcessError(-1, None)

            result = self.xcrun.version

        check_output.assert_called_with([self.xcrun.EXECUTABLE, '--version'])
        self.assertIsNone(result)

    @utils.requires_module('unittest.mock')
    def test_version_invalid_version_string(self):
        with mock.patch('build_swift.shell.check_output') as check_output:
            check_output.side_effect = 'Invalid version output!\n'

            result = self.xcrun.version

        check_output.assert_called_with([self.xcrun.EXECUTABLE, '--version'])
        self.assertIsNone(result)

    @utils.requires_module('unittest.mock')
    def test_version(self):
        with mock.patch('build_swift.shell.check_output') as check_output:
            check_output.return_value = 'xcrun version 42.\n'

            result = self.xcrun.version

        check_output.assert_called_with([self.xcrun.EXECUTABLE, '--version'])
        self.assertEqual(result, Version('42'))

    # -------------------------------------------------------------------------
    # find

    @utils.requires_module('unittest.mock')
    def test_find_missing_tool(self):
        with mock.patch('build_swift.shell.check_output') as check_output:
            check_output.side_effect = shell.CalledProcessError(-1, None)

            result = self.xcrun.find(_UNKNOWN_TOOL, sdk=_KNOWN_SDK)

        command = [
            self.xcrun.EXECUTABLE,
            '--sdk', _KNOWN_SDK,
            '--find', _UNKNOWN_TOOL,
        ]

        check_output.assert_called_with(command, stderr=shell.DEVNULL)
        self.assertIsNone(result)

    @utils.requires_module('unittest.mock')
    def test_find_existing_tool(self):
        with mock.patch('build_swift.shell.check_output') as check_output:
            check_output.return_value = '{}\n'.format(_KNOWN_TOOL_PATH)

            result = self.xcrun.find(_KNOWN_TOOL, sdk=_KNOWN_SDK)

        command = [
            self.xcrun.EXECUTABLE,
            '--sdk', _KNOWN_SDK,
            '--find', _KNOWN_TOOL,
        ]

        check_output.assert_called_with(command, stderr=shell.DEVNULL)
        self.assertEqual(result, _KNOWN_TOOL_PATH)

    # -------------------------------------------------------------------------
    # kill_cache

    @utils.requires_module('unittest.mock')
    def test_kill_cache(self):
        with mock.patch.object(self.xcrun, 'call') as mock_call:
            self.xcrun.kill_cache()

        mock_call.assert_called_with('--kill-cache')

    # -------------------------------------------------------------------------
    # sdk_path

    @utils.requires_module('unittest.mock')
    def test_sdk_path_unknown_sdk(self):
        with mock.patch('build_swift.shell.check_output') as check_output:
            check_output.side_effect = shell.CalledProcessError(-1, None)

            result = self.xcrun.sdk_path(sdk=_UNKNOWN_SDK)

        command = [
            self.xcrun.EXECUTABLE,
            '--sdk', _UNKNOWN_SDK,
            '--show-sdk-path',
        ]

        check_output.assert_called_with(command, stderr=shell.DEVNULL)
        self.assertIsNone(result)

    @utils.requires_module('unittest.mock')
    def test_sdk_path_known_sdk(self):
        with mock.patch('build_swift.shell.check_output') as check_output:
            check_output.return_value = '{}\n'.format(_KNOWN_SDK_PATH)

            result = self.xcrun.sdk_path(sdk=_KNOWN_SDK)

        command = [
            self.xcrun.EXECUTABLE,
            '--sdk', _KNOWN_SDK,
            '--show-sdk-path',
        ]

        check_output.assert_called_with(command, stderr=shell.DEVNULL)
        self.assertEqual(result, _KNOWN_SDK_PATH)

    # -------------------------------------------------------------------------
    # sdk_version

    @utils.requires_module('unittest.mock')
    def test_sdk_version_unknown_sdk(self):
        with mock.patch('build_swift.shell.check_output') as check_output:
            check_output.side_effect = shell.CalledProcessError(-1, None)

            result = self.xcrun.sdk_version(sdk=_UNKNOWN_SDK)

        command = [
            self.xcrun.EXECUTABLE,
            '--sdk', _UNKNOWN_SDK,
            '--show-sdk-version',
        ]

        check_output.assert_called_with(command, stderr=shell.DEVNULL)
        self.assertIsNone(result)

    @utils.requires_module('unittest.mock')
    def test_sdk_version_known_sdk(self):
        with mock.patch('build_swift.shell.check_output') as check_output:
            check_output.return_value = '{}\n'.format(_KNOWN_SDK_VERSION)

            result = self.xcrun.sdk_version(sdk=_KNOWN_SDK)

        command = [
            self.xcrun.EXECUTABLE,
            '--sdk', _KNOWN_SDK,
            '--show-sdk-version',
        ]

        check_output.assert_called_with(command, stderr=shell.DEVNULL)
        self.assertEqual(result, Version(_KNOWN_SDK_VERSION))

    # -------------------------------------------------------------------------
    # sdk_build_version

    @utils.requires_module('unittest.mock')
    def test_sdk_build_version_unknown_sdk(self):
        with mock.patch('build_swift.shell.check_output') as check_output:
            check_output.side_effect = shell.CalledProcessError(-1, None)

            result = self.xcrun.sdk_build_version(sdk=_UNKNOWN_SDK)

        command = [
            self.xcrun.EXECUTABLE,
            '--sdk', _UNKNOWN_SDK,
            '--show-sdk-build-version',
        ]

        check_output.assert_called_with(command, stderr=shell.DEVNULL)
        self.assertIsNone(result)

    @utils.requires_module('unittest.mock')
    def test_sdk_build_version_known_sdk(self):
        with mock.patch('build_swift.shell.check_output') as check_output:
            check_output.return_value = '{}\n'.format(_KNOWN_SDK_BUILD_VERSION)

            result = self.xcrun.sdk_build_version(sdk=_KNOWN_SDK)

        command = [
            self.xcrun.EXECUTABLE,
            '--sdk', _KNOWN_SDK,
            '--show-sdk-build-version',
        ]

        check_output.assert_called_with(command, stderr=shell.DEVNULL)
        self.assertEqual(result, Version(_KNOWN_SDK_BUILD_VERSION))

    # -------------------------------------------------------------------------
    # sdk_platform_path

    @utils.requires_module('unittest.mock')
    def test_sdk_platform_path_unknown_sdk(self):
        with mock.patch('build_swift.shell.check_output') as check_output:
            check_output.side_effect = shell.CalledProcessError(-1, None)

            result = self.xcrun.sdk_platform_path(sdk=_UNKNOWN_SDK)

        command = [
            self.xcrun.EXECUTABLE,
            '--sdk', _UNKNOWN_SDK,
            '--show-sdk-platform-path',
        ]

        check_output.assert_called_with(command, stderr=shell.DEVNULL)
        self.assertIsNone(result)

    @utils.requires_module('unittest.mock')
    def test_sdk_platform_path_known_sdk(self):
        with mock.patch('build_swift.shell.check_output') as check_output:
            check_output.return_value = '{}\n'.format(_KNOWN_SDK_PLATFORM_PATH)

            result = self.xcrun.sdk_platform_path(sdk=_KNOWN_SDK)

        command = [
            self.xcrun.EXECUTABLE,
            '--sdk', _KNOWN_SDK,
            '--show-sdk-platform-path',
        ]

        check_output.assert_called_with(command, stderr=shell.DEVNULL)
        self.assertEqual(result, _KNOWN_SDK_PLATFORM_PATH)

    # -------------------------------------------------------------------------
    # sdk_platform_version

    @utils.requires_module('unittest.mock')
    def test_sdk_platform_version_unknown_sdk(self):
        with mock.patch('build_swift.shell.check_output') as check_output:
            check_output.side_effect = shell.CalledProcessError(-1, None)

            result = self.xcrun.sdk_platform_version(sdk=_UNKNOWN_SDK)

        command = [
            self.xcrun.EXECUTABLE,
            '--sdk', _UNKNOWN_SDK,
            '--show-sdk-platform-version',
        ]

        check_output.assert_called_with(command, stderr=shell.DEVNULL)
        self.assertIsNone(result)

    @utils.requires_module('unittest.mock')
    def test_sdk_platform_version_known_sdk(self):
        with mock.patch('build_swift.shell.check_output') as check_output:
            check_output.return_value = '{}\n'.format(
                _KNOWN_SDK_PLATFORM_VERSION)

            result = self.xcrun.sdk_platform_version(sdk=_KNOWN_SDK)

        command = [
            self.xcrun.EXECUTABLE,
            '--sdk', _KNOWN_SDK,
            '--show-sdk-platform-version',
        ]

        check_output.assert_called_with(command, stderr=shell.DEVNULL)
        self.assertEqual(result, Version(_KNOWN_SDK_PLATFORM_VERSION))
