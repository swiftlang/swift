# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


from __future__ import absolute_import, unicode_literals

import collections
import sys
import unittest

from build_swift import shell

import six
from six import StringIO

from .. import utils


try:
    # Python 3.4
    from pathlib import Path
except ImportError:
    pass

try:
    # Python 3.3
    from unittest import mock
    from unittest.mock import patch, mock_open, MagicMock
except ImportError:
    mock, mock_open = None, None

    class MagicMock(object):
        def __init__(self, *args, **kwargs):
            pass

    def _id(obj):
        return obj

    def patch(*args, **kwargs):
        return _id


# -----------------------------------------------------------------------------
# Constants

_OPEN_NAME = '{}.open'.format(six.moves.builtins.__name__)


# -----------------------------------------------------------------------------
# Test Cases

class TestHelpers(unittest.TestCase):
    """Unit tests for the helper functions defined in the build_swift.shell
    module.
    """

    # -------------------------------------------------------------------------
    # _flatmap

    def test_flatmap(self):
        def duplicate(x):
            return [x, x]

        result = shell._flatmap(duplicate, [1, 2, 3])

        self.assertIsInstance(result, collections.Iterable)
        self.assertEqual(list(result), [1, 1, 2, 2, 3, 3])

    # -------------------------------------------------------------------------
    # _convert_pathlib_path

    @utils.requires_module('unittest.mock')
    @utils.requires_module('pathlib')
    @patch('build_swift.shell.Path', None)
    def test_convert_pathlib_path_pathlib_not_imported(self):
        path = Path('/path/to/file.txt')

        self.assertEqual(shell._convert_pathlib_path(path), path)

    @utils.requires_module('pathlib')
    def test_convert_pathlib_path(self):
        path = Path('/path/to/file.txt')

        self.assertEqual(shell._convert_pathlib_path(''), '')

        self.assertEqual(
            shell._convert_pathlib_path(path),
            six.text_type(path))

    # -------------------------------------------------------------------------
    # _get_stream_file

    def test_get_stream_file(self):
        self.assertEqual(shell._get_stream_file(shell.PIPE), sys.stdout)
        self.assertEqual(shell._get_stream_file(shell.STDOUT), sys.stdout)

        self.assertEqual(shell._get_stream_file(sys.stdout), sys.stdout)
        self.assertEqual(shell._get_stream_file(sys.stderr), sys.stderr)

    def test_get_stream_file_raises_devnull(self):
        with self.assertRaises(ValueError):
            shell._get_stream_file(shell.DEVNULL)

    # -------------------------------------------------------------------------
    # _echo_command

    @utils.requires_module('unittest.mock')
    def test_echo_command(self):
        test_command = ['sudo', 'rm', '-rf', '/tmp/*']
        mock_stream = MagicMock()

        shell._echo_command(test_command, mock_stream)

        mock_stream.write.assert_called_with(
            '>>> {}\n'.format(shell.quote(test_command)))
        assert(mock_stream.flush.called)

    @utils.requires_module('unittest.mock')
    def test_echo_command_custom_prefix(self):
        mock_stream = MagicMock()

        shell._echo_command('ls', mock_stream, prefix='$ ')

        mock_stream.write.assert_called_with('$ ls\n')
        assert(mock_stream.flush.called)

    # -------------------------------------------------------------------------
    # _normalize_args

    def test_normalize_args_splits_basestring(self):
        command = 'rm -rf /Applications/Xcode.app'

        self.assertEqual(
            shell._normalize_args(command),
            ['rm', '-rf', '/Applications/Xcode.app'])

    def test_normalize_args_list_str(self):
        command = ['rm', '-rf', '/Applications/Xcode.app']

        self.assertEqual(shell._normalize_args(command), command)

    def test_normalize_args_converts_wrappers(self):
        sudo = shell.wraps('sudo')
        rm = shell.wraps('rm')

        command = [sudo, rm, '-rf', '/Applications/Xcode.app']

        self.assertEqual(
            shell._normalize_args(command),
            ['sudo', 'rm', '-rf', '/Applications/Xcode.app'])

    def test_normalize_args_converts_complex_wrapper_commands(self):
        sudo_rm_rf = shell.wraps('sudo rm -rf')

        command = [sudo_rm_rf, '/Applications/Xcode.app']

        self.assertEqual(
            shell._normalize_args(command),
            ['sudo', 'rm', '-rf', '/Applications/Xcode.app'])

    @utils.requires_module('pathlib')
    def test_normalize_args_accepts_single_wrapper_arg(self):
        rm_xcode = shell.wraps(['rm', '-rf', Path('/Applications/Xcode.app')])

        self.assertEqual(
            shell._normalize_args(rm_xcode),
            ['rm', '-rf', '/Applications/Xcode.app'])

    @utils.requires_module('pathlib')
    def test_normalize_args_converts_pathlib_path(self):
        command = ['rm', '-rf', Path('/Applications/Xcode.app')]

        self.assertEqual(
            shell._normalize_args(command),
            ['rm', '-rf', '/Applications/Xcode.app'])

    @utils.requires_module('pathlib')
    def test_normalize_args_converts_pathlib_path_in_wrapper_commands(self):
        rm_xcode = shell.wraps(['rm', '-rf', Path('/Applications/Xcode.app')])

        self.assertEqual(
            shell._normalize_args([rm_xcode]),
            ['rm', '-rf', '/Applications/Xcode.app'])


class TestDecorators(unittest.TestCase):
    """Unit tests for the decorators defined in the build_swift.shell module
    used to backport or add functionality to the subprocess wrappers.
    """

    # -------------------------------------------------------------------------
    # _backport_devnull

    @utils.requires_module('unittest.mock')
    @patch(_OPEN_NAME, new_callable=mock_open)
    @patch('build_swift.shell._PY_VERSION', (3, 2))
    def test_backport_devnull_stdout_kwarg(self, mock_open):
        mock_file = MagicMock()
        mock_open.return_value.__enter__.return_value = mock_file

        @shell._backport_devnull
        def func(command, **kwargs):
            self.assertEqual(kwargs['stdout'], mock_file)

        func('', stdout=shell.DEVNULL)
        assert(mock_open.return_value.__enter__.called)
        assert(mock_open.return_value.__exit__.called)

    @utils.requires_module('unittest.mock')
    @patch(_OPEN_NAME, new_callable=mock_open)
    @patch('build_swift.shell._PY_VERSION', (3, 2))
    def test_backport_devnull_stderr_kwarg(self, mock_open):
        mock_file = MagicMock()
        mock_open.return_value.__enter__.return_value = mock_file

        @shell._backport_devnull
        def func(command, **kwargs):
            self.assertEqual(kwargs['stderr'], mock_file)

        func('', stderr=shell.DEVNULL)
        assert(mock_open.return_value.__enter__.called)
        assert(mock_open.return_value.__exit__.called)

    @utils.requires_module('unittest.mock')
    @patch(_OPEN_NAME, new_callable=mock_open)
    def test_backport_devnull_does_not_open(self, mock_open):
        @shell._backport_devnull
        def func(command):
            pass

        func('')
        mock_open.return_value.__enter__.assert_not_called()
        mock_open.return_value.__exit__.assert_not_called()

    @utils.requires_module('unittest.mock')
    @patch('build_swift.shell._PY_VERSION', (3, 3))
    def test_backport_devnull_noop_starting_with_python_3_3(self):
        def func():
            pass

        self.assertEqual(shell._backport_devnull(func), func)

    # -------------------------------------------------------------------------
    # _normalize_command

    def test_normalize_command_basestring_command_noop(self):
        test_command = 'touch test.txt'

        @shell._normalize_command
        def func(command):
            self.assertEqual(command, test_command)

        func(test_command)

    @utils.requires_module('unittest.mock')
    @patch('build_swift.shell._normalize_args')
    def test_normalize_command(self, mock_normalize_args):
        test_command = ['rm', '-rf', '/tmp/*']

        @shell._normalize_command
        def func(command):
            pass

        func(test_command)
        mock_normalize_args.assert_called_with(test_command)

    # -------------------------------------------------------------------------
    # _add_echo_kwarg

    @utils.requires_module('unittest.mock')
    @patch('build_swift.shell._echo_command')
    def test_add_echo_kwarg_calls_echo_command(self, mock_echo_command):
        test_command = ['rm', '-rf', '/tmp/*']

        @shell._add_echo_kwarg
        def func(command, **kwargs):
            pass

        mock_stream = mock.mock_open()

        func(test_command, echo=True, stdout=mock_stream)
        mock_echo_command.assert_called_with(test_command, mock_stream)

    @utils.requires_module('unittest.mock')
    @patch('build_swift.shell._echo_command')
    def test_add_echo_kwarg_noop_echo_false(self, mock_echo_command):
        test_command = ['rm', '-rf', '/tmp/*']

        @shell._add_echo_kwarg
        def func(command):
            pass

        func(test_command)
        func(test_command, echo=False)
        mock_echo_command.assert_not_called()


class TestPublicFunctions(unittest.TestCase):
    """Unit tests for the public functions defined in the build_swift.shell
    module.
    """

    # -------------------------------------------------------------------------
    # quote

    def test_quote_string(self):
        self.assertEqual(
            shell.quote('/Applications/App Store.app'),
            "'/Applications/App Store.app'")

    def test_quote_iterable(self):
        self.assertEqual(
            shell.quote(['rm', '-rf', '~/Documents/My Homework']),
            "rm -rf '~/Documents/My Homework'")

    # -------------------------------------------------------------------------
    # rerun_as_root

    def test_rerun_as_root(self):
        pass


class TestSubprocessWrappers(unittest.TestCase):
    """Unit tests for the subprocess wrappers defined in the build_swift.shell
    module.
    """

    # -------------------------------------------------------------------------
    # Popen

    # NOTE: Testing the Popen class is harder than it might appear. We're not
    # able to mock out the subprocess.Popen superclass as one might initially
    # expect. Rather that shell.Popen class object already exists and inherts
    # from subprocess.Popen, thus mocking it out does not change the behavior.
    # Ultimately this class is merely a wrapper that uses already tested
    # decorators to add functionality so testing here might not provide any
    # benefit.

    # -------------------------------------------------------------------------
    # call

    @utils.requires_module('unittest.mock')
    @patch('subprocess.call')
    def test_call(self, mock_call):
        shell.call('ls')

        mock_call.assert_called_with('ls')

    # -------------------------------------------------------------------------
    # check_call

    @utils.requires_module('unittest.mock')
    @patch('subprocess.check_call')
    def test_check_call(self, mock_check_call):
        shell.check_call('ls')

        mock_check_call.assert_called_with('ls')

    # -------------------------------------------------------------------------
    # check_output

    @utils.requires_module('unittest.mock')
    @patch('subprocess.check_output')
    def test_check_output(self, mock_check_output):
        # Before Python 3 the subprocess.check_output function returned bytes.
        if six.PY3:
            mock_check_output.return_value = ''
        else:
            mock_check_output.return_value = b''

        output = shell.check_output('ls')

        # We always expect str (utf-8) output
        self.assertIsInstance(output, six.text_type)

        if six.PY3:
            mock_check_output.assert_called_with('ls', encoding='utf-8')
        else:
            mock_check_output.assert_called_with('ls')


class TestShellUtilities(unittest.TestCase):
    """Unit tests for the shell utility wrappers defined in the
    build_swift.shell module.
    """

    # -------------------------------------------------------------------------
    # copy

    @utils.requires_module('unittest.mock')
    @patch('os.path.isfile', MagicMock(return_value=True))
    @patch('shutil.copyfile', MagicMock())
    @patch('build_swift.shell._convert_pathlib_path')
    def test_copy_converts_pathlib_paths(self, mock_convert):
        source = Path('/source/path')
        dest = Path('/dest/path')

        shell.copy(source, dest)

        mock_convert.assert_has_calls([
            mock.call(source),
            mock.call(dest),
        ])

    @utils.requires_module('unittest.mock')
    @patch('os.path.isfile', MagicMock(return_value=True))
    @patch('shutil.copyfile')
    def test_copy_files(self, mock_copyfile):
        source = '/source/path'
        dest = '/dest/path'

        shell.copy(source, dest)

        mock_copyfile.assert_called_with(source, dest)

    @utils.requires_module('unittest.mock')
    @patch('os.path.isfile', MagicMock(return_value=False))
    @patch('os.path.isdir', MagicMock(return_value=True))
    @patch('shutil.copytree')
    def test_copy_directories(self, mock_copytree):
        source = '/source/path'
        dest = '/dest/path'

        shell.copy(source, dest)

        mock_copytree.assert_called_with(source, dest)

    @utils.requires_module('unittest.mock')
    @patch('os.path.isfile', MagicMock(return_value=True))
    @patch('shutil.copyfile', MagicMock())
    @patch('sys.stdout', new_callable=StringIO)
    def test_copy_echos_fake_cp_file_command(self, mock_stdout):
        source = '/source/path'
        dest = '/dest/path'

        shell.copy(source, dest, echo=True)

        self.assertEqual(
            mock_stdout.getvalue(),
            '>>> cp {} {}\n'.format(source, dest))

    @utils.requires_module('unittest.mock')
    @patch('os.path.isfile', MagicMock(return_value=False))
    @patch('os.path.isdir', MagicMock(return_value=True))
    @patch('shutil.copytree', MagicMock())
    @patch('sys.stdout', new_callable=StringIO)
    def test_copy_echos_fake_cp_directory_command(self, mock_stdout):
        source = '/source/path'
        dest = '/dest/path'

        shell.copy(source, dest, echo=True)

        self.assertEqual(
            mock_stdout.getvalue(),
            '>>> cp -R {} {}\n'.format(source, dest))

    # -------------------------------------------------------------------------
    # pushd

    @utils.requires_module('unittest.mock')
    @utils.requires_module('pathlib')
    @patch('os.getcwd', MagicMock(return_value='/start/path'))
    @patch('build_swift.shell._convert_pathlib_path')
    def test_pushd_converts_pathlib_path(self, mock_convert):
        path = Path('/other/path')
        mock_convert.return_value = six.text_type(path)

        shell.pushd(path)

        mock_convert.assert_called_with(path)

    @utils.requires_module('unittest.mock')
    @patch('os.getcwd', MagicMock(return_value='/start/path'))
    @patch('os.chdir')
    def test_pushd_restores_cwd(self, mock_chdir):
        with shell.pushd('/other/path'):
            mock_chdir.assert_called_with('/other/path')

        mock_chdir.assert_called_with('/start/path')

    @utils.requires_module('unittest.mock')
    @patch('os.getcwd', MagicMock(return_value='/start/path'))
    @patch('os.chdir', MagicMock())
    @patch('sys.stdout', new_callable=StringIO)
    def test_pushd_echos_fake_pushd_popd_commands(self, mock_stdout):
        with shell.pushd('/other/path', echo=True):
            pass

        self.assertEqual(mock_stdout.getvalue().splitlines(), [
            '>>> pushd /other/path',
            '>>> popd'
        ])

    # -------------------------------------------------------------------------
    # makedirs

    @utils.requires_module('unittest.mock')
    @utils.requires_module('pathlib')
    @patch('os.path.exists', MagicMock(return_value=False))
    @patch('os.makedirs', MagicMock())
    @patch('build_swift.shell._convert_pathlib_path')
    def test_makedirs_converts_pathlib_path(self, mock_convert):
        path = Path('/some/directory')

        shell.makedirs(path)

        mock_convert.assert_called_with(path)

    @utils.requires_module('unittest.mock')
    @patch('os.path.exists', MagicMock(return_value=True))
    @patch('os.makedirs')
    def test_makedirs_noop_path_exists(self, mock_makedirs):
        shell.makedirs('/some/directory')

        mock_makedirs.assert_not_called()

    @utils.requires_module('unittest.mock')
    @patch('os.path.exists', MagicMock(return_value=False))
    @patch('os.makedirs')
    def test_makedirs_creates_path(self, mock_makedirs):
        path = '/some/directory'

        shell.makedirs(path)

        mock_makedirs.assert_called_with(path)

    @utils.requires_module('unittest.mock')
    @patch('os.path.exists', MagicMock(return_value=False))
    @patch('os.makedirs', MagicMock())
    @patch('sys.stdout', new_callable=StringIO)
    def test_makedirs_echos_fake_mkdir_command(self, mock_stdout):
        path = '/some/directory'

        shell.makedirs(path, echo=True)

        self.assertEqual(
            mock_stdout.getvalue(),
            '>>> mkdir -p {}\n'.format(path))

    # -------------------------------------------------------------------------
    # move

    @utils.requires_module('unittest.mock')
    @utils.requires_module('pathlib')
    @patch('shutil.move', MagicMock())
    @patch('build_swift.shell._convert_pathlib_path')
    def test_move_converts_pathlib_paths(self, mock_convert):
        source = Path('/source/path')
        dest = Path('/dest/path')

        shell.move(source, dest)

        mock_convert.assert_has_calls([
            mock.call(source),
            mock.call(dest),
        ])

    @utils.requires_module('unittest.mock')
    @patch('shutil.move')
    def test_move(self, mock_move):
        source = '/source/path'
        dest = '/dest/path'

        shell.move(source, dest)

        mock_move.assert_called_with(source, dest)

    @utils.requires_module('unittest.mock')
    @patch('shutil.move', MagicMock())
    @patch('sys.stdout', new_callable=StringIO)
    def test_move_echos_fake_mv_command(self, mock_stdout):
        source = '/source/path'
        dest = '/dest/path'

        shell.move(source, dest, echo=True)

        self.assertEqual(
            mock_stdout.getvalue(),
            '>>> mv {} {}\n'.format(source, dest))

    # -------------------------------------------------------------------------
    # remove

    @utils.requires_module('unittest.mock')
    @utils.requires_module('pathlib')
    @patch('os.path.isfile', MagicMock(return_value=True))
    @patch('os.remove', MagicMock())
    @patch('build_swift.shell._convert_pathlib_path')
    def test_remove_converts_pathlib_paths(self, mock_convert):
        path = Path('/path/to/remove')

        shell.remove(path)

        mock_convert.assert_called_with(path)

    @utils.requires_module('unittest.mock')
    @patch('os.path.isfile', MagicMock(return_value=True))
    @patch('os.remove')
    def test_remove_files(self, mock_remove):
        path = '/path/to/remove'

        shell.remove(path)

        mock_remove.assert_called_with(path)

    @utils.requires_module('unittest.mock')
    @patch('os.path.isfile', MagicMock(return_value=False))
    @patch('os.path.isdir', MagicMock(return_value=True))
    @patch('shutil.rmtree')
    def test_remove_directories(self, mock_rmtree):
        path = '/path/to/remove'

        shell.remove(path)

        mock_rmtree.assert_called_with(path, ignore_errors=True)

    @utils.requires_module('unittest.mock')
    @patch('os.path.isfile', MagicMock(return_value=True))
    @patch('os.remove', MagicMock())
    @patch('sys.stdout', new_callable=StringIO)
    def test_remove_echos_fake_rm_file_command(self, mock_stdout):
        path = '/path/to/remove'

        shell.remove(path, echo=True)

        self.assertEqual(
            mock_stdout.getvalue(),
            '>>> rm {}\n'.format(path))

    @utils.requires_module('unittest.mock')
    @patch('os.path.isfile', MagicMock(return_value=False))
    @patch('os.path.isdir', MagicMock(return_value=True))
    @patch('shutil.rmtree', MagicMock())
    @patch('sys.stdout', new_callable=StringIO)
    def test_remove_echos_fake_rm_directory_command(self, mock_stdout):
        path = '/path/to/remove'

        shell.remove(path, echo=True)

        self.assertEqual(
            mock_stdout.getvalue(),
            '>>> rm -rf {}\n'.format(path))

    # -------------------------------------------------------------------------
    # symlink

    @utils.requires_module('unittest.mock')
    @utils.requires_module('pathlib')
    @patch('os.symlink', MagicMock())
    @patch('build_swift.shell._convert_pathlib_path')
    def test_symlink_converts_pathlib_paths(self, mock_convert):
        source = Path('/source/path')
        dest = Path('/dest/path')

        shell.symlink(source, dest)

        mock_convert.assert_has_calls([
            mock.call(source),
            mock.call(dest),
        ])

    @utils.requires_module('unittest.mock')
    @patch('os.symlink')
    def test_symlink(self, mock_symlink):
        source = '/source/path'
        dest = '/dest/path'

        shell.symlink(source, dest)

        mock_symlink.assert_called_with(source, dest)

    @utils.requires_module('unittest.mock')
    @patch('os.symlink', MagicMock())
    @patch('sys.stdout', new_callable=StringIO)
    def test_symlink_echos_fake_ln_command(self, mock_stdout):
        source = '/source/path'
        dest = '/dest/path'

        shell.symlink(source, dest, echo=True)

        self.assertEqual(
            mock_stdout.getvalue(),
            '>>> ln -s {} {}\n'.format(source, dest))

    # -------------------------------------------------------------------------
    # which

    # NOTE: We currently have a polyfill for the shutil.which function. This
    # will be swapped out for the real-deal as soon as we convert to Python 3,
    # which should be in the near future. We could also use a backport package
    # from pypi, but we rely on the shell module working in scripting that does
    # not use a virtual environment at the moment. Until we either adopt
    # Python 3 by default _or_ enforce virtual environments for all our scripts
    # we are stuck with the polyfill.

    def test_which(self):
        pass


class TestAbstractWrapper(unittest.TestCase):
    """Unit tests for the AbstractWrapper class defined in the build_swift.shell
    module.
    """

    def test_cannot_be_instantiated(self):
        with self.assertRaises(TypeError):
            shell.AbstractWrapper()


class TestCommandWrapper(unittest.TestCase):
    """Unit tests for the CommandWrapper class defined in the build_swift.shell
    module.
    """

    # -------------------------------------------------------------------------
    # wraps

    def test_wraps(self):
        sudo = shell.wraps('sudo')

        self.assertIsInstance(sudo, shell.CommandWrapper)
        self.assertEqual(sudo.command, ['sudo'])

    # -------------------------------------------------------------------------

    @utils.requires_module('pathlib')
    def test_command_normalized(self):
        wrapper = shell.CommandWrapper(['ls', '-al', Path('/tmp')])

        self.assertEqual(wrapper._command, ['ls', '-al', '/tmp'])

    def test_command_property(self):
        git = shell.CommandWrapper('git')

        self.assertEqual(git.command, ['git'])

    @utils.requires_module('unittest.mock')
    def test_callable(self):
        ls = shell.CommandWrapper('ls')

        with patch.object(ls, 'check_call') as mock_check_call:
            ls('-al')

        mock_check_call.assert_called_with('-al')

    # -------------------------------------------------------------------------
    # Subprocess Wrappers

    @utils.requires_module('unittest.mock')
    @patch('build_swift.shell.Popen')
    def test_Popen(self, mock_popen):
        ls = shell.CommandWrapper('ls')

        ls.Popen('-al')

        mock_popen.assert_called_with(['ls', '-al'])

    @utils.requires_module('unittest.mock')
    @patch('build_swift.shell.call')
    def test_call(self, mock_call):
        ls = shell.CommandWrapper('ls')

        ls.call('-al')

        mock_call.assert_called_with(['ls', '-al'])

    @utils.requires_module('unittest.mock')
    @patch('build_swift.shell.check_call')
    def test_check_call(self, mock_check_call):
        ls = shell.CommandWrapper('ls')

        ls.check_call('-al')

        mock_check_call.assert_called_with(['ls', '-al'])

    @utils.requires_module('unittest.mock')
    @patch('build_swift.shell.check_output')
    def test_check_output(self, mock_check_output):
        ls = shell.CommandWrapper('ls')

        ls.check_output('-al')

        mock_check_output.assert_called_with(['ls', '-al'])


class TestExecutableWrapper(unittest.TestCase):
    """Unit tests for the ExecutableWrapper class defined in the
    build_swift.shell module.
    """

    def test_raises_without_executable(self):
        class MyWrapper(shell.ExecutableWrapper):
            pass

        with self.assertRaises(AttributeError):
            MyWrapper()

    def test_raises_complex_executable(self):
        class MyWrapper(shell.ExecutableWrapper):
            EXECUTABLE = ['xcrun', 'swiftc']

        with self.assertRaises(AttributeError):
            MyWrapper()

    @utils.requires_module('pathlib')
    def test_converts_pathlib_path(self):
        class MyWrapper(shell.ExecutableWrapper):
            EXECUTABLE = Path('/usr/local/bin/xbs')

        wrapper = MyWrapper()

        self.assertEqual(wrapper.EXECUTABLE, '/usr/local/bin/xbs')

    def test_command_property(self):
        class MyWrapper(shell.ExecutableWrapper):
            EXECUTABLE = 'test'

        wrapper = MyWrapper()

        self.assertEqual(wrapper.command, ['test'])

    @utils.requires_module('unittest.mock')
    @patch('build_swift.shell.which')
    def test_path_property(self, mock_which):
        class MyWrapper(shell.ExecutableWrapper):
            EXECUTABLE = 'test'

        wrapper = MyWrapper()
        wrapper.path

        mock_which.assert_called_with('test')
