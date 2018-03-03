# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


from __future__ import absolute_import, unicode_literals

import os
import sys

from .utils import TestCase
from .. import shell

try:
    # Python 3
    from io import StringIO
except ImportError:
    from StringIO import StringIO


class TestCommandExecutor(TestCase):

    def test_settings_passthrough(self):
        sh = shell.CommandExecutor(env={'EDITOR': 'subl'})

        sh._fake_popen(['ls', '-al'])

    def test_add_action(self):
        sh = shell.CommandExecutor()
        self.assertEqual(len(sh._actions), 1)

        def callable(command):
            pass

        sh.add_action(callable)
        self.assertEqual(sh._actions[1], callable)

    def test_add_echo_action(self):
        sh = shell.CommandExecutor()

        sh.add_echo_action()
        action = sh._actions[1]
        self.assertIsInstance(action, shell._EchoAction)
        self.assertEqual(action.output_stream, sh._stdout)
        self.assertEqual(action.prefix, shell._EchoAction.DEFAULT_PREFIX)

        sh._actions.pop()

        stream = StringIO()

        sh.add_echo_action(output_stream=stream, prefix='$ ')
        action = sh._actions[1]
        self.assertIsInstance(action, shell._EchoAction)
        self.assertEqual(action.output_stream, stream)
        self.assertEqual(action.prefix, '$ ')

        sh._fake_popen(['true'])
        self.assertEqual(stream.getvalue(), '$ true\n')

    def test_popen(self):
        devnull = open(os.devnull, 'w')
        sh = shell.CommandExecutor(stdout=devnull, stderr=devnull)

        p = sh.popen([sys.executable, '--version'])
        self.assertEqual(p.returncode, 0)

        p = sh.popen([sys.executable, '-c', 'raise SysExit(1)'])
        self.assertEqual(p.returncode, 1)

    def test_call(self):
        devnull = open(os.devnull, 'w')
        sh = shell.CommandExecutor(stdout=devnull, stderr=devnull)

        returncode = sh.call([sys.executable, '--version'])
        self.assertEqual(returncode, 0)

        returncode = sh.call([sys.executable, '-c', 'raise SysExit(1)'])
        self.assertEqual(returncode, 1)

    def test_check_call(self):
        devnull = open(os.devnull, 'w')
        sh = shell.CommandExecutor(stdout=devnull, stderr=devnull)

        with self.assertNotRaises(shell.CalledProcessError):
            sh.check_call([sys.executable, '--version'])

        with self.assertRaises(shell.CalledProcessError):
            sh.check_call([sys.executable, '-c', 'raise SysExit(1)'])

    def test_check_output(self):
        devnull = open(os.devnull, 'w')
        sh = shell.CommandExecutor(stdout=devnull, stderr=devnull)

        output = sh.check_output([sys.executable, '--version'])
        self.assertRegexpMatches(r'^Python [0-9.]+$', output)

        with self.assertRaises(shell.CalledProcessError):
            sh.check_output([sys.executable, '-c', 'raise SysExit(1)'])

    def test_history(self):
        sh = shell.CommandExecutor()

        sh._fake_popen(['ls', '-al'])
        sh._fake_popen(['touch', 'text.txt'])
        sh._fake_popen(['python', '-m', 'unittest', 'discover'])

        history = sh.history()
        self.assertListEqual(history, [
            ['ls', '-al'],
            ['touch', 'text.txt'],
            ['python', '-m', 'unittest', 'discover'],
        ])

    def test_command_quoting(self):
        sh = shell.CommandExecutor()

        sh._fake_popen(['cd', '/Applications/Sample Program.app'])
        self.assertListEqual(sh.history(), [
            ['cd', "'/Applications/Sample Program.app'"],
        ])

    # TODO: Devise testing for shell command wrappers


class TestNullExecutor(TestCase):

    def test_commands_return_fake_popen(self):
        sh = shell.NullExecutor()

        p = sh.popen(['true'])

        self.assertIsInstance(p, shell._FakePopen)
