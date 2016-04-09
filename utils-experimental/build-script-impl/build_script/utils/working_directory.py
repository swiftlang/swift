# utils/cached_property.py --------------------------------------*- python -*-
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

# NOTE: NOT TO USE.
# TODO: Migrate all users to shell.pushd()
# TODO: MIGRATE doctests to shell.pushd()

import os


class WorkingDirectory(object):
    '''
    A context manager for changing the current working directory.

    >>> import os.path
    >>> import tempfile
    >>> tmpdir = os.path.realpath(tempfile.mkdtemp())
    >>> basedir = os.getcwd()
    >>> with WorkingDirectory(tmpdir):
    ...    assert os.getcwd() == tmpdir
    ...
    >>> assert os.getcwd() == basedir
    >>> with WorkingDirectory(tmpdir):
    ...    assert os.getcwd() == tmpdir
    ...    os.makedirs('foo')
    ...    with WorkingDirectory('foo'):
    ...        assert.os.getcwd == os.path.join(tempdir, 'foo')
    ...    assert os.getcwd() == tmp
    ...
    >>> assert os.getcwd == basedir
    >>> os.rmdir(tmpdir)
    '''

    def __init__(self, new_cwd):
        self.new_cwd = new_cwd

    def __enter__(self):
        self.old_cwd = os.getcwd()
        os.chdir(self.new_cwd)

    def __exit__(self, type, value, traceback):
        os.chdir(self.old_cwd)
