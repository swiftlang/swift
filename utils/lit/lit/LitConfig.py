from __future__ import absolute_import
import inspect
import os
import sys

import lit.Test
import lit.formats
import lit.TestingConfig
import lit.util

# LitConfig must be a new style class for properties to work
class LitConfig(object):
    """LitConfig - Configuration data for a 'lit' test runner instance, shared
    across all tests.

    The LitConfig object is also used to communicate with client configuration
    files, it is always passed in as the global variable 'lit' so that
    configuration files can access common functionality and internal components
    easily.
    """

    def __init__(self, progname, path, quiet,
                 useValgrind, valgrindLeakCheck, valgrindArgs,
                 noExecute, debug, isWindows,
                 params, config_prefix = None,
                 maxIndividualTestTime = 0):
        # The name of the test runner.
        self.progname = progname
        # The items to add to the PATH environment variable.
        self.path = [str(p) for p in path]
        self.quiet = bool(quiet)
        self.useValgrind = bool(useValgrind)
        self.valgrindLeakCheck = bool(valgrindLeakCheck)
        self.valgrindUserArgs = list(valgrindArgs)
        self.noExecute = noExecute
        self.debug = debug
        self.isWindows = bool(isWindows)
        self.params = dict(params)
        self.bashPath = None

        # Configuration files to look for when discovering test suites.
        self.config_prefix = config_prefix or 'lit'
        self.config_name = '%s.cfg' % (self.config_prefix,)
        self.site_config_name = '%s.site.cfg' % (self.config_prefix,)
        self.local_config_name = '%s.local.cfg' % (self.config_prefix,)

        self.numErrors = 0
        self.numWarnings = 0

        self.valgrindArgs = []
        if self.useValgrind:
            self.valgrindArgs = ['valgrind', '-q', '--run-libc-freeres=no',
                                 '--tool=memcheck', '--trace-children=yes',
                                 '--error-exitcode=123']
            if self.valgrindLeakCheck:
                self.valgrindArgs.append('--leak-check=full')
            else:
                # The default is 'summary'.
                self.valgrindArgs.append('--leak-check=no')
            self.valgrindArgs.extend(self.valgrindUserArgs)

        self.maxIndividualTestTime = maxIndividualTestTime

    @property
    def maxIndividualTestTime(self):
        """
            Interface for getting maximum time to spend executing
            a single test
        """
        return self._maxIndividualTestTime

    @maxIndividualTestTime.setter
    def maxIndividualTestTime(self, value):
        """
            Interface for setting maximum time to spend executing
            a single test
        """
        self._maxIndividualTestTime = value
        if self.maxIndividualTestTime > 0:
            # The current implementation needs psutil to set
            # a timeout per test. Check it's available.
            # See lit.util.killProcessAndChildren()
            try:
                import psutil
            except ImportError:
                self.fatal("Setting a timeout per test requires the"
                           " Python psutil module but it could not be"
                           " found. Try installing it via pip or via"
                           " your operating system's package manager.")
        elif self.maxIndividualTestTime < 0:
            self.fatal('The timeout per test must be >= 0 seconds')

    def load_config(self, config, path):
        """load_config(config, path) - Load a config object from an alternate
        path."""
        if self.debug:
            self.note('load_config from %r' % path)
        config.load_from_path(path, self)
        return config

    def getBashPath(self):
        """getBashPath - Get the path to 'bash'"""
        if self.bashPath is not None:
            return self.bashPath

        self.bashPath = lit.util.which('bash', os.pathsep.join(self.path))
        if self.bashPath is None:
            self.bashPath = lit.util.which('bash')

        if self.bashPath is None:
            self.bashPath = ''

        return self.bashPath

    def getToolsPath(self, dir, paths, tools):
        if dir is not None and os.path.isabs(dir) and os.path.isdir(dir):
            if not lit.util.checkToolsPath(dir, tools):
                return None
        else:
            dir = lit.util.whichTools(tools, paths)

        # bash
        self.bashPath = lit.util.which('bash', dir)
        if self.bashPath is None:
            self.bashPath = ''

        return dir

    def _write_message(self, kind, message):
        # Get the file/line where this message was generated.
        f = inspect.currentframe()
        # Step out of _write_message, and then out of wrapper.
        f = f.f_back.f_back
        file,line,_,_,_ = inspect.getframeinfo(f)
        location = '%s:%d' % (os.path.basename(file), line)

        sys.stderr.write('%s: %s: %s: %s\n' % (self.progname, location,
                                               kind, message))

    def note(self, message):
        self._write_message('note', message)

    def warning(self, message):
        self._write_message('warning', message)
        self.numWarnings += 1

    def error(self, message):
        self._write_message('error', message)
        self.numErrors += 1

    def fatal(self, message):
        self._write_message('fatal', message)
        sys.exit(2)
