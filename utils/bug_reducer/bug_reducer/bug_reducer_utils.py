
import json
import os
import subprocess

DRY_RUN = False


def br_call(args):
    if DRY_RUN:
        print('DRY RUN: ' + ' '.join(args))
        return 0
    return subprocess.call(args, stdout=open('/dev/null'), stderr=open('/dev/null'))


class SwiftTools(object):
    """A utility class that enables users to easily find sil-tools without needing
to constantly reform paths to the build directory. Also provides safety by
asserting if one of the tools does not exist at the specified path"""

    def __init__(self, swift_build_dir):
        self.swift_build_dir = swift_build_dir

    def _get_tool(self, name):
        path = os.path.join(self.swift_build_dir, 'bin', name)
        if not os.access(path, os.F_OK):
            error_msg = "Error! {} does not exist at: {}".format(name, path)
            raise RuntimeError(error_msg)
        return path

    @property
    def sil_nm(self):
        """Return the path to sil-nm in the specified swift build directory. Throws a
runtime error if the tool does not exist"""
        return self._get_tool('sil-nm')

    @property
    def swiftc(self):
        """Return the path to swiftc in the specified swift build directory. Throws a
runtime error if the tool does not exist"""
        return self._get_tool('swiftc')

    @property
    def sil_opt(self):
        """Return the path to sil-opt in the specified swift build directory. Throws a
runtime error if the tool does not exist"""
        return self._get_tool('sil-opt')

    @property
    def sil_func_extractor(self):
        """Return the path to sil-func-extractor in the specified swift build
directory. Throws a runtime error if the tool does not exist."""
        return self._get_tool('sil-opt')

    @property
    def sil_passpipeline_dumper(self):
        """Return the path to sil-passpipeline-dumper in the specified swift build
directory. Throws a runtime error if the tool does not exist

        """
        return self._get_tool('sil-passpipeline-dumper')


def maybe_abspath(x):
    if x is None:
        return x
    return os.path.abspath(x)


class SILOptInvoker(object):

    def __init__(self, args, tools, early_passes, extra_args):
        self.tools = tools
        self.module_cache = args.module_cache
        self.sdk = args.sdk
        self.target = args.target
        self.resource_dir = maybe_abspath(args.resource_dir)
        self.work_dir = maybe_abspath(args.work_dir)
        self.module_name = args.module_name
        self.extra_args = extra_args

        # Start by creating our workdir if necessary
        subprocess.check_call(["mkdir", "-p", self.work_dir])

        # Then copy our input file into the work dir
        base_input_file = os.path.basename(args.input_file)
        (base, ext) = os.path.splitext(base_input_file)
        self.base_input_file_stem = base
        self.base_input_file_ext = ".sib"

        # First emit an initial *.sib file. This ensures no matter if we have a
        # *.swiftmodule, *.sil, or *.sib file, we are always using *.sib.
        self._invoke(args.input_file, [],
                     self.get_suffixed_filename('initial'))
        self.input_file = self.get_suffixed_filename('initial+early')

        # Finally, run the initial sil-opt invocation running the
        # early-passes. We will not run them again.
        self._invoke(self.get_suffixed_filename('initial'),
                     early_passes,
                     self.input_file)

    def get_suffixed_filename(self, suffix):
        basename = self.base_input_file_stem + '_' + suffix
        basename += self.base_input_file_ext
        return os.path.join(self.work_dir, basename)

    @property
    def base_args(self):
        x = [self.tools.sil_opt, "-emit-sib"]
        if self.sdk is not None:
            x.append("-sdk=%s" % self.sdk)
        if self.target is not None:
            x.append("-target=%s" % self.target)
        if self.resource_dir is not None:
            x.append("-resource-dir=%s" % self.resource_dir)
        if self.module_cache is not None:
            x.append("-module-cache-path=%s" % self.module_cache)
        if self.module_name is not None:
            x.append("-module-name=%s" % self.module_name)
        return x

    def _cmdline(self, input_file, passes, output_file='-'):
        base_args = self.base_args
        base_args.extend([input_file, '-o', output_file])
        base_args.extend(self.extra_args)
        base_args.extend(passes)
        return base_args

    def _invoke(self, input_file, passes, output_filename):
        cmdline = self._cmdline(input_file, passes, output_filename)
        return br_call(cmdline)

    def invoke_with_passlist(self, passes, output_filename):
        return self._invoke(self.input_file, passes, output_filename)

    def cmdline_with_passlist(self, passes):
        return self._cmdline(self.input_file, passes)
