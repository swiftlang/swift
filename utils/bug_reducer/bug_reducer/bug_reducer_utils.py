
import os
import subprocess

DRY_RUN = False
SQUELCH_STDERR = True

def br_call(args, dry_run=DRY_RUN):
    if dry_run:
        print('DRY RUN BRCALL: ' + ' '.join(args))
        return 0
    if SQUELCH_STDERR:
        return subprocess.call(args, stdout=open('/dev/null'),
                               stderr=open('/dev/null'))
    else:
        # Useful for debugging.
        return subprocess.call(args, stdout=open('/dev/null'))


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
        return self._get_tool('sil-func-extractor')

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


class SILToolInvokerConfig(object):

    def __init__(self, args):
        self.module_cache = args.module_cache
        self.sdk = args.sdk
        self.target = args.target
        self.resource_dir = maybe_abspath(args.resource_dir)
        self.work_dir = maybe_abspath(args.work_dir)
        self.module_name = args.module_name


class SILToolInvoker(object):

    def __init__(self, config, extra_args=None):
        self.config = config
        self.extra_args = extra_args

    @property
    def base_args(self):
        x = [self.tool]
        if self.config.sdk is not None:
            x.append("-sdk=%s" % self.config.sdk)
        if self.config.target is not None:
            x.append("-target=%s" % self.config.target)
        if self.config.resource_dir is not None:
            x.append("-resource-dir=%s" % self.config.resource_dir)
        if self.config.module_cache is not None:
            x.append("-module-cache-path=%s" % self.config.module_cache)
        if self.config.module_name is not None:
            x.append("-module-name=%s" % self.config.module_name)
        return x

    @property
    def tool(self):
        raise RuntimeError('Abstract Method')


class SILConstantInputToolInvoker(SILToolInvoker):

    def __init__(self, config, tools, initial_input_file, extra_args):
        SILToolInvoker.__init__(self, config, extra_args)
        self.tools = tools

        # Start by creating our workdir if necessary
        subprocess.check_call(["mkdir", "-p", self.config.work_dir])

        # Then copy our input file into the work dir
        base_input_file = os.path.basename(initial_input_file)
        (base, ext) = os.path.splitext(base_input_file)
        self.base_input_file_stem = base
        self.base_input_file_ext = ".sib"

        # First emit an initial *.sib file. This ensures no matter if we have a
        # *.swiftmodule, *.sil, or *.sib file, we are always using *.sib.
        self.input_file = initial_input_file

    def _invoke(self, input_file, passes, output_filename):
        raise RuntimeError('Abstract method')

    @property
    def base_args(self):
        base_args = SILToolInvoker.base_args.fget(self)
        base_args.append('-emit-sib')
        return base_args

    def get_suffixed_filename(self, suffix):
        basename = self.base_input_file_stem + '_' + suffix
        basename += self.base_input_file_ext
        return os.path.join(self.config.work_dir, basename)


class SILOptInvoker(SILConstantInputToolInvoker):

    def __init__(self, config, tools, input_file, extra_args):
        SILConstantInputToolInvoker.__init__(self, config, tools, input_file,
                                             extra_args)
        self.input_file = self.get_suffixed_filename('initial')
        self._invoke(input_file, [], self.input_file)

    @property
    def tool(self):
        return self.tools.sil_opt

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


class SILFuncExtractorInvoker(SILConstantInputToolInvoker):

    def __init__(self, config, tools, input_file):
        SILConstantInputToolInvoker.__init__(self, config, tools, input_file,
                                             [])

    @property
    def tool(self):
        return self.tools.sil_func_extractor

    def _cmdline(self, input_file, funclist_path, output_file='-', invert=False):
        assert(isinstance(funclist_path, str))
        base_args = self.base_args
        base_args.extend([input_file, '-o', output_file,
                          '-func-file=%s' % funclist_path])
        if invert:
            base_args.append('-invert')
        return base_args

    def _invoke(self, input_file, funclist_path, output_filename, invert=False):
        assert(isinstance(funclist_path, str))
        cmdline = self._cmdline(input_file, funclist_path, output_filename,
                                invert)
        return br_call(cmdline)

    def invoke_with_functions(self, funclist_path, output_filename, invert=False):
        assert(isinstance(funclist_path, str))
        return self._invoke(self.input_file, funclist_path, output_filename,
                            invert)

    def cmdline_with_functions(self, funclist_path, invert=False):
        assert(isinstance(funclist_path, str))
        return self._cmdline(self.input_file, funclist_path, invert)


class SILNMInvoker(SILToolInvoker):

    def __init__(self, config, tools):
        self.tools = tools
        SILToolInvoker.__init__(self, config)

    @property
    def tool(self):
        return self.tools.sil_nm

    def get_symbols(self, input_file):
        cmdline = self.base_args
        cmdline.append(input_file)
        output = subprocess.check_output(cmdline)
        for l in output.split("\n")[:-1]:
            t = tuple(l.split(" "))
            assert(len(t) == 2)
            yield t
