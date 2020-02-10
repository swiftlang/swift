#!/usr/bin/env python

from __future__ import print_function

import argparse
import errno
import itertools
import json
import multiprocessing
import os
import shutil
import subprocess
import sys
import traceback

BARE_INTERFACE_SEARCH_PATHS = [
    "usr/lib/swift",
    "System/iOSSupport/usr/lib/swift"
]
DEFAULT_FRAMEWORK_INTERFACE_SEARCH_PATHS = [
    "System/Library/Frameworks",
    "System/iOSSupport/System/Library/Frameworks"
]
STDLIB_NAME = 'Swift'

MONOTONIC_VERSION = 1


def create_parser():
    parser = argparse.ArgumentParser(
        description="Builds an SDK's swiftinterfaces into swiftmodules. "
                    "Always searches usr/lib/swift in addition to whichever "
                    "framework directories are passed on the command line.",
        prog=os.path.basename(__file__),
        usage='%(prog)s -o output/ [INTERFACE_SEARCH_DIRS]',
        epilog='Environment variables: SDKROOT, SWIFT_EXEC, '
               'SWIFT_FORCE_MODULE_LOADING')
    parser.add_argument('interface_framework_dirs', nargs='*',
                        metavar='INTERFACE_SEARCH_DIRS',
                        help='Relative paths to search for frameworks with '
                             'interfaces (default: System/Library/Frameworks)')
    parser.add_argument('-o', dest='output_dir',
                        help='Directory to which the output will be emitted '
                             '(required)')
    parser.add_argument('-j', dest='jobs', type=int,
                        help='The number of parallel jobs to execute '
                             '(default: # of cores)')
    parser.add_argument('-v', dest='verbose', action='store_true',
                        help='Print command invocations and progress info')
    parser.add_argument('-n', dest='dry_run', action='store_true',
                        help='Dry run: don\'t actually run anything')
    parser.add_argument('-sdk', default=os.getenv('SDKROOT'),
                        help='SDK to find frameworks and interfaces in '
                             '(default: $SDKROOT)')
    parser.add_argument('-F', dest='framework_dirs', metavar='DIR',
                        action='append', default=[],
                        help='Add additional framework search paths')
    parser.add_argument('-Fsystem', '-iframework',
                        dest='system_framework_dirs', metavar='DIR',
                        action='append', default=[],
                        help='Add additional system framework search paths')
    parser.add_argument('-Fsystem-iosmac',
                        dest='iosmac_system_framework_dirs', metavar='DIR',
                        action='append', default=[],
                        help='Add system framework search paths '
                             'for iOSMac only')
    parser.add_argument('-I', dest='include_dirs', metavar='DIR',
                        action='append', default=[],
                        help='Add additional header/module search paths')
    parser.add_argument('-module-cache-path',
                        help='Temporary directory to store intermediate info')
    parser.add_argument('-log-path',
                        help='Directory to write stdout/stderr output to')
    parser.add_argument('-skip-stdlib', action='store_true',
                        help='Don\'t build the standard library interface')
    parser.add_argument('-disable-modules-validate-system-headers',
                        action='store_true',
                        help='Disable modules verification for system headers')
    parser.add_argument('-xfails', metavar='PATH',
                        help='JSON file containing an array of the modules '
                             'expected to fail')
    parser.add_argument('-check-only', action='store_true',
                        help='Assume the resulting modules will be thrown '
                             'away (may be faster)')
    parser.add_argument('-ignore-non-stdlib-failures', action='store_true',
                        help='Treat all modules but the stdlib as XFAILed')
    parser.add_argument('-debug-crash-compiler', action='store_true',
                        help='Have the compiler crash (for testing purposes)')
    parser.add_argument('-machine-parseable-monotonic-version',
                        action='store_true',
                        help='For comparing versions of this tool')
    return parser


def fatal(msg):
    print(msg, file=sys.stderr)
    sys.exit(1)


def run_command(args, dry_run):
    if dry_run:
        return (0, "", "")
    proc = subprocess.Popen(args, stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE)
    try:
        out, err = proc.communicate()
        exitcode = proc.returncode
        return (exitcode, out, err)
    except KeyboardInterrupt:
        proc.terminate()
        raise


def make_dirs_if_needed(path, dry_run):
    if dry_run:
        return
    try:
        os.makedirs(path)
    except OSError as e:
        if e.errno != errno.EEXIST:
            raise


class NegatedSet:
    def __init__(self, contents):
        self._contents = frozenset(contents)

    def __contains__(self, item):
        return item not in self._contents


class ModuleFile:
    def __init__(self, name, path, is_expected_to_fail):
        self.name = name
        self.path = path
        self.is_expected_to_fail = is_expected_to_fail


def collect_slices(xfails, swiftmodule_dir):
    if not os.path.isdir(swiftmodule_dir):
        return
    module_name, extension = \
        os.path.splitext(os.path.basename(swiftmodule_dir))
    assert extension == ".swiftmodule"

    is_xfail = module_name in xfails
    for entry in os.listdir(swiftmodule_dir):
        _, extension = os.path.splitext(entry)
        if extension == ".swiftinterface":
            yield ModuleFile(module_name, os.path.join(swiftmodule_dir, entry),
                             is_xfail)


def collect_framework_modules(sdk, xfails, sdk_relative_framework_dirs):
    for sdk_relative_framework_dir in sdk_relative_framework_dirs:
        framework_dir = os.path.join(sdk, sdk_relative_framework_dir)
        if not os.access(framework_dir, os.R_OK):
            continue
        for entry in os.listdir(framework_dir):
            path_without_extension, extension = os.path.splitext(entry)
            if extension != ".framework":
                continue
            module_name = os.path.basename(path_without_extension)
            swiftmodule = os.path.join(framework_dir, entry, "Modules",
                                       module_name + ".swiftmodule")
            if os.access(swiftmodule, os.R_OK):
                for x in collect_slices(xfails, swiftmodule):
                    yield x


def collect_non_framework_modules(sdk, xfails, sdk_relative_search_dirs):
    for sdk_relative_search_dir in sdk_relative_search_dirs:
        search_dir = os.path.join(sdk, sdk_relative_search_dir)
        for dir_path, _, file_names in os.walk(search_dir, followlinks=True):
            if os.path.splitext(dir_path)[1] == ".swiftmodule":
                for x in collect_slices(xfails, dir_path):
                    yield x
            else:
                for interface in file_names:
                    module_name, extension = os.path.splitext(interface)
                    if extension == ".swiftinterface":
                        is_xfail = module_name in xfails
                        yield ModuleFile(module_name,
                                         os.path.join(dir_path, interface),
                                         is_xfail)


def should_retry_compilation(stderr):
    if "has been modified since the module file" in stderr:
        return True
    if "mismatched umbrella headers in submodule" in stderr:
        return True
    if "is out of date and needs to be rebuilt: signature mismatch" in stderr:
        return True
    if "current parser token 'include'" in stderr:
        return True
    if "current parser token 'import'" in stderr:
        return True
    return False


def run_with_module_cache_retry(command_args, module_cache_path, dry_run):
    """Hack: runs a command several times, clearing the module cache if we get
    an error about header files being modified during the run.

    This shouldn't be necessary (the cached PCM files should automatically be
    regenerated) but there seems to still be a bug in Clang that we haven't
    tracked down yet.
    """
    RETRIES = 3
    attempts_stderr = ""
    for r in range(RETRIES):
        status, stdout, stderr = run_command(command_args, dry_run)
        if status == 0:
            break
        if not should_retry_compilation(stderr):
            break
        if module_cache_path:
            shutil.rmtree(module_cache_path, ignore_errors=True)
        # If all retries fail, output information for each instance.
        attempts_stderr += (
            "\n*** Compilation attempt {}/{} failed with modules bugs. "
            "Error output:\n".format(r + 1, RETRIES))
        attempts_stderr += stderr
        stderr = attempts_stderr
    return (status, stdout, stderr)


def log_output_to_file(content, module_name, interface_base, label, log_path):
    if not log_path:
        return
    if not content:
        return
    make_dirs_if_needed(log_path, dry_run=False)
    log_name = module_name + "-" + interface_base + "-" + label + ".txt"
    with open(os.path.join(log_path, log_name), "w") as output_file:
        output_file.write(content)


def looks_like_iosmac(interface_base):
    return 'ios-macabi' in interface_base


def rename_interface_for_iosmac_if_needed(interface_base, module_path):
    """Hack: Both macOS and iOSMac use 'x86_64' as the short name for a module
    interface file, and while we want to move away from this it's something we
    need to handle in the short term. Manually rename these to the full form of
    the target-specific module when we're obviously on macOS or iOSMac.
    """
    if interface_base != 'x86_64':
        return interface_base
    if '/iOSSupport/' in module_path:
        return 'x86_64-apple-ios-macabi'
    if '/MacOS' in module_path:
        return 'x86_64-apple-macos'
    return interface_base


def process_module(module_file):
    global args, shared_output_lock
    try:
        interface_base, _ = \
            os.path.splitext(os.path.basename(module_file.path))
        interface_base = \
            rename_interface_for_iosmac_if_needed(interface_base,
                                                  module_file.path)

        swiftc = os.getenv('SWIFT_EXEC',
                           os.path.join(os.path.dirname(__file__), 'swiftc'))
        command_args = [
            swiftc, '-frontend',
            '-build-module-from-parseable-interface',
            '-sdk', args.sdk,
            '-prebuilt-module-cache-path', args.output_dir,
            '-track-system-dependencies'
        ]
        module_cache_path = ""
        if args.module_cache_path:
            module_cache_path = os.path.join(args.module_cache_path,
                                             str(os.getpid()))
            command_args += ('-module-cache-path', module_cache_path)
        if args.debug_crash_compiler:
            command_args += ('-debug-crash-immediately',)
        if not args.check_only:
            command_args += (
                '-serialize-parseable-module-interface-dependency-hashes',)
        if args.disable_modules_validate_system_headers:
            command_args += (
                '-disable-modules-validate-system-headers',)

        # FIXME: This shouldn't be necessary, but the module name is checked
        # before the frontend action is.
        if module_file.name == STDLIB_NAME:
            command_args += ('-parse-stdlib',)

        if looks_like_iosmac(interface_base):
            for system_framework_path in args.iosmac_system_framework_dirs:
                command_args += ('-Fsystem', system_framework_path)
            command_args += ('-Fsystem', os.path.join(args.sdk, "System",
                                                      "iOSSupport", "System",
                                                      "Library", "Frameworks"))

        for include_path in args.include_dirs:
            command_args += ('-I', include_path)
        for system_framework_path in args.system_framework_dirs:
            command_args += ('-Fsystem', system_framework_path)
        for framework_path in args.framework_dirs:
            command_args += ('-F', framework_path)

        command_args += ('-module-name', module_file.name, module_file.path)

        output_path = os.path.join(args.output_dir,
                                   module_file.name + ".swiftmodule")

        if interface_base != module_file.name:
            make_dirs_if_needed(output_path, args.dry_run)
            output_path = os.path.join(output_path,
                                       interface_base + ".swiftmodule")

        command_args += ('-o', output_path)

        if args.verbose:
            with shared_output_lock:
                print("# Starting " + module_file.path)
                print(' '.join(command_args))
                sys.stdout.flush()
        status, stdout, stderr = run_with_module_cache_retry(
            command_args, module_cache_path=module_cache_path,
            dry_run=args.dry_run)
        log_output_to_file(stdout, module_file.name, interface_base, "out",
                           log_path=args.log_path)
        log_output_to_file(stderr, module_file.name, interface_base, "err",
                           log_path=args.log_path)
        return (module_file, status, stdout, stderr)
    except BaseException:
        # We're catching everything here because we don't want to take down the
        # other jobs.
        return (module_file, 1, "",
                "".join(traceback.format_exception(*sys.exc_info())))


def set_up_child(parent_args, lock):
    global args, shared_output_lock
    args = parent_args
    shared_output_lock = lock


def process_module_files(pool, module_files):
    results = pool.imap_unordered(process_module, module_files)

    overall_exit_status = 0
    for (module_file, exit_status, stdout, stderr) in results:
        with shared_output_lock:
            if exit_status != 0:
                print("# ", end="")
                if module_file.is_expected_to_fail:
                    print("(XFAIL) ", end="")
                else:
                    print("(FAIL) ", end="")
                print(module_file.path)
                if (not module_file.is_expected_to_fail) or args.verbose:
                    print(stdout, end="")
                    print(stderr, end="", file=sys.stderr)
            elif module_file.is_expected_to_fail:
                print("# (UPASS) " + module_file.path)
            elif args.verbose:
                print("# (PASS) " + module_file.path)
            sys.stdout.flush()
            if overall_exit_status == 0 and \
                    not module_file.is_expected_to_fail:
                overall_exit_status = exit_status
    return overall_exit_status


def main():
    global args, shared_output_lock
    parser = create_parser()
    args = parser.parse_args()

    if args.machine_parseable_monotonic_version:
        print(MONOTONIC_VERSION)
        sys.exit(0)

    if 'SWIFT_FORCE_MODULE_LOADING' not in os.environ:
        os.environ['SWIFT_FORCE_MODULE_LOADING'] = 'prefer-serialized'

    if not args.output_dir:
        fatal("argument -o is required")
    if not args.sdk:
        fatal("SDKROOT must be set in the environment")
    if not os.path.isdir(args.sdk):
        fatal("invalid SDK: " + args.sdk)

    xfails = ()
    if args.ignore_non_stdlib_failures:
        if args.xfails:
            print("warning: ignoring -xfails because "
                  "-ignore-non-stdlib-failures was provided", file=sys.stderr)
        xfails = NegatedSet((STDLIB_NAME,))
    elif args.xfails:
        with open(args.xfails) as xfails_file:
            xfails = json.load(xfails_file)

    make_dirs_if_needed(args.output_dir, args.dry_run)
    if 'ANDROID_DATA' not in os.environ:
        shared_output_lock = multiprocessing.Lock()
        pool = multiprocessing.Pool(args.jobs, set_up_child,
                                    (args, shared_output_lock))
    else:
        # Android doesn't support Python's multiprocessing as it doesn't have
        # sem_open, so switch to a ThreadPool instead.
        import threading
        shared_output_lock = threading.Lock()
        from multiprocessing.pool import ThreadPool
        pool = ThreadPool(args.jobs, set_up_child,
                          (args, shared_output_lock))

    interface_framework_dirs = (args.interface_framework_dirs or
                                DEFAULT_FRAMEWORK_INTERFACE_SEARCH_PATHS)

    module_files = list(itertools.chain(
        collect_non_framework_modules(args.sdk, xfails,
                                      BARE_INTERFACE_SEARCH_PATHS),
        collect_framework_modules(args.sdk, xfails, interface_framework_dirs)))

    if not args.skip_stdlib:
        # Always do the stdlib first, so that we can use it in later steps
        stdlib_module_files = (
            x for x in module_files if x.name == STDLIB_NAME)
        status = process_module_files(pool, stdlib_module_files)
        if status != 0:
            sys.exit(status)

    non_stdlib_module_files = (
        x for x in module_files if x.name != STDLIB_NAME)
    status = process_module_files(pool, non_stdlib_module_files)
    sys.exit(status)


if __name__ == '__main__':
    main()
