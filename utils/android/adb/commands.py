# adb/commands.py - Run executables on an Android device -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------
#
# Push executables to an Android device and run them, capturing their output
# and exit code.
#
# ----------------------------------------------------------------------------

import os
import subprocess
import tempfile
import uuid


# A temporary directory on the Android device.
DEVICE_TEMP_DIR = '/data/local/tmp'
ENV_PREFIX = 'ANDROID_CHILD_'


# Subprocess output might be bytes or str depending on OS and Python version
def _as_str(s):
    return s if isinstance(s, str) else str(s, 'utf-8')


def shell(args):
    """
    Execute 'adb shell' with the given arguments.

    Raise an exception if 'adb shell' returns a non-zero exit code.
    Note that this only occurs if communication with the connected device
    fails, not if the command run on the device fails.
    """
    return subprocess.check_output(['adb', 'shell'] + args)


def rmdir(path):
    """Remove all files in the device directory at `path`."""
    shell(['rm', '-rf', '{}/*'.format(path)])


def push(local_paths, device_path):
    """Move the files at the given local paths to the path on the device."""
    if isinstance(local_paths, str):
        local_paths = [local_paths]
    try:
        # In recent versions of ADB, push supports --sync, which checksums the
        # files to be transmitted and skip the ones that haven't changed, which
        # improves the effective transfer speed.
        return subprocess.check_output(
            ['adb', 'push', '--sync'] + local_paths + [device_path],
            stderr=subprocess.STDOUT).strip()
    except subprocess.CalledProcessError as e:
        if "unrecognized option '--sync'" in _as_str(e.output):
            return subprocess.check_output(
                ['adb', 'push'] + local_paths + [device_path],
                stderr=subprocess.STDOUT).strip()
        else:
            raise e


def reboot():
    """Reboot the connected Android device, waiting for it to return online."""
    subprocess.check_call(['adb', 'reboot'])
    subprocess.check_call(['adb', 'wait-for-device'])


def _create_executable_on_device(device_path, contents):
    _, tmp = tempfile.mkstemp()
    with open(tmp, 'w') as f:
        f.write(contents)
    push(tmp, device_path)
    shell(['chmod', '755', device_path])


def execute_on_device(executable_path, executable_arguments):
    """
    Run an executable on an Android device.

    Push an executable at the given 'executable_path' to an Android device,
    then execute that executable on the device, passing any additional
    'executable_arguments'. Return 0 if the executable succeeded when run on
    device, and 1 otherwise.

    This function is not as simple as calling 'adb shell', for two reasons:

    1. 'adb shell' can only take input up to a certain length, so it fails for
       long executable names or when a large amount of arguments are passed to
       the executable. This function attempts to limit the size of any string
       passed to 'adb shell'.
    2. 'adb shell' ignores the exit code of any command it runs. This function
       therefore uses its own mechanisms to determine whether the executable
       had a successful exit code when run on device.
    """
    # We'll be running the executable in a temporary directory in
    # /data/local/tmp. `adb shell` has trouble with commands that
    # exceed a certain length, so to err on the safe side we only
    # use the first 10 characters of the UUID.
    uuid_dir = '{}/{}'.format(DEVICE_TEMP_DIR, str(uuid.uuid4())[:10])
    shell(['mkdir', '-p', uuid_dir])

    # `adb` can only handle commands under a certain length. That's why we
    # hide the arguments and piping/status in executable files. However, at
    # least one resilience test relies on checking the executable name, so we
    # need to use the same name as the one provided.
    executable_name = os.path.basename(executable_path)
    executable = '{}/{}'.format(uuid_dir, executable_name)
    push(executable_path, executable)
    shell(['chmod', '+x', executable])

    child_environment = ['{}="{}"'.format(k.replace(ENV_PREFIX, '', 1), v)
                         for (k, v) in os.environ.items()
                         if k.startswith(ENV_PREFIX)]

    # The executables are sometimes passed arguments, and sometimes those
    # arguments are files that have to be pushed, but also the argument values
    # have to be changed to the new path in the Android device.
    translated_executable_arguments = []
    for executable_argument in executable_arguments:
        # Currently we only support arguments that are file paths themselves.
        # Things like `--foo=/path/to/file` or directories are not supported.
        # Relative paths from the executable to the arguments are not kept.
        if os.path.isfile(executable_argument):
            final_path = '{}/{}'.format(uuid_dir,
                                        os.path.basename(executable_argument))
            push(executable_argument, final_path)
            translated_executable_arguments.append(final_path)
        else:
            translated_executable_arguments.append(executable_argument)

    # When running the executable on the device, we need to pass it the same
    # arguments, as well as specify the correct LD_LIBRARY_PATH. Save these
    # to a file we can easily call multiple times.
    executable_with_args = '{}/__executable_with_args'.format(uuid_dir)
    _create_executable_on_device(
        executable_with_args,
        'LD_LIBRARY_PATH={uuid_dir}:{tmp_dir} '
        '{child_environment} {executable} {executable_arguments}'.format(
            uuid_dir=uuid_dir,
            tmp_dir=DEVICE_TEMP_DIR,
            child_environment=' '.join(child_environment),
            executable=executable,
            executable_arguments=' '.join(translated_executable_arguments)))

    # Write the output from the test executable to a file named '__stdout', and
    # if the test executable succeeds, write 'SUCCEEDED' to a file
    # named '__succeeded'. We do this because `adb shell` does not report
    # the exit code of the command it executes on the device, so instead we
    # check the '__succeeded' file for our string.
    executable_stdout = '{}/__stdout'.format(uuid_dir)
    succeeded_token = 'SUCCEEDED'
    executable_succeeded = '{}/__succeeded'.format(uuid_dir)
    executable_piped = '{}/__executable_piped'.format(uuid_dir)
    _create_executable_on_device(
        executable_piped,
        '{executable_with_args} > {executable_stdout} && '
        'echo "{succeeded_token}" > {executable_succeeded}'.format(
            executable_with_args=executable_with_args,
            executable_stdout=executable_stdout,
            succeeded_token=succeeded_token,
            executable_succeeded=executable_succeeded))

    # We've pushed everything we need to the device.
    # Now execute the wrapper script.
    shell([executable_piped])

    # Grab the results of running the executable on device.
    stdout = _as_str(shell(['cat', executable_stdout]))
    exitcode = _as_str(shell(['cat', executable_succeeded]))
    if not exitcode.startswith(succeeded_token):
        debug_command = '$ adb shell {}'.format(executable_with_args)
        print('Executable exited with a non-zero code on the Android device.\n'
              'Device stdout:\n'
              '{stdout}\n'
              'To debug, run:\n'
              '{debug_command}\n'.format(
                  stdout=stdout,
                  debug_command=debug_command))

        # Exit early so that the output isn't passed to FileCheck, nor are any
        # temporary directories removed; this allows the user to re-run
        # the executable on the device.
        return 1

    print(stdout, end='')

    shell(['rm', '-rf', uuid_dir])
    return 0
