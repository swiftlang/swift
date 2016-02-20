import subprocess


def _command_for_architecture(architecture=None):
    if architecture is None:
        return ['otool']
    else:
        return ['otool', '-arch', architecture]


def fat_headers(path):
    """
    Returns the headers for the executable at the given path.
    Raises a subprocess.CalledProcessError if otool encounters an error,
    such as not finding a file at the given path.
    """
    return subprocess.check_output(['otool', '-V', '-f', path])


def load_commands(path, architecture=None, include_text_sections=False):
    """
    Returns the load commands for the executable at the given path,
    for the given architecture. If print_text_section is specified,
    the disassembled text section of the load commands is also output.

    Raises a subprocess.CalledProcessError if otool encounters an error,
    such as not finding a file at the given path.
    """
    command = _command_for_architecture(architecture) + ['-l']
    if include_text_sections:
        command += ['-v', '-t']
    return subprocess.check_output(command + [path])


def text_sections(path, architecture=None):
    """
    Returns the contents of the text sections of the executable at the
    given path, for the given architecture.

    Raises a subprocess.CalledProcessError if otool encounters an error,
    such as not finding a file at the given path.
    """
    return subprocess.check_output(
        _command_for_architecture(architecture) +
        ['-v', '-s', '__TEXT', '__textcoal_nt', path])
