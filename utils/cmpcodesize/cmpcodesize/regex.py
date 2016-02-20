import re


# Cache the compiled regex into a global object.
_ARCHITECTURE_REGEX = re.compile('architecture (\S+)')


def architecture(fat_headers):
    """
    Given a string representing fat headers from an executable,
    returns one of the following:

    1. arm64, if that is one of the architectures listed.
    2. If arm64 us not listed, the first architecture that is listed.
    3. None, if no architectures are listed.
    """
    result = None
    for line in fat_headers.splitlines():
        match = _ARCHITECTURE_REGEX.match(line)
        if match:
            arch = match.group(1)
            if arch == 'arm64':
                return arch
            elif result is None:
                result = match.group(1)
    return result
