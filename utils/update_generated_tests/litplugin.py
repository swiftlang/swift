import hashlib
import re
import subprocess
from lit_support.split_file import SplitFileTarget

"""
This file provides the `generate_test_lit_plugin` function, which is invoked on failed RUN lines when lit is executed with --update-tests.
It checks whether the test file contains a GENERATED-BY: line, and if so executes that line (after performing lit substitutions) and updates the file with the output.
All lines before GENERATED-BY: are kept as is.
If the GENERATED-BY is in a `split-file` slice it updates the corresponding slice in the source file.
"""

_GENERATED_BY_RE = re.compile(r"^//\s*GENERATED-BY:\s*(.*)")
_GENERATED_HASH_RE = re.compile(r"^//\s*GENERATED-HASH:\s*(.*)")


def _run_and_update(test_path, cmd):
    """
    Run `cmd`, use its stdout to replace the GENERATED-BY section content in
    `test_path`. A GENERATED-HASH comment is inserted after the GENERATED-BY
    line containing a SHA-256 hash of the output. If the file already contains
    a GENERATED-HASH and the hash matches, the file is not rewritten.

    Returns (error_string, False) on failure.
    Returns (None, False) if the hash is unchanged and the file was not updated.
    Returns (None, True) if the file was updated.
    """
    proc = subprocess.run(cmd, shell=True, capture_output=True, text=True)
    if proc.returncode != 0:
        return (f"GENERATED-BY command failed:\n{proc.stderr}", False)

    output = proc.stdout

    with open(test_path, "r") as f:
        lines = f.readlines()

    generated_by_idx = None
    for i, line in enumerate(lines):
        if _GENERATED_BY_RE.match(line.strip()):
            generated_by_idx = i
            break

    assert (
        generated_by_idx is not None
    ), f"GENERATED-BY not found in {test_path}"

    new_hash = hashlib.sha256(output.encode()).hexdigest()

    # Check for an existing GENERATED-HASH line immediately after GENERATED-BY.
    content_start = generated_by_idx + 1
    old_hash = None
    if content_start < len(lines):
        m = _GENERATED_HASH_RE.match(lines[content_start].strip())
        if m:
            old_hash = m.group(1).strip()
            content_start += 1

    if old_hash == new_hash:
        return (None, False)

    slice_end = None
    for i, line in enumerate(lines[content_start:], start=content_start):
        if SplitFileTarget._get_split_line_path(line) is not None:
            slice_end = i
            break

    output_lines = output.splitlines(keepends=True)
    if output_lines and not output_lines[-1].endswith("\n"):
        output_lines[-1] += "\n"

    hash_line = f"// GENERATED-HASH: {new_hash}\n"
    lines_after = lines[slice_end:] if slice_end is not None else []

    with open(test_path, "w") as f:
        f.writelines(
            lines[: generated_by_idx + 1] + [hash_line] + output_lines + lines_after
        )

    return (None, True)


def update_generated_test(test_path, substitutions):
    """
    Standalone entry point (used by update-generated-tests.py).
    Find the GENERATED-BY directive in test_path, apply `substitutions` (a
    sequence of (pattern, replacement) pairs as accepted by
    lit.TestRunner.applySubstitutions), run the resulting command, and update
    the file with the output.

    Returns (None, None) if no GENERATED-BY was found, or if the output did not
    change since last generation.
    Returns (error_string, None) on failure.
    Returns (None, message_string) on success.
    """
    from lit.TestRunner import applySubstitutions

    with open(test_path, "r") as f:
        lines = f.readlines()

    for line in lines:
        m = _GENERATED_BY_RE.match(line.strip())
        if m:
            [cmd] = applySubstitutions([m.group(1).strip()], substitutions)
            (err, changed) = _run_and_update(test_path, cmd)
            if err:
                return (err, None)
            if changed:
                return (None, f"updated file: {test_path}")

    return (None, None)


def generate_test_lit_plugin(result, test, commands):
    from lit.TestRunner import getTempPaths, getDefaultSubstitutions

    tmpDir, tmpBase = getTempPaths(test)
    substitutions = getDefaultSubstitutions(test, tmpDir, tmpBase)
    (err, msg) = update_generated_test(test.getFilePath(), substitutions)
    return err or msg
