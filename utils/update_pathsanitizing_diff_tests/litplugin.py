import os
from swift_path_sanitize import reference_path, output_path, temp_dir_from_args
from lit_support.split_file import propagate_split_files

"""
This file provides the `psd_lit_plugin` function, which is invoked on failed RUN
lines when lit is executed with --update-tests.
It checks whether the failed command is a PathSanitizingDiff invocation and, if
so, repairs the test by replacing the reference file with the sanitized program
output. PathSanitizingDiff writes its sanitized input to an output file in the
per-test temporary namespace (see PathSanitizingDiff and
swift_path_sanitize.output_path); this plugin reads that output, overwrites the
reference file, and, if the reference file originated from `split-file`,
propagates the new content into the corresponding slice of the test file.
"""


def repair_test(test_path, reference, output, commands):
    """
    Repair a PathSanitizingDiff test using the sanitized output previously
    written to `output`. Overwrites `reference` with the output content and,
    if `reference` came from `split-file`, propagates the content into the
    matching slice of `test_path`.

    Returns (error_string, None) on failure.
    Returns (None, message_string) on success.
    """
    if not os.path.exists(output):
        return (
            "update-pathsanitizing-diff-test: expected sanitized output at "
            f"{output} but it was not found",
            None,
        )

    with open(output, "r") as f:
        content = f.read()
    with open(reference, "w") as f:
        f.write(content)

    updated_files = propagate_split_files(test_path, [reference], commands)
    return (
        None,
        f"update-pathsanitizing-diff-test: updated {updated_files[0]}",
    )


def psd_lit_plugin(result, test, commands):
    args = result.command.args
    if not any(arg.endswith("PathSanitizingDiff") for arg in args):
        return None

    reference = reference_path(args)
    temp_dir = temp_dir_from_args(args)
    if reference is None or temp_dir is None:
        return None

    output = output_path(temp_dir, reference)
    err, msg = repair_test(test.getFilePath(), reference, output, commands)
    return err or msg
