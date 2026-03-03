from update_verify_tests.core import check_expectations
from lit_support.split_file import propagate_split_files

"""
This file provides the `uvt_lit_plugin` function, which is invoked on failed RUN lines when lit is executed with --update-tests.
It checks whether the failed command is a swift compiler invocation with the `-verify` flag and analyses the output to try to
repair the failed test. If the updated file was originally created by `split-file` it updates the corresponding slice in the source file.
"""


def uvt_lit_plugin(result, test, commands):
    if (
        not any(e.endswith("swift-frontend") for e in result.command.args)
        or not "-verify" in result.command.args
    ):
        return None

    prefix = ""
    for i, arg in enumerate(result.command.args):
        if arg == "-verify-additional-prefix":
            if i + 1 >= len(result.command.args):
                return None
            if prefix:
                # can only handle at most 1 additional prefix at the moment
                return None
            prefix = result.command.args[i + 1]

    (err, updated_files) = check_expectations(result.stderr.split("\n"), prefix)
    if err:
        return err

    updated_files = propagate_split_files(test.getFilePath(), updated_files, commands)

    if len(updated_files) > 1:
        return "\n\t".join(["updated files:"] + updated_files)
    return f"updated file: {updated_files[0]}"
