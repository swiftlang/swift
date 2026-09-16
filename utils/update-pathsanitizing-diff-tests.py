import argparse
import shlex
import sys

from swift_path_sanitize import output_path
from update_pathsanitizing_diff_tests.litplugin import repair_test

"""
Standalone entry point for repairing a PathSanitizingDiff test from the output
file that PathSanitizingDiff writes into the per-test temporary namespace. This
mirrors the lit --update-tests behaviour (see
update_pathsanitizing_diff_tests.litplugin) so that the update logic can be
exercised outside lit.

Given the test file, the reference file, the temporary location (%t), and the
`split-file` output directory, it locates the `<temp-dir>`-derived output (see
swift_path_sanitize.output_path), overwrites the reference file with it, and
propagates the content into the matching slice of the test file.

Example usage:
  python3 update-pathsanitizing-diff-tests.py test.swift %t/out.expected \\
      --temp-dir %t --split-file-dir %t
"""


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("test_file", help="The test file to update")
    parser.add_argument("reference", help="The reference file to update")
    parser.add_argument(
        "--temp-dir",
        required=True,
        help="The test's temporary location (%%t) PathSanitizingDiff was given",
    )
    parser.add_argument(
        "--split-file-dir",
        help="Directory the reference file was materialized into by split-file",
    )
    args = parser.parse_args()

    # Synthesize the `split-file` command that propagate_split_files scans for,
    # so the standalone path shares the lit plugin's propagation logic.
    commands = []
    if args.split_file_dir:
        commands = [
            "split-file %s %s"
            % (shlex.quote(args.test_file), shlex.quote(args.split_file_dir))
        ]

    output = output_path(args.temp_dir, args.reference)
    (err, msg) = repair_test(args.test_file, args.reference, output, commands)
    if err:
        print(err, file=sys.stderr)
        sys.exit(1)
    if msg:
        print(msg)


if __name__ == "__main__":
    main()
