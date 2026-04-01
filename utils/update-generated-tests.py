import sys
import argparse
from update_generated_tests.litplugin import update_generated_test

"""
Run the GENERATED-BY command found in a test file and update the file with
the output.  All lines before and including the GENERATED-BY: comment are
preserved; everything after it (up to the next split-file section header, or
EOF) is replaced with the command's stdout.

Substitutions can be applied to the GENERATED-BY command before it is
executed by passing one or more --subst PATTERN REPLACEMENT pairs.  PATTERN
is treated as a regular expression (as in re.sub).

Example usage:
  python3 update-generated-tests.py path/to/test.swift
  python3 update-generated-tests.py --subst '%t' /tmp/mytest.tmp path/to/test.swift
"""


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("test_file", help="The test file to update")
    parser.add_argument(
        "--subst",
        nargs=2,
        action="append",
        metavar=("PATTERN", "REPLACEMENT"),
        default=[],
        help="Apply a substitution to the GENERATED-BY command (may be repeated)",
    )
    args = parser.parse_args()

    (err, msg) = update_generated_test(args.test_file, substitutions=args.subst)
    if err:
        print(err, file=sys.stderr)
        sys.exit(1)
    if msg:
        print(msg)


if __name__ == "__main__":
    main()

