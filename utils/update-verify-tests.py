import sys
import argparse
from update_verify_tests.core import check_expectations

"""
 Pipe output from clang's -verify into this script to have the test case updated to expect the actual diagnostic output.
 When inserting new expected-* checks it will place them on the line before the location of the diagnostic, with an @+1,
 or @+N for some N if there are multiple diagnostics emitted on the same line. If the current checks are using @-N for
 this line, the new check will follow that convention also.
 Existing checks will be left untouched as much as possible, including their location and whitespace content, to minimize
 diffs. If inaccurate their count will be updated, or the check removed entirely.

 Missing features:
  - multiple prefixes on the same line (-verify=my-prefix,my-other-prefix)
  - multiple prefixes on separate RUN lines (RUN: -verify=my-prefix\nRUN: -verify my-other-prefix)
  - regexes with expected-*-re: existing ones will be left untouched if accurate, but the script will abort if there are any
    diagnostic mismatches on the same line.
  - multiple checks targeting the same line are supported, but a line may only contain one check
  - if multiple checks targeting the same line are failing the script is not guaranteed to produce a minimal diff

Example usage:
  clang -verify [file] | python3 update-verify-tests.py
  clang -verify=check [file] | python3 update-verify-tests.py --prefix check
"""


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--prefix", default="", help="The prefix passed to -verify"
    )
    args = parser.parse_args()
    (ret_code, output) = check_expectations(sys.stdin.readlines(), args.prefix)
    print(output)
    sys.exit(ret_code)


if __name__ == "__main__":
    main()

