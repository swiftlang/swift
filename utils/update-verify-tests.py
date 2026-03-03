import sys
import argparse
from update_verify_tests.core import check_expectations

"""
 Pipe output from swift-frontend's -verify into this script to have the test case updated to expect the actual diagnostic output.
 When inserting new expected-* checks it will place them on the line before the location of the diagnostic, with an @+1,
 or @+N for some N if there are multiple diagnostics emitted on the same line. If the current checks are using @-N for
 this line, the new check will follow that convention also.
 Existing checks will be left untouched as much as possible, including their location and whitespace content, to minimize
 diffs. If inaccurate their count will be updated, or the check removed entirely.

 Missing features:
  - multiple prefixes on the same line (-verify-additional-prefix my-prefix -verify-additional-prefix my-other-prefix)
  - multiple prefixes on separate RUN lines (RUN: -verify-additional-prefix my-prefix\nRUN: -verify-additional-prefix my-other-prefix)
  - regexes matchers
  - multiple checks targeting the same line are supported, but a line may only contain one check
  - if multiple checks targeting the same line are failing the script is not guaranteed to produce a minimal diff
  - remarks
  - expansions
  - columns
  - fix-its
  - doc files

Example usage:
  swift-frontend -verify [file] | python3 update-verify-tests.py
  swift-frontend -verify -verify-additional-prefix check- [file] | python3 update-verify-tests.py --prefix check-
"""


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--prefix", default="", help="The prefix passed to -verify")
    args = parser.parse_args()
    (err, updated_files) = check_expectations(sys.stdin.readlines(), args.prefix)
    if err:
        print(err)
        sys.exit(1)

    if len(updated_files) > 1:
        print("\n\t".join(["updated files:"] + updated_files))
    print(f"updated file: {updated_files[0]}")


if __name__ == "__main__":
    main()
