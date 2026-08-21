import sys
import argparse
from update_verify_tests.core import minimize_verify_test

"""
 Minimize an already-passing -verify test case by merging redundant
 prefixed expected-* directives.

 For each RUN line the script determines which verify prefixes are active
 (the default empty prefix plus any `-verify-additional-prefix` values).
 It then finds overlapping expected-* directives - those that point to the
 same source location, have the same category, content, count, and fix-its
 but differ in prefix - and merges them when a single prefix exists that is
 active in exactly the union of RUN lines covered by the overlapping set.

Example usage:
  python3 minimize-verify-tests.py test.swift
"""


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("file", nargs="+", help="Test file(s) to minimize")
    args = parser.parse_args()
    for f in args.file:
        err = minimize_verify_test(f)
        if err:
            print(err, file=sys.stderr)
            sys.exit(1)


if __name__ == "__main__":
    main()
