#!/usr/bin/env python3

import argparse
import json
import sys


def error(message):
    sys.stderr.write(message)
    sys.stderr.write("\n")
    sys.exit(1)


def invalid_file(features_file, message):
    error(f"Invalid features file '{features_file}': {message}")


def parse_args():
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description="""
        Merges the given feature files together into stdout. Each file FILE
        must be given a corresponding PREFIX to prefix the name of each entry
        in its features list, though these could be empty if no prefix is
        required.

        Note that the files and prefixes are treated as an ordered list, ie.
        the first FILE corresponds to the first PREFIX.
        """)

    parser.add_argument(
        "--file", "-f", action="append", dest="files",
        help="path of a file to merge"
    )
    parser.add_argument(
        "--prefix", "-p", action="append", dest="prefixes",
        help="prefix to prepend to the name of each feature"
    )

    return parser.parse_known_args()


def read_features(from_file, add_prefix):
    with open(from_file, "r") as f:
        features_dict = json.load(f)

    if "features" not in features_dict:
        invalid_file(from_file, "missing 'features' key")

    features = []
    for feature in features_dict["features"]:
        if "name" not in feature:
            invalid_file(from_file, "missing name in features list")

        new_feature = {"name": add_prefix + feature["name"]}

        if "value" in feature:
            new_feature.update({"value" : feature["value"]})

        features.append(new_feature)
    return features


def main():
    (args, _) = parse_args()

    if not args.files:
        error("No files to merge were provided")

    if len(args.files) != len(args.prefixes):
        error("Must supply the same number of files and prefixes")

    features = []
    for i, (f, prefix) in enumerate(zip(args.files, args.prefixes)):
        features.extend(read_features(f, prefix))

    data = {
        "features": features
    }
    sys.stdout.write(json.dumps(data, indent=2))


if __name__ == '__main__':
    main()
