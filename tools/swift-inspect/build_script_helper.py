#!/usr/bin/env python3

import argparse
import os
import subprocess
import sys


def perform_swift_action(args, swift_tool_name):
    swift_tool_path = os.path.join(args.toolchain, "usr", "bin", swift_tool_name)
    swift_tool_args = [
        swift_tool_path,
        "--package-path",
        args.package_path,
        "--build-path",
        args.build_path,
        "--configuration",
        args.configuration,
        "-Xswiftc",
        "-I",
        "-Xswiftc",
        os.path.join(args.toolchain, 'usr', 'include', 'swift'),
        "-Xswiftc",
        "-L",
        "-Xswiftc",
        os.path.join(args.toolchain, 'usr', 'lib', 'swift', 'macosx'),
        "-Xswiftc",
        "-lswiftRemoteMirror"
    ]
    swift_tool_env = dict(os.environ)
    swift_tool_env["DYLD_LIBRARY_PATH"] = os.path.join(
        args.toolchain, 'usr', 'lib', 'swift', 'macosx')

    if args.verbose:
        swift_tool_args.append("--verbose")
    env_settings = [(k + "=" + v) for k, v in swift_tool_env.items()]
    print(' '.join(env_settings + swift_tool_args))
    subprocess.call(swift_tool_args, env=swift_tool_env)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--verbose", "-v", action="store_true")
    parser.add_argument("--action", type=str, required=True)
    parser.add_argument("--package-path", type=str, required=True)
    parser.add_argument("--build-path", type=str, required=True)
    parser.add_argument("--toolchain", type=str, required=True)
    parser.add_argument("--configuration", type=str, choices=['debug', 'release'],
                        default='release')

    args = parser.parse_args()

    # Create our bin directory so we can copy in the binaries.
    bin_dir = os.path.join(args.build_path, "bin")
    if not os.path.isdir(bin_dir):
        os.makedirs(bin_dir)

    if args.action == "build":
        perform_swift_action(args, "swift-build")
    elif args.action == "test":
        perform_swift_action(args, "swift-test")
    else:
        sys.exit("Unknown action " + args.action)


if __name__ == "__main__":
    main()
