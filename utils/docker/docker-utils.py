#!/usr/bin/env python

"""
 This source file is part of the Swift.org open source project

 Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
 Licensed under Apache License v2.0 with Runtime Library Exception

 See http://swift.org/LICENSE.txt for license information
 See http://swift.org/CONTRIBUTORS.txt for Swift project authors

 -------------------------------------------------------------------------
 This script contains helper operations to use Docker for SwitPM development
 and testing.
"""

import argparse
import urllib2
import os
import subprocess
import yaml

def call(args):
    """Prints and executes a command."""
    print " ".join(args)
    return subprocess.call(args)

def docker_run(command_args):
    """Runs a command in the container."""
    call([
        "docker",
        "run",
        "-it",
        "-v", os.getcwd() + ":/swift",
        "-w", "/swift",
        "--rm",
        "swift-build-1604"
    ] + command_args)

def build(_):
    """Builds a docker image."""
    call([
        "docker",
        "build",
        "-t", "swift-build-1604",
        os.path.dirname(os.path.realpath(__file__))
    ])

def run(args):
    """Runs an executable in the container."""
    docker_run([args.executable] + args.arguments)

def main():
    """Main script entry-point."""

    parser = argparse.ArgumentParser(
        usage="%(prog)s [build|run]",
        description="This script simplifies all the docker operations to build "
                    "and run a container for building Swift on Linux.")
    subparsers = parser.add_subparsers(dest='command')

    # build
    parser_build = subparsers.add_parser(
        "build",
        help="builds a docker image from the latest snapshot.")
    parser_build.set_defaults(func=build)

    # run
    parser_run = subparsers.add_parser(
        "run",
        help="runs an executable in a container.")
    parser_run.add_argument("executable", help="the executable to run")
    parser_run.add_argument("arguments", nargs="*")
    parser_run.set_defaults(func=run)

    args = parser.parse_args()
    args.func(args)

if __name__ == "__main__":
    main()
