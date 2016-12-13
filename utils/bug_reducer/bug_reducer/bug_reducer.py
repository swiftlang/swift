#!/usr/bin/env python

import argparse

import opt_bug_reducer

import random_bug_finder


def main():
    parser = argparse.ArgumentParser(description="""\
A program for reducing sib/sil crashers""")
    subparsers = parser.add_subparsers()

    opt_subparser = subparsers.add_parser("opt")
    opt_subparser.add_argument('swift_build_dir',
                               help='Path to the swift build directory '
                               'containing tools to use')
    opt_bug_reducer.add_parser_arguments(opt_subparser)

    random_search_subparser = subparsers.add_parser("random-search")
    random_search_subparser.add_argument('swift_build_dir',
                                         help='Path to the swift build '
                                         'directory containing tools to use')
    random_bug_finder.add_parser_arguments(random_search_subparser)

    args = parser.parse_args()
    args.func(args)


main()
