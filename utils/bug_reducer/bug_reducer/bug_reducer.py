#!/usr/bin/env python

import argparse

import func_bug_reducer

import opt_bug_reducer

import random_bug_finder


def add_subparser(subparsers, module, name):
    sparser = subparsers.add_parser(name)
    sparser.add_argument('swift_build_dir',
                         help='Path to the swift build directory '
                         'containing tools to use')
    module.add_parser_arguments(sparser)


def main():
    parser = argparse.ArgumentParser(description="""\
A program for reducing sib/sil crashers""")
    subparsers = parser.add_subparsers()

    add_subparser(subparsers, opt_bug_reducer, 'opt')
    add_subparser(subparsers, random_bug_finder, 'random-search')
    add_subparser(subparsers, func_bug_reducer, 'func')

    args = parser.parse_args()
    args.func(args)


main()
