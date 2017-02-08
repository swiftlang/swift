# ====--- compare.py - Compare built products' sizes -*- coding: utf-8 -*-===//
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

from __future__ import print_function

import collections
import os
import re
import subprocess

from operator import itemgetter

categories = [
    # Cpp
    ["CPP", re.compile('^(__Z|_+swift)')],

    # Objective-C
    ["ObjC", re.compile('^[+-]\[')],

    # Swift
    ["Partial Apply", re.compile('^__(TPA|T0.*T[aA]$)')],
    ["Protocol Witness", re.compile('^__(TTW|T0.*TW$)')],
    ["Value Witness", re.compile('^__(Tw|T0.*w..$)')],
    ["Type Metadata", re.compile('^__(TM|T0.*(N|M.)$)')],
    # Function signature specialization of a generic specialization.
    ["FuncSigGen Spec", re.compile(
        '^__(TTSf.*__TTSg|T0.*T[gGpP]q?[0-9].*Tfq?[0-9])')],
    ["Generic Spec", re.compile('^__(TTSg|T0.*T[gGpP]q?[0-9])')],
    ["FuncSig Spec", re.compile('^__(TTSf|T0.*Tfq?[0-9])')],
    ["Generic Function", re.compile(
        '__(T[^0].*q(x|d?[0-9]*_)|T0.*q(z|d?[0-9]*_))')],
    ["Static Func", re.compile('^__(TZF|T0.*FZ)')],
    ["Swift @objc Func", re.compile('^__(TTo|T0.*To$)')],
    ["Accessor", re.compile('^__(TW[atTlI]|T0.*W[atTlI]$)')],
    ["Getter/Setter", re.compile('^__(T[Fvi][gsmwWl]|T0.*f[gGsmwWal]$)')],
    ["Swift Function", re.compile('^__(TF|T0.*(F|f.|f[AuU][0-9]*_)$)')],
    ["Unknown", re.compile('')]
]


def add_function(sizes, function, start_addr, end_addr, group_by_prefix):
    if not function or start_addr is None or end_addr is None:
        return

    size = end_addr - start_addr

    if group_by_prefix:
        if function.endswith('_merged'):
            function = function[:-7]
        for cat in categories:
            cat_name = cat[0]
            pattern = cat[1]
            if pattern.match(function):
                sizes[cat_name] += size
                return
        assert False, "function name not matching any pattern"
    else:
        sizes[function] += size


def flatten(*args):
    for x in args:
        if hasattr(x, '__iter__'):
            for y in flatten(*x):
                yield y
        else:
            yield x


def read_sizes(sizes, file_name, function_details, group_by_prefix):
    # Check if multiple architectures are supported by the object file.
    # Prefer arm64 if available.
    architectures = subprocess.check_output(
        ["otool", "-V", "-f", file_name]).split("\n")
    arch = None
    arch_pattern = re.compile('architecture ([\S]+)')
    for architecture in architectures:
        arch_match = arch_pattern.match(architecture)
        if arch_match:
            if arch is None:
                arch = arch_match.group(1)
            if "arm64" in arch:
                arch = "arm64"
    if arch is not None:
        arch_params = ["-arch", arch]
    else:
        arch_params = []

    if function_details:
        content = subprocess.check_output(
            flatten([
                "otool",
                arch_params,
                "-l",
                "-v",
                "-t",
                file_name]
            )).split("\n")
        content += subprocess.check_output(flatten(
            ["otool", arch_params, "-v", "-s", "__TEXT", "__textcoal_nt",
             file_name])).split("\n")
    else:
        content = subprocess.check_output(
            flatten(["otool", arch_params, "-l", file_name])).split("\n")

    sect_name = None
    curr_func = None
    start_addr = None
    end_addr = None

    section_pattern = re.compile(' +sectname ([\S]+)')
    size_pattern = re.compile(' +size ([\da-fx]+)')
    asmline_pattern = re.compile('^([0-9a-fA-F]+)\s')
    label_pattern = re.compile('^((\-*\[[^\]]*\])|[^\/\s]+):$')

    for line in content:
        asmline_match = asmline_pattern.match(line)
        if asmline_match:
            addr = int(asmline_match.group(1), 16)
            if start_addr is None:
                start_addr = addr
            end_addr = addr
        elif line == "Section":
            sect_name = None
        else:
            label_match = label_pattern.match(line)
            size_match = size_pattern.match(line)
            section_match = section_pattern.match(line)
            if label_match:
                func_name = label_match.group(1)
                add_function(sizes, curr_func, start_addr,
                             end_addr, group_by_prefix)
                curr_func = func_name
                start_addr = None
                end_addr = None
            elif size_match and sect_name and group_by_prefix:
                size = int(size_match.group(1), 16)
                sizes[sect_name] += size
            elif section_match:
                sect_name = section_match.group(1)
                if sect_name == "__textcoal_nt":
                    sect_name = "__text"

    add_function(sizes, curr_func, start_addr, end_addr, group_by_prefix)


def compare_sizes(old_sizes, new_sizes, name_key, title):
    old_size = old_sizes[name_key]
    new_size = new_sizes[name_key]
    if old_size is not None and new_size is not None:
        if old_size != 0:
            perc = "%.1f%%" % (
                (1.0 - float(new_size) / float(old_size)) * 100.0)
        else:
            perc = "- "
        print("%-26s%16s: %8d  %8d  %6s" %
              (title, name_key, old_size, new_size, perc))


def compare_sizes_of_file(old_files, new_files, all_sections, list_categories):
    old_sizes = collections.defaultdict(int)
    new_sizes = collections.defaultdict(int)
    for old_file in old_files:
        read_sizes(old_sizes, old_file, list_categories, True)
    for new_file in new_files:
        read_sizes(new_sizes, new_file, list_categories, True)

    if len(old_files) == 1 and len(new_files) == 1:
        old_base = os.path.basename(old_files[0])
        new_base = os.path.basename(new_files[0])
        title = old_base
        if old_base != new_base:
            title += "-" + new_base
    else:
        title = "old-new"

    compare_sizes(old_sizes, new_sizes, "__text", title)
    if list_categories:
        for cat in categories:
            cat_name = cat[0]
            compare_sizes(old_sizes, new_sizes, cat_name, "")

    if all_sections:
        section_title = "    section"
        compare_sizes(old_sizes, new_sizes, "__textcoal_nt", section_title)
        compare_sizes(old_sizes, new_sizes, "__stubs", section_title)
        compare_sizes(old_sizes, new_sizes, "__const", section_title)
        compare_sizes(old_sizes, new_sizes, "__cstring", section_title)
        compare_sizes(old_sizes, new_sizes, "__objc_methname", section_title)
        compare_sizes(old_sizes, new_sizes, "__const", section_title)
        compare_sizes(old_sizes, new_sizes, "__objc_const", section_title)
        compare_sizes(old_sizes, new_sizes, "__data", section_title)
        compare_sizes(old_sizes, new_sizes, "__swift1_proto", section_title)
        compare_sizes(old_sizes, new_sizes, "__common", section_title)
        compare_sizes(old_sizes, new_sizes, "__bss", section_title)


def list_function_sizes(size_array):
    for pair in sorted(size_array, key=itemgetter(1)):
        name = pair[0]
        size = pair[1]
        yield "%8d %s" % (size, name)


def compare_function_sizes(old_files, new_files):
    old_sizes = collections.defaultdict(int)
    new_sizes = collections.defaultdict(int)
    for name in old_files:
        read_sizes(old_sizes, name, True, False)
    for name in new_files:
        read_sizes(new_sizes, name, True, False)

    only_in_file1 = []
    only_in_file2 = []
    in_both = []

    only_in_file1size = 0
    only_in_file2size = 0
    in_both_size = 0

    for func, old_size in old_sizes.items():
        new_size = new_sizes[func]
        if new_size != 0:
            in_both.append((func, old_size, new_size))
        else:
            only_in_file1.append((func, old_size))
            only_in_file1size += old_size

    for func, new_size in new_sizes.items():
        old_size = old_sizes[func]
        if old_size == 0:
            only_in_file2.append((func, new_size))
            only_in_file2size += new_size

    if only_in_file1:
        print("Only in old file(s)")
        print(os.linesep.join(list_function_sizes(only_in_file1)))
        print("Total size of functions only in old file: {}".format(
            only_in_file1size))
        print()

    if only_in_file2:
        print("Only in new files(s)")
        print(os.linesep.join(list_function_sizes(only_in_file2)))
        print("Total size of functions only in new file: {}".format(
            only_in_file2size))
        print()

    if in_both:
        size_increase = 0
        size_decrease = 0
        print("%8s %8s %8s" % ("old", "new", "diff"))
        for triple in sorted(
                in_both,
                key=lambda tup: (tup[2] - tup[1], tup[1])):
            func = triple[0]
            old_size = triple[1]
            new_size = triple[2]
            diff = new_size - old_size
            if diff > 0:
                size_increase += diff
            else:
                size_decrease -= diff
            if diff == 0:
                in_both_size += new_size
            print("%8d %8d %8d %s" %
                  (old_size, new_size, new_size - old_size, func))
        print("Total size of functions " +
              "with the same size in both files: {}".format(in_both_size))
        print("Total size of functions " +
              "that got smaller: {}".format(size_decrease))
        print("Total size of functions " +
              "that got bigger: {}".format(size_increase))
        print("Total size change of functions present " +
              "in both files: {}".format(size_increase - size_decrease))
