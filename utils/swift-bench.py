#!/usr/bin/env python
# ===--- swift-bench.py ------------------------------*- coding: utf-8 -*-===//
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

# This file implements a test harness for running Swift performance benchmarks.
#
# Its input is a set of swift files, containing functions named 'bench_*' that
# take no arguments and returns Int. The harness makes a separate test from
# each of these functions, runs all the tests and reports aggregate results.
#
# The workflow of the harness is the following:
#   o Basing on the input files, generate 'processed' files. These files
#     contain a main function with simple arguments parsing, time measurement
#     utilities and a loop in which the bench-functions are called.
#   o When all files are processed, the harness begins to compile them, keeping
#     track of all compile fails for later results reporting.
#   o When all files are compiled, the harness begins to run the tests. The
#     harness chooses a number of iterations for each tests to achieve the best
#     accuracy in the given time limit (in order to do that, it performs
#     several auxiliary test runs). When the iteration number is chosen, the
#     measurement of execution time is actually performed.
#   o At this point everything is ready, and the harness simply reports the
#     results.
#
# Ideas for the harness improvement and development are welcomed here:
# rdar://problem/18072938

from __future__ import print_function

import argparse
import math
import os
import re
import subprocess
import sys


# This regular expression is looking for Swift functions named `bench_*`
# that take no arguments and return an Int.  The Swift code for such
# a function is:
#
#     func bench_myname() {
#         // function body goes here
#     }
BENCH_RE = re.compile(
    r"^\s*"             # whitespace at the start of the line
    r"func\s+"          # 'func' keyword, which must be followed by
                        # at least one space
    r"bench_([a-zA-Z0-9_]+)\s*"
                        # name of the function
    r"\s*\(\s*\)"       # argument list
    r"\s*->\s*Int\s*"   # return type
    r"({)?"             # opening brace of the function body
    r"\s*$"             # whitespace ot the end of the line
)


def pstdev(sample):
    """Given a list of numbers, return the population standard deviation.

    For a population x_1, x_2, ..., x_N with mean M, the standard deviation
    is defined as

        sqrt( 1/N * [ (x_1 - M)^2 + (x_2 - M)^2 + ... + (x_N - M)^2 ] )
    """
    if len(sample) == 0:
        raise ValueError("Cannot calculate the standard deviation of an "
                         "empty list!")
    mean = sum(sample) / float(len(sample))
    inner = 1.0 / len(sample) * (sum((x - mean) ** 2 for x in sample))
    return math.sqrt(inner)


class SwiftBenchHarness(object):
    sources = []
    verbose_level = 0
    compiler = ""
    tests = {}
    time_limit = 1000
    min_sample_time = 100
    min_iter_time = 1
    opt_flags = []

    def log(self, str, level):
        if self.verbose_level >= level:
            for _ in range(1, level):
                sys.stdout.write('  ')
            print(str)

    def run_command(self, cmd):
        self.log('    Executing: ' + ' '.join(cmd), 1)
        return subprocess.check_output(cmd, stderr=subprocess.STDOUT)

    def parse_arguments(self):
        self.log("Parsing arguments.", 2)
        parser = argparse.ArgumentParser()
        parser.add_argument(
            "-v", "--verbosity",
            help="increase output verbosity", type=int)
        parser.add_argument("files", help="input files", nargs='+')
        parser.add_argument(
            '-c', '--compiler',
            help="compiler to use", default="swiftc")
        parser.add_argument(
            '-t', '--timelimit',
            help="Time limit for every test", type=int)
        parser.add_argument(
            '-s', '--sampletime',
            help="Minimum time for every sample", type=int)
        parser.add_argument(
            '-f', '--flags', help="Compilation flags", nargs='+')
        args = parser.parse_args()
        if args.verbosity:
            self.verbose_level = args.verbosity
        self.sources = args.files
        self.compiler = args.compiler
        if args.flags:
            self.opt_flags = args.flags
        if args.timelimit and args.timelimit > 0:
            self.time_limit = args.timelimit
        if args.sampletime and args.sampletime > 0:
            self.min_sample_time = args.sampletime
        self.log("Sources: %s." % ', '.join(self.sources), 3)
        self.log("Compiler: %s." % self.compiler, 3)
        self.log("Opt flags: %s." % ', '.join(self.opt_flags), 3)
        self.log("Verbosity: %s." % self.verbose_level, 3)
        self.log("Time limit: %s." % self.time_limit, 3)
        self.log("Min sample time: %s." % self.min_sample_time, 3)

    def process_source(self, name):
        self.log("Processing source file: %s." % name, 2)

        header = """
@_silgen_name("mach_absolute_time") func __mach_absolute_time__() -> UInt64
@_silgen_name("opaqueGetInt32")
func _opaqueGetInt32(x: Int) -> Int
@_silgen_name("opaqueGetInt64")
func _opaqueGetInt64(x: Int) -> Int

@inline(never)
public func getInt(x: Int) -> Int {
#if arch(i386) || arch(arm)
  return _opaqueGetInt32(x)
#elseif arch(x86_64) || arch(arm64) || arch(powerpc64) || \
arch(powerpc64le) || arch(s390x)
  return _opaqueGetInt64(x)
#else
  return x
#endif
}
@inline(never)
func False() -> Bool { return getInt(1) == 0 }
@inline(never)
func Consume(x: Int) { if False() { println(x) } }
"""
        before_bench = """
@inline(never)
"""
        into_bench = """
  if False() { return 0 }
"""
        main_begin = """
func main() {
  var N = 1
  var name = ""
  if CommandLine.arguments.count > 1 {
    N = CommandLine.arguments[1].toInt()!
  }
"""
        main_body = """
  name = "%s"
  if CommandLine.arguments.count <= 2 || CommandLine.arguments[2] == name {
    let start = __mach_absolute_time__()
    for _ in 1...N {
      bench_%s()
    }
    let end = __mach_absolute_time__()
    println("\(name),\(N),\(end - start)")
  }
"""
        main_end = """
}
main()
"""

        with open(name) as f:
            lines = list(f)
        output = header
        looking_for_curly_brace = False
        test_names = []
        for lineno, l in enumerate(lines, start=1):
            if looking_for_curly_brace:
                output += l
                if "{" not in l:
                    continue
                looking_for_curly_brace = False
                output += into_bench
                continue

            m = BENCH_RE.match(l)
            if m:
                output += before_bench
                output += l
                bench_name = m.group(1)
                self.log("Benchmark found: %s (line %d)" %
                         (bench_name, lineno), 3)
                self.tests[
                    name + ":" +
                    bench_name] = Test(bench_name, name, "", "")
                test_names.append(bench_name)
                if m.group(2):
                    output += into_bench
                else:
                    looking_for_curly_brace = True
            else:
                output += l

        output += main_begin
        for n in test_names:
            output += main_body % (n, n)
        processed_name = 'processed_' + os.path.basename(name)
        output += main_end
        with open(processed_name, 'w') as f:
            f.write(output)
        for n in test_names:
            self.tests[name + ":" + n].processed_source = processed_name

    def process_sources(self):
        self.log("Processing sources: %s." % self.sources, 2)
        for s in self.sources:
            self.process_source(s)

    def compile_opaque_cfile(self):
        self.log("Generating and compiling C file with opaque functions.", 3)
        file_body = """
#include <stdint.h>
extern "C" int32_t opaqueGetInt32(int32_t x) { return x; }
extern "C" int64_t opaqueGetInt64(int64_t x) { return x; }
"""
        with open('opaque.cpp', 'w') as f:
            f.write(file_body)
        # TODO: Handle subprocess.CalledProcessError for this call:
        self.run_command(
            ['clang++', 'opaque.cpp', '-o', 'opaque.o', '-c', '-O2'])

    compiled_files = {}

    def compile_source(self, name):
        self.tests[name].binary = "./" + \
            self.tests[name].processed_source.split(os.extsep)[0]
        if not self.tests[name].processed_source in self.compiled_files:
            try:
                self.run_command([
                    self.compiler,
                    self.tests[name].processed_source,
                    "-o",
                    self.tests[name].binary + '.o',
                    '-c'
                ] + self.opt_flags)
                self.run_command([
                    self.compiler,
                    '-o',
                    self.tests[name].binary,
                    self.tests[name].binary + '.o',
                    'opaque.o'
                ])
                self.compiled_files[
                    self.tests[name].processed_source] = ('', '')
            except subprocess.CalledProcessError as e:
                self.compiled_files[self.tests[name].processed_source] = (
                    'COMPFAIL', e.output)

        (status, output) = self.compiled_files[
            self.tests[name].processed_source]
        self.tests[name].status = status
        self.tests[name].output = output

    def compile_sources(self):
        self.log("Compiling processed sources.", 2)
        self.compile_opaque_cfile()
        for t in self.tests:
            self.compile_source(t)

    def run_benchmarks(self):
        self.log("Running benchmarks.", 2)
        for t in self.tests:
            self.run_bench(t)

    def parse_benchmark_output(self, res):
        # Parse lines like
        # TestName,NNN,MMM
        # where NNN - performed iterations number, MMM - execution time (in ns)
        results_re = re.compile(r"(\w+),[ \t]*(\d+),[ \t]*(\d+)")
        m = results_re.match(res)
        if not m:
            return ("", 0, 0)
        return (m.group(1), m.group(2), m.group(3))

    def compute_iters_number(self, name):
        scale = 1
        spent = 0
        # Measure time for one iteration
        # If it's too small, increase number of iteration until it's measurable
        while (spent <= self.min_iter_time):
            try:
                r = self.run_command([
                    self.tests[name].binary, str(scale),
                    self.tests[name].name])
                (test_name, iters_computed, exec_time) = \
                    self.parse_benchmark_output(r)
                # Convert ns to ms
                spent = int(exec_time) / 1000000
                if spent <= self.min_iter_time:
                    scale *= 2
                if scale > sys.maxint:
                    return (0, 0)
            except subprocess.CalledProcessError as e:
                r = e.output
                break
        if spent == 0:
            spent = 1
        # Now compute number of samples we can take in the given time limit
        mult = int(self.min_sample_time / spent)
        if mult == 0:
            mult = 1
        scale *= mult
        spent *= mult
        samples = int(self.time_limit / spent)
        if samples == 0:
            samples = 1
        return (samples, scale)

    def run_bench(self, name):
        if not self.tests[name].status == "":
            return
        (num_samples, iter_scale) = self.compute_iters_number(name)
        if (num_samples, iter_scale) == (0, 0):
            self.tests[name].status = "CAN'T MEASURE"
            self.tests[name].output = (
                "Can't find number of iterations for the test to last " +
                "longer than %d ms." % self.min_iter_time)
            return
        samples = []
        self.log("Running bench: %s, numsamples: %d" % (name, num_samples), 2)
        for _ in range(0, num_samples):
            try:
                r = self.run_command([self.tests[name].binary, str(iter_scale),
                                      self.tests[name].name])
                (test_name, iters_computed, exec_time) = \
                    self.parse_benchmark_output(r)
                # TODO: Verify test_name and iters_computed
                samples.append(int(exec_time) / iter_scale)
                self.tests[name].output = r
            except subprocess.CalledProcessError as e:
                self.tests[name].status = "RUNFAIL"
                self.tests[name].output = e.output
                break
        res = TestResults(name, samples)
        self.tests[name].results = res

    def report_results(self):
        self.log("\nReporting results.", 2)
        print("==================================================")
        for t in self.tests:
            self.tests[t].do_print()


class Test(object):

    def __init__(self, name, source, processed_source, binary):
        self.name = name
        self.source = source
        self.processed_source = processed_source
        self.binary = binary
        self.status = ""
        self.results = None
        self.output = None

    def do_print(self):
        print("NAME: %s" % self.name)
        print("SOURCE: %s" % self.source)
        if self.status == "":
            if self.results is not None:
                self.results.do_print()
        else:
            print("STATUS: %s" % self.status)
            print("OUTPUT:")
            print(self.output)
            print("END OF OUTPUT")
        print("")


class TestResults(object):

    def __init__(self, name, samples):
        self.name = name
        self.samples = samples
        if len(samples) > 0:
            self.process()

    def process(self):
        self.minimum = min(self.samples)
        self.maximum = max(self.samples)
        self.avg = sum(self.samples) / len(self.samples)
        self.std = pstdev(self.samples)
        self.err = self.std / math.sqrt(len(self.samples))
        self.int_min = self.avg - self.err * 1.96
        self.int_max = self.avg + self.err * 1.96

    def do_print(self):
        print("SAMPLES: %d" % len(self.samples))
        print("MIN: %3.2e" % self.minimum)
        print("MAX: %3.2e" % self.maximum)
        print("AVG: %3.2e" % self.avg)
        print("STD: %3.2e" % self.std)
        print("ERR: %3.2e (%2.1f%%)" % (self.err, self.err * 100 / self.avg))
        print("CONF INT 0.95: (%3.2e, %3.2e)" % (self.int_min, self.int_max))
        print("")


def main():
    harness = SwiftBenchHarness()
    harness.parse_arguments()
    harness.process_sources()
    harness.compile_sources()
    harness.run_benchmarks()
    harness.report_results()


main()
