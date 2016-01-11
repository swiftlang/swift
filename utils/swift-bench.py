#!/usr/bin/env python
##===--- swift-bench.py -------------------------------*- coding: utf-8 -*-===##
##
## This source file is part of the Swift.org open source project
##
## Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
## Licensed under Apache License v2.0 with Runtime Library Exception
##
## See http://swift.org/LICENSE.txt for license information
## See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
##
##===----------------------------------------------------------------------===##

# This file implements a test harness for running Swift performance benchmarks.
#
# Its input is a set of swift files, containing functions named 'bench_*' that
# take no arguments and returns Int. The harness makes a separate test from
# each of these functions, runs all the tests and reports aggregate results.
#
# The workflow of the harness is the following:
#   o Basing on the input files, generate 'processed' files. These files contain
#     a main function with simple arguments parsing, time measurement utilities
#     and a loop in which the bench-functions are called.
#   o When all files are processed, the harness begins to compile them, keeping
#     track of all compile fails for later results reporting.
#   o When all files are compiled, the harness begins to run the tests. The
#     harness chooses a number of iterations for each tests to achieve the best
#     accuracy in the given time limit (in order to do that, it performs several
#     auxiliary test runs). When the iteration number is chosen, the measurement
#     of execution time is actually performed.
#   o At this point everything is ready, and the harness simply reports the
#     results.
#
# Ideas for the harness improvement and development are welcomed here:
# rdar://problem/18072938

from __future__ import print_function

import subprocess
import numpy
import re
import os
import sys
import argparse


class SwiftBenchHarness:
  sources = []
  verboseLevel = 0
  compiler = ""
  tests = {}
  timeLimit = 1000
  minSampleTime = 100
  minIterTime = 1
  optFlags = []


  def log(self, str, level):
    if self.verboseLevel >= level:
      for i in range(1,level):
        sys.stdout.write('  ')
      print(str)


  def runCommand(self, cmd):
    self.log('    Executing: ' + ' '.join(cmd), 1)
    return subprocess.check_output(cmd, stderr=subprocess.STDOUT)


  def parseArguments(self):
    self.log("Parsing arguments.", 2)
    parser = argparse.ArgumentParser()
    parser.add_argument("-v", "--verbosity", help="increase output verbosity", type=int)
    parser.add_argument("files", help="input files", nargs='+')
    parser.add_argument('-c', '--compiler', help="compiler to use")
    parser.add_argument('-t', '--timelimit', help="Time limit for every test", type=int)
    parser.add_argument('-s', '--sampletime', help="Minimum time for every sample", type=int)
    parser.add_argument('-f', '--flags', help="Compilation flags", nargs='+')
    args = parser.parse_args()
    if args.verbosity:
      self.verboseLevel = args.verbosity
    self.sources = args.files
    if args.flags:
      self.optFlags = args.flags
    if args.compiler:
      self.compiler = args.compiler
    else:
      self.compiler = 'swiftc'
    if args.timelimit and args.timelimit > 0:
      self.timeLimit = args.timelimit
    if args.sampletime and args.sampletime > 0:
      self.minSampleTime = args.sampletime
    self.log("Sources: %s." % ', '.join(self.sources), 3)
    self.log("Compiler: %s." % self.compiler, 3)
    self.log("Opt flags: %s." % ', '.join(self.optFlags), 3)
    self.log("Verbosity: %s." % self.verboseLevel, 3)
    self.log("Time limit: %s." % self.timeLimit, 3)
    self.log("Min sample time: %s." % self.minSampleTime, 3)


  def processSource(self, name):
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
#elseif arch(x86_64) || arch(arm64)
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
    beforeBench = """
@inline(never)
"""
    intoBench = """
  if False() { return 0 }
"""
    mainBegin = """
func main() {
  var N = 1
  var name = ""
  if Process.arguments.count > 1 {
    N = Process.arguments[1].toInt()!
  }
"""
    mainBody = """
  name = "%s"
  if Process.arguments.count <= 2 || Process.arguments[2] == name {
    let start = __mach_absolute_time__()
    for _ in 1...N {
      bench_%s()
    }
    let end = __mach_absolute_time__()
    println("\(name),\(N),\(end - start)")
  }
"""
    mainEnd = """
}
main()
"""

    benchRE = re.compile("^\s*func\s\s*bench_([a-zA-Z0-9_]+)\s*\(\s*\)\s*->\s*Int\s*({)?\s*$")
    with open(name) as f:
      lines = list(f)
    output = header
    lookingForCurlyBrace = False
    testNames = []
    for l in lines:
      if lookingForCurlyBrace:
        output += l
        if "{" not in l:
          continue
        lookingForCurlyBrace = False
        output += intoBench
        continue

      m = benchRE.match(l)
      if m:
        output += beforeBench
        output += l
        benchName = m.group(1)
        # TODO: Keep track of the line number as well
        self.log("Benchmark found: %s" % benchName, 3)
        self.tests[name+":"+benchName] = Test(benchName, name, "", "")
        testNames.append(benchName)
        if m.group(2):
          output += intoBench
        else:
          lookingForCurlyBrace = True
      else:
        output += l

    output += mainBegin
    for n in testNames:
      output += mainBody % (n, n)
    processedName = 'processed_' + os.path.basename(name)
    output += mainEnd
    with open(processedName, 'w') as f:
      f.write(output)
    for n in testNames:
      self.tests[name+":"+n].processedSource = processedName


  def processSources(self):
    self.log("Processing sources: %s." % self.sources, 2)
    for s in self.sources:
      self.processSource(s)


  def compileOpaqueCFile(self):
    self.log("Generating and compiling C file with opaque functions.", 3)
    fileBody = """
#include <stdint.h>
extern "C" int32_t opaqueGetInt32(int32_t x) { return x; }
extern "C" int64_t opaqueGetInt64(int64_t x) { return x; }
"""
    with open('opaque.cpp', 'w') as f:
      f.write(fileBody)
    # TODO: Handle subprocess.CalledProcessError for this call:
    self.runCommand(['clang++', 'opaque.cpp', '-o', 'opaque.o', '-c', '-O2'])

  compiledFiles = {}
  def compileSource(self, name):
    self.tests[name].binary = "./"+self.tests[name].processedSource.split(os.extsep)[0]
    if not self.tests[name].processedSource in self.compiledFiles:
      try:
        self.runCommand([self.compiler, self.tests[name].processedSource, "-o", self.tests[name].binary + '.o', '-c'] + self.optFlags)
        self.runCommand([self.compiler, '-o', self.tests[name].binary, self.tests[name].binary + '.o', 'opaque.o'])
        self.compiledFiles[self.tests[name].processedSource] = ('', '')
      except subprocess.CalledProcessError as e:
        self.compiledFiles[self.tests[name].processedSource] = ('COMPFAIL', e.output)

    (status, output) = self.compiledFiles[self.tests[name].processedSource]
    self.tests[name].status = status
    self.tests[name].output = output


  def compileSources(self):
    self.log("Compiling processed sources.", 2)
    self.compileOpaqueCFile()
    for t in self.tests:
      self.compileSource(t)


  def runBenchmarks(self):
    self.log("Running benchmarks.", 2)
    for t in self.tests:
      self.runBench(t)


  def parseBenchmarkOutput(self, res):
    # Parse lines like
    # TestName,NNN,MMM
    # where NNN - performed iterations number, MMM - execution time (in ns)
    RESULTS_RE = re.compile(r"(\w+),[ \t]*(\d+),[ \t]*(\d+)")
    m = RESULTS_RE.match(res)
    if not m:
      return ("", 0, 0)
    return (m.group(1), m.group(2), m.group(3))


  def computeItersNumber(self, name):
    scale = 1
    spent = 0
    # Measure time for one iteration
    # If it's too small, increase number of iteration until it's measurable
    while (spent <= self.minIterTime):
      try:
        r = self.runCommand([self.tests[name].binary, str(scale),
                             self.tests[name].name])
        (testName, itersComputed, execTime) = self.parseBenchmarkOutput(r)
        spent = int(execTime) / 1000000 # Convert ns to ms
        if spent <= self.minIterTime:
          scale *= 2
        if scale > sys.maxint:
          return (0, 0)
      except subprocess.CalledProcessError as e:
        r = e.output
        break
    if spent == 0:
      spent = 1
    # Now compute number of samples we can take in the given time limit
    mult = int(self.minSampleTime / spent)
    if mult == 0:
      mult = 1
    scale *= mult
    spent *= mult
    samples = int(self.timeLimit / spent)
    if samples == 0:
      samples = 1
    return (samples, scale)


  def runBench(self, name):
    if not self.tests[name].status == "":
      return
    (numSamples, iterScale) = self.computeItersNumber(name)
    if (numSamples, iterScale) == (0, 0):
      self.tests[name].status = "CAN'T MEASURE"
      self.tests[name].output = "Can't find number of iterations for the test to last longer than %d ms." % self.minIterTime
      return
    samples = []
    self.log("Running bench: %s, numsamples: %d" % (name, numSamples), 2)
    for i in range(0,numSamples):
      try:
        r = self.runCommand([self.tests[name].binary, str(iterScale),
                             self.tests[name].name])
        (testName, itersComputed, execTime) = self.parseBenchmarkOutput(r)
        # TODO: Verify testName and itersComputed
        samples.append(int(execTime) / iterScale)
        self.tests[name].output = r
      except subprocess.CalledProcessError as e:
        self.tests[name].status = "RUNFAIL"
        self.tests[name].output = e.output
        break
    res = TestResults(name, samples)
    self.tests[name].results = res


  def reportResults(self):
    self.log("\nReporting results.", 2)
    print("==================================================")
    for t in self.tests:
      self.tests[t].Print()


class Test:
  def __init__(self, name, source, processedSource, binary):
    self.name = name
    self.source = source
    self.processedSource = processedSource
    self.binary = binary
    self.status = ""
  def Print(self):
    print("NAME: %s" % self.name)
    print("SOURCE: %s" % self.source)
    if self.status == "":
      self.results.Print()
    else:
      print("STATUS: %s" % self.status)
      print("OUTPUT:")
      print(self.output)
      print("END OF OUTPUT")
    print("")


class TestResults:
  def __init__(self, name, samples):
    self.name = name
    self.samples = samples
    if len(samples) > 0:
      self.Process()
  def Process(self):
    self.minimum = min(self.samples)
    self.maximum = max(self.samples)
    self.avg = sum(self.samples)/len(self.samples)
    self.std = numpy.std(self.samples)
    self.err = self.std/numpy.sqrt(len(self.samples))
    self.int_min = self.avg - self.err*1.96
    self.int_max = self.avg + self.err*1.96
  def Print(self):
    print("SAMPLES: %d" % len(self.samples))
    print("MIN: %3.2e" % self.minimum)
    print("MAX: %3.2e" % self.maximum)
    print("AVG: %3.2e" % self.avg)
    print("STD: %3.2e" % self.std)
    print("ERR: %3.2e (%2.1f%%)" % (self.err, self.err*100/self.avg))
    print("CONF INT 0.95: (%3.2e, %3.2e)" % (self.int_min, self.int_max))
    print("")


def main():
  harness = SwiftBenchHarness()
  harness.parseArguments()
  harness.processSources()
  harness.compileSources()
  harness.runBenchmarks()
  harness.reportResults()


main()
