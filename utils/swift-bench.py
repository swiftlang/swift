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
#     auxiliary test runs). When the iteration number is chosen, the measurent
#     of execution time is actually performed.
#   o At this point everything is ready, and the harness simply reports the
#     results.
#
# Ideas for the harness improvement and development are welcomed here:
# rdar://problem/18072938

import subprocess
import numpy
import time
import re
import os
import sys

# TODO: Wrap into a class
sources = []
verboseLevel = 10
compiler = ""
tests = {}


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



def log(str, level = 1):
  if verboseLevel >= level:
    for i in range(1,level):
      sys.stdout.write('  ')
    print(str)


def parseArguments():
  global compiler
  global sources
  log("Parsing arguments.", 2)
  # This routien is not implemented yet.
  # Hardcode some values for testing purposes.
  sources.append("bench.swift")
  sources.append("bench2.swift")
  sources.append("benchCompFail.swift")
  sources.append("benchAbort.swift")
  sources.append("benchMulti.swift")
  compiler = "/Users/michaelzolotukhin/devel/swift/build/Ninja-RelWithDebInfo/swift/bin/swiftc"
  log("Sources: %s." % sources, 3)


def processSource(name):
  global tests
  log("Processing source file: %s." % name, 2)

  header = """
@asmname("mach_absolute_time") func __mach_absolute_time__() -> UInt64
@inline(never)
func False() -> Bool { return false }
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
  f = open(name)
  lines = f.readlines()
  f.close()
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
      log("Benchmark found: %s" % benchName, 3)
      tests[name+":"+benchName] = Test(benchName, name, "", "")
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
  processedName = 'processed_'+name
  output += mainEnd
  f = open(processedName, 'w')
  f.write(output)
  f.close()
  for n in testNames:
    tests[name+":"+n].processedSource = processedName


def processSources():
  log("Processing sources: %s." % sources, 2)
  for s in sources:
    processSource(s)


def compileSources():
  log("Compiling processed sources.", 2)
  for t in tests:
    tests[t].binary = "./"+tests[t].processedSource.split(os.extsep)[0]
    try:
      log("Executing '%s %s -o %s'" % (compiler, tests[t].processedSource, tests[t].binary), 3)
      r = subprocess.check_output([compiler, tests[t].processedSource, "-o", tests[t].binary], stderr=subprocess.STDOUT)
    except subprocess.CalledProcessError as e:
      tests[t].output = e.output
      tests[t].status = "COMPFAIL"


def runBenchmarks():
  log("Running benchmarks.", 2)
  for t in tests:
    runBench(t)


def parseBenchmarkOutput(res):
  # Parse lines like
  # TestName,NNN,MMM
  # where NNN - performed iterations number, MMM - execution time (in ns)
  RESULTS_RE = re.compile(r"(\w+),[ \t]*(\d+),[ \t]*(\d+)")
  m = RESULTS_RE.match(res)
  if not m:
    return ("", 0, 0)
  return (m.group(1), m.group(2), m.group(3))


def computeItersNumber(name):
  timeLimit = 1000
  itertime = 100
  mintime = 1
  scale = 1
  spent = 0
  # Mesaure time for one iteration
  # If it's too small, increase number of iteration until it's measurable
  while (spent <= mintime):
    try:
      r = subprocess.check_output([tests[name].binary, str(scale),
                                   tests[name].name], stderr=subprocess.STDOUT)
      (testName, itersComputed, execTime) = parseBenchmarkOutput(r)
      spent = int(execTime) / 1000000 # Convert ns to ms
      if spent <= mintime:
        scale *= 2
    except subprocess.CalledProcessError as e:
      r = e.output
      break
  if spent == 0:
    spent = 1
  # Now compute number of samples we can take in the given time limit
  mult = int(itertime / spent)
  if mult == 0:
    mult = 1
  scale *= mult
  spent *= mult
  samples = int(timeLimit / spent)
  if samples == 0:
    samples = 1
  return (samples, scale)


def runBench(name):
  if not tests[name].status == "":
    return
  (numSamples, iterScale) = computeItersNumber(name)
  samples = []
  log("Running bench: %s, numsamples: %d" % (name, numSamples), 2)
  output = ""
  for i in range(0,numSamples):
    try:
      r = subprocess.check_output([tests[name].binary, str(iterScale),
                                   tests[name].name], stderr=subprocess.STDOUT)
      (testName, itersComputed, execTime) = parseBenchmarkOutput(r)
      # TODO: Verify testName and itersComputed
      samples.append(int(execTime) / iterScale)
      tests[name].output = r
    except subprocess.CalledProcessError as e:
      tests[name].status = "RUNFAIL"
      tests[name].output = e.output
      break
  res = TestResults(name, samples)
  tests[name].results = res


def reportResults():
  log("\nReporting results.", 2)
  print("==================================================")
  for t in tests:
    tests[t].Print()


def main():
  parseArguments()
  processSources()
  compileSources()
  runBenchmarks()
  reportResults()


main()
