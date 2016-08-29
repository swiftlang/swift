#!/usr/bin/env python

"""
lit - LLVM Integrated Tester.

See lit.pod for more information.
"""

from __future__ import absolute_import
import math, os, platform, random, re, sys, time

import lit.ProgressBar
import lit.LitConfig
import lit.Test
import lit.run
import lit.util
import lit.discovery

class TestingProgressDisplay(object):
    def __init__(self, opts, numTests, progressBar=None):
        self.opts = opts
        self.numTests = numTests
        self.current = None
        self.progressBar = progressBar
        self.completed = 0

    def finish(self):
        if self.progressBar:
            self.progressBar.clear()
        elif self.opts.quiet:
            pass
        elif self.opts.succinct:
            sys.stdout.write('\n')

    def update(self, test):
        self.completed += 1

        if self.opts.incremental:
            update_incremental_cache(test)

        if self.progressBar:
            self.progressBar.update(float(self.completed)/self.numTests,
                                    test.getFullName())

        shouldShow = test.result.code.isFailure or \
            self.opts.showAllOutput or \
            (not self.opts.quiet and not self.opts.succinct)
        if not shouldShow:
            return

        if self.progressBar:
            self.progressBar.clear()

        # Show the test result line.
        test_name = test.getFullName()
        print('%s: %s (%d of %d)' % (test.result.code.name, test_name,
                                     self.completed, self.numTests))

        # Show the test failure output, if requested.
        if (test.result.code.isFailure and self.opts.showOutput) or \
           self.opts.showAllOutput:
            if test.result.code.isFailure:
                print("%s TEST '%s' FAILED %s" % ('*'*20, test.getFullName(),
                                                  '*'*20))
            print(test.result.output)
            print("*" * 20)

        # Report test metrics, if present.
        if test.result.metrics:
            print("%s TEST '%s' RESULTS %s" % ('*'*10, test.getFullName(),
                                               '*'*10))
            items = sorted(test.result.metrics.items())
            for metric_name, value in items:
                print('%s: %s ' % (metric_name, value.format()))
            print("*" * 10)

        # Ensure the output is flushed.
        sys.stdout.flush()

def write_test_results(run, lit_config, testing_time, output_path):
    try:
        import json
    except ImportError:
        lit_config.fatal('test output unsupported with Python 2.5')

    # Construct the data we will write.
    data = {}
    # Encode the current lit version as a schema version.
    data['__version__'] = lit.__versioninfo__
    data['elapsed'] = testing_time
    # FIXME: Record some information on the lit configuration used?
    # FIXME: Record information from the individual test suites?

    # Encode the tests.
    data['tests'] = tests_data = []
    for test in run.tests:
        test_data = {
            'name' : test.getFullName(),
            'code' : test.result.code.name,
            'output' : test.result.output,
            'elapsed' : test.result.elapsed }

        # Add test metrics, if present.
        if test.result.metrics:
            test_data['metrics'] = metrics_data = {}
            for key, value in test.result.metrics.items():
                metrics_data[key] = value.todata()

        tests_data.append(test_data)

    # Write the output.
    f = open(output_path, 'w')
    try:
        json.dump(data, f, indent=2, sort_keys=True)
        f.write('\n')
    finally:
        f.close()

def update_incremental_cache(test):
    if not test.result.code.isFailure:
        return
    fname = test.getFilePath()
    os.utime(fname, None)

def sort_by_incremental_cache(run):
    def sortIndex(test):
        fname = test.getFilePath()
        try:
            return -os.path.getmtime(fname)
        except:
            return 0
    run.tests.sort(key = lambda t: sortIndex(t))

def main(builtinParameters = {}):
    # Use processes by default on Unix platforms.
    isWindows = platform.system() == 'Windows'
    useProcessesIsDefault = not isWindows

    global options
    from optparse import OptionParser, OptionGroup
    parser = OptionParser("usage: %prog [options] {file-or-path}")

    parser.add_option("", "--version", dest="show_version",
                      help="Show version and exit",
                      action="store_true", default=False)
    parser.add_option("-j", "--threads", dest="numThreads", metavar="N",
                      help="Number of testing threads",
                      type=int, action="store", default=None)
    parser.add_option("", "--config-prefix", dest="configPrefix",
                      metavar="NAME", help="Prefix for 'lit' config files",
                      action="store", default=None)
    parser.add_option("-D", "--param", dest="userParameters",
                      metavar="NAME=VAL",
                      help="Add 'NAME' = 'VAL' to the user defined parameters",
                      type=str, action="append", default=[])

    group = OptionGroup(parser, "Output Format")
    # FIXME: I find these names very confusing, although I like the
    # functionality.
    group.add_option("-q", "--quiet", dest="quiet",
                     help="Suppress no error output",
                     action="store_true", default=False)
    group.add_option("-s", "--succinct", dest="succinct",
                     help="Reduce amount of output",
                     action="store_true", default=False)
    group.add_option("-v", "--verbose", dest="showOutput",
                     help="Show test output for failures",
                     action="store_true", default=False)
    group.add_option("-a", "--show-all", dest="showAllOutput",
                     help="Display all commandlines and output",
                     action="store_true", default=False)
    group.add_option("-o", "--output", dest="output_path",
                     help="Write test results to the provided path",
                     action="store", type=str, metavar="PATH")
    group.add_option("", "--no-progress-bar", dest="useProgressBar",
                     help="Do not use curses based progress bar",
                     action="store_false", default=True)
    group.add_option("", "--show-unsupported", dest="show_unsupported",
                     help="Show unsupported tests",
                     action="store_true", default=False)
    group.add_option("", "--show-xfail", dest="show_xfail",
                     help="Show tests that were expected to fail",
                     action="store_true", default=False)
    parser.add_option_group(group)

    group = OptionGroup(parser, "Test Execution")
    group.add_option("", "--path", dest="path",
                     help="Additional paths to add to testing environment",
                     action="append", type=str, default=[])
    group.add_option("", "--vg", dest="useValgrind",
                     help="Run tests under valgrind",
                     action="store_true", default=False)
    group.add_option("", "--vg-leak", dest="valgrindLeakCheck",
                     help="Check for memory leaks under valgrind",
                     action="store_true", default=False)
    group.add_option("", "--vg-arg", dest="valgrindArgs", metavar="ARG",
                     help="Specify an extra argument for valgrind",
                     type=str, action="append", default=[])
    group.add_option("", "--time-tests", dest="timeTests",
                     help="Track elapsed wall time for each test",
                     action="store_true", default=False)
    group.add_option("", "--no-execute", dest="noExecute",
                     help="Don't execute any tests (assume PASS)",
                     action="store_true", default=False)
    group.add_option("", "--xunit-xml-output", dest="xunit_output_file",
                      help=("Write XUnit-compatible XML test reports to the"
                            " specified file"), default=None)
    group.add_option("", "--timeout", dest="maxIndividualTestTime",
                     help="Maximum time to spend running a single test (in seconds)."
                     "0 means no time limit. [Default: 0]",
                    type=int, default=None)
    parser.add_option_group(group)

    group = OptionGroup(parser, "Test Selection")
    group.add_option("", "--max-tests", dest="maxTests", metavar="N",
                     help="Maximum number of tests to run",
                     action="store", type=int, default=None)
    group.add_option("", "--max-time", dest="maxTime", metavar="N",
                     help="Maximum time to spend testing (in seconds)",
                     action="store", type=float, default=None)
    group.add_option("", "--shuffle", dest="shuffle",
                     help="Run tests in random order",
                     action="store_true", default=False)
    group.add_option("-i", "--incremental", dest="incremental",
                     help="Run modified and failing tests first (updates "
                     "mtimes)",
                     action="store_true", default=False)
    group.add_option("", "--filter", dest="filter", metavar="REGEX",
                     help=("Only run tests with paths matching the given "
                           "regular expression"),
                     action="store", default=None)
    parser.add_option_group(group)

    group = OptionGroup(parser, "Debug and Experimental Options")
    group.add_option("", "--debug", dest="debug",
                      help="Enable debugging (for 'lit' development)",
                      action="store_true", default=False)
    group.add_option("", "--show-suites", dest="showSuites",
                      help="Show discovered test suites",
                      action="store_true", default=False)
    group.add_option("", "--show-tests", dest="showTests",
                      help="Show all discovered tests",
                      action="store_true", default=False)
    group.add_option("", "--use-processes", dest="useProcesses",
                      help="Run tests in parallel with processes (not threads)",
                      action="store_true", default=useProcessesIsDefault)
    group.add_option("", "--use-threads", dest="useProcesses",
                      help="Run tests in parallel with threads (not processes)",
                      action="store_false", default=useProcessesIsDefault)
    parser.add_option_group(group)

    (opts, args) = parser.parse_args()

    if opts.show_version:
        print("lit %s" % (lit.__version__,))
        return

    if not args:
        parser.error('No inputs specified')

    if opts.numThreads is None:
# Python <2.5 has a race condition causing lit to always fail with numThreads>1
# http://bugs.python.org/issue1731717
# I haven't seen this bug occur with 2.5.2 and later, so only enable multiple
# threads by default there.
       if sys.hexversion >= 0x2050200:
               opts.numThreads = lit.util.detectCPUs()
       else:
               opts.numThreads = 1

    inputs = args

    # Create the user defined parameters.
    userParams = dict(builtinParameters)
    for entry in opts.userParameters:
        if '=' not in entry:
            name,val = entry,''
        else:
            name,val = entry.split('=', 1)
        userParams[name] = val

    # Decide what the requested maximum indvidual test time should be
    if opts.maxIndividualTestTime != None:
        maxIndividualTestTime = opts.maxIndividualTestTime
    else:
        # Default is zero
        maxIndividualTestTime = 0


    # Create the global config object.
    litConfig = lit.LitConfig.LitConfig(
        progname = os.path.basename(sys.argv[0]),
        path = opts.path,
        quiet = opts.quiet,
        useValgrind = opts.useValgrind,
        valgrindLeakCheck = opts.valgrindLeakCheck,
        valgrindArgs = opts.valgrindArgs,
        noExecute = opts.noExecute,
        debug = opts.debug,
        isWindows = isWindows,
        params = userParams,
        config_prefix = opts.configPrefix,
        maxIndividualTestTime = maxIndividualTestTime)

    # Perform test discovery.
    run = lit.run.Run(litConfig,
                      lit.discovery.find_tests_for_inputs(litConfig, inputs))

    # After test discovery the configuration might have changed
    # the maxIndividualTestTime. If we explicitly set this on the
    # command line then override what was set in the test configuration
    if opts.maxIndividualTestTime != None:
        if opts.maxIndividualTestTime != litConfig.maxIndividualTestTime:
            litConfig.note(('The test suite configuration requested an individual'
                ' test timeout of {0} seconds but a timeout of {1} seconds was'
                ' requested on the command line. Forcing timeout to be {1}'
                ' seconds')
                .format(litConfig.maxIndividualTestTime,
                        opts.maxIndividualTestTime))
            litConfig.maxIndividualTestTime = opts.maxIndividualTestTime

    if opts.showSuites or opts.showTests:
        # Aggregate the tests by suite.
        suitesAndTests = {}
        for result_test in run.tests:
            if result_test.suite not in suitesAndTests:
                suitesAndTests[result_test.suite] = []
            suitesAndTests[result_test.suite].append(result_test)
        suitesAndTests = list(suitesAndTests.items())
        suitesAndTests.sort(key = lambda item: item[0].name)

        # Show the suites, if requested.
        if opts.showSuites:
            print('-- Test Suites --')
            for ts,ts_tests in suitesAndTests:
                print('  %s - %d tests' %(ts.name, len(ts_tests)))
                print('    Source Root: %s' % ts.source_root)
                print('    Exec Root  : %s' % ts.exec_root)

        # Show the tests, if requested.
        if opts.showTests:
            print('-- Available Tests --')
            for ts,ts_tests in suitesAndTests:
                ts_tests.sort(key = lambda test: test.path_in_suite)
                for test in ts_tests:
                    print('  %s' % (test.getFullName(),))

        # Exit.
        sys.exit(0)

    # Select and order the tests.
    numTotalTests = len(run.tests)

    # First, select based on the filter expression if given.
    if opts.filter:
        try:
            rex = re.compile(opts.filter)
        except:
            parser.error("invalid regular expression for --filter: %r" % (
                    opts.filter))
        run.tests = [result_test for result_test in run.tests
                     if rex.search(result_test.getFullName())]

    # Then select the order.
    if opts.shuffle:
        random.shuffle(run.tests)
    elif opts.incremental:
        sort_by_incremental_cache(run)
    else:
        run.tests.sort(key = lambda result_test: result_test.getFullName())

    # Finally limit the number of tests, if desired.
    if opts.maxTests is not None:
        run.tests = run.tests[:opts.maxTests]

    # Don't create more threads than tests.
    opts.numThreads = min(len(run.tests), opts.numThreads)

    # Because some tests use threads internally, and at least on Linux each
    # of these threads counts toward the current process limit, try to
    # raise the (soft) process limit so that tests don't fail due to
    # resource exhaustion.
    try:
        cpus = lit.util.detectCPUs()
        desired_limit = opts.numThreads * cpus * 2 # the 2 is a safety factor

        # Import the resource module here inside this try block because it
        # will likely fail on Windows.
        import resource

        max_procs_soft, max_procs_hard = resource.getrlimit(resource.RLIMIT_NPROC)
        desired_limit = min(desired_limit, max_procs_hard)

        if max_procs_soft < desired_limit:
            resource.setrlimit(resource.RLIMIT_NPROC, (desired_limit, max_procs_hard))
            litConfig.note('raised the process limit from %d to %d' % \
                               (max_procs_soft, desired_limit))
    except:
        pass

    extra = ''
    if len(run.tests) != numTotalTests:
        extra = ' of %d' % numTotalTests
    header = '-- Testing: %d%s tests, %d threads --'%(len(run.tests), extra,
                                                      opts.numThreads)
    progressBar = None
    if not opts.quiet:
        if opts.succinct and opts.useProgressBar:
            try:
                tc = lit.ProgressBar.TerminalController()
                progressBar = lit.ProgressBar.ProgressBar(tc, header)
            except ValueError:
                print(header)
                progressBar = lit.ProgressBar.SimpleProgressBar('Testing: ')
        else:
            print(header)

    startTime = time.time()
    display = TestingProgressDisplay(opts, len(run.tests), progressBar)
    try:
        run.execute_tests(display, opts.numThreads, opts.maxTime,
                          opts.useProcesses)
    except KeyboardInterrupt:
        sys.exit(2)
    display.finish()

    testing_time = time.time() - startTime
    if not opts.quiet:
        print('Testing Time: %.2fs' % (testing_time,))

    # Write out the test data, if requested.
    if opts.output_path is not None:
        write_test_results(run, litConfig, testing_time, opts.output_path)

    # List test results organized by kind.
    hasFailures = False
    byCode = {}
    for test in run.tests:
        if test.result.code not in byCode:
            byCode[test.result.code] = []
        byCode[test.result.code].append(test)
        if test.result.code.isFailure:
            hasFailures = True

    # Print each test in any of the failing groups.
    for title,code in (('Unexpected Passing Tests', lit.Test.XPASS),
                       ('Failing Tests', lit.Test.FAIL),
                       ('Unresolved Tests', lit.Test.UNRESOLVED),
                       ('Unsupported Tests', lit.Test.UNSUPPORTED),
                       ('Expected Failing Tests', lit.Test.XFAIL),
                       ('Timed Out Tests', lit.Test.TIMEOUT)):
        if (lit.Test.XFAIL == code and not opts.show_xfail) or \
           (lit.Test.UNSUPPORTED == code and not opts.show_unsupported):
            continue
        elts = byCode.get(code)
        if not elts:
            continue
        print('*'*20)
        print('%s (%d):' % (title, len(elts)))
        for test in elts:
            print('    %s' % test.getFullName())
        sys.stdout.write('\n')

    if opts.timeTests and run.tests:
        # Order by time.
        test_times = [(test.getFullName(), test.result.elapsed)
                      for test in run.tests]
        lit.util.printHistogram(test_times, title='Tests')

    for name,code in (('Expected Passes    ', lit.Test.PASS),
                      ('Passes With Retry  ', lit.Test.FLAKYPASS),
                      ('Expected Failures  ', lit.Test.XFAIL),
                      ('Unsupported Tests  ', lit.Test.UNSUPPORTED),
                      ('Unresolved Tests   ', lit.Test.UNRESOLVED),
                      ('Unexpected Passes  ', lit.Test.XPASS),
                      ('Unexpected Failures', lit.Test.FAIL),
                      ('Individual Timeouts', lit.Test.TIMEOUT)):
        if opts.quiet and not code.isFailure:
            continue
        N = len(byCode.get(code,[]))
        if N:
            print('  %s: %d' % (name,N))

    if opts.xunit_output_file:
        # Collect the tests, indexed by test suite
        by_suite = {}
        for result_test in run.tests:
            suite = result_test.suite.config.name
            if suite not in by_suite:
                by_suite[suite] = {
                                   'passes'   : 0,
                                   'failures' : 0,
                                   'tests'    : [] }
            by_suite[suite]['tests'].append(result_test)
            if result_test.result.code.isFailure:
                by_suite[suite]['failures'] += 1
            else:
                by_suite[suite]['passes'] += 1
        xunit_output_file = open(opts.xunit_output_file, "w")
        xunit_output_file.write("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n")
        xunit_output_file.write("<testsuites>\n")
        for suite_name, suite in by_suite.items():
            safe_suite_name = suite_name.replace(".", "-")
            xunit_output_file.write("<testsuite name='" + safe_suite_name + "'")
            xunit_output_file.write(" tests='" + str(suite['passes'] + 
              suite['failures']) + "'")
            xunit_output_file.write(" failures='" + str(suite['failures']) + 
              "'>\n")
            for result_test in suite['tests']:
                xunit_output_file.write(result_test.getJUnitXML() + "\n")
            xunit_output_file.write("</testsuite>\n")
        xunit_output_file.write("</testsuites>")
        xunit_output_file.close()

    # If we encountered any additional errors, exit abnormally.
    if litConfig.numErrors:
        sys.stderr.write('\n%d error(s), exiting.\n' % litConfig.numErrors)
        sys.exit(2)

    # Warn about warnings.
    if litConfig.numWarnings:
        sys.stderr.write('\n%d warning(s) in tests.\n' % litConfig.numWarnings)

    if hasFailures:
        sys.exit(1)
    sys.exit(0)

if __name__=='__main__':
    main()
