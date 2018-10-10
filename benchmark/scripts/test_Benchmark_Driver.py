#!/usr/bin/python
# -*- coding: utf-8 -*-

# ===--- test_Benchmark_Driver.py ----------------------------------------===//
#
#  This source file is part of the Swift.org open source project
#
#  Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
#  Licensed under Apache License v2.0 with Runtime Library Exception
#
#  See https://swift.org/LICENSE.txt for license information
#  See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ===---------------------------------------------------------------------===//

import logging
import os
import time
import unittest

from imp import load_source

from compare_perf_tests import PerformanceTestResult

from test_utils import Mock, MockLoggingHandler, Stub, captured_output

# import Benchmark_Driver  # doesn't work because it misses '.py' extension
Benchmark_Driver = load_source(
    'Benchmark_Driver', os.path.join(os.path.dirname(
        os.path.abspath(__file__)), 'Benchmark_Driver'))
# from Benchmark_Driver import parse_args
parse_args = Benchmark_Driver.parse_args
BenchmarkDriver = Benchmark_Driver.BenchmarkDriver
BenchmarkDoctor = Benchmark_Driver.BenchmarkDoctor
LoggingReportFormatter = Benchmark_Driver.LoggingReportFormatter


class Test_parse_args(unittest.TestCase):
    def assert_contains(self, texts, output):
        assert not isinstance(texts, str)
        for text in texts:
            self.assertIn(text, output)

    def test_requires_command_argument(self):
        with captured_output() as (_, err):
            self.assertRaises(SystemExit, parse_args, [])
        self.assert_contains(['usage:', 'COMMAND', 'too few arguments'],
                             err.getvalue())

    def test_command_help_lists_commands(self):
        with captured_output() as (out, _):
            self.assertRaises(SystemExit, parse_args, ['-h'])
        self.assert_contains(['COMMAND', 'run', 'compare', 'check'],
                             out.getvalue())

    def test_run_benchmarks_by_name_or_ordinal(self):
        benchmarks = ['AngryPhonebook', '42']
        self.assertEquals(
            parse_args(['run'] + benchmarks).benchmarks, benchmarks)

    def test_run_benchmarks_matching_pattern(self):
        regexes = ['Prefix', '.*Suffix.*']
        filters = ['-f', regexes[0], '-f', regexes[1]]
        self.assertEquals(parse_args(['run'] + filters).filters, regexes)

    def test_run_benchmarks_and_filters_are_exclusive(self):
        with captured_output() as (_, err):
            self.assertRaises(SystemExit,
                              parse_args, 'run -f Filter1 Benchmark1'.split())
        self.assert_contains(
            ['error',
             'argument BENCHMARK: not allowed with argument -f/--filter'],
            err.getvalue())

    def test_tests_location(self):
        here = os.path.dirname(os.path.abspath(__file__))
        self.assertEquals(parse_args(['run']).tests, here)
        tests = '/benchmarks/are/here'
        self.assertEquals(parse_args(['run', '-t', tests]).tests, tests)

    def test_optimization_argument(self):
        self.assertEquals(parse_args(['run']).optimization, 'O')
        self.assertEquals(
            parse_args(['run', '-o', 'O']).optimization, 'O')
        self.assertEquals(
            parse_args(['run', '-o', 'Onone']).optimization, 'Onone')
        self.assertEquals(
            parse_args(['run', '-o', 'Osize']).optimization, 'Osize')

        with captured_output() as (_, err):
            self.assertRaises(SystemExit,
                              parse_args, ['run', '-o', 'bogus'])
        self.assert_contains(
            ['error:',
             "argument -o/--optimization: invalid choice: 'bogus'",
             "(choose from 'O', 'Onone', 'Osize')"],
            err.getvalue())

    def test_independent_samples(self):
        self.assertEquals(parse_args(['run']).independent_samples, 1)
        self.assertEquals(parse_args(['run', '-i', '3']).independent_samples,
                          3)
        with captured_output() as (out, err):
            self.assertRaises(SystemExit,
                              parse_args, ['run', '-i', '-3'])
        self.assert_contains(
            ['error:', "argument -i/--independent-samples: " +
             "invalid positive_int value: '-3'"],
            err.getvalue())

    def test_output_dir(self):
        self.assertIsNone(parse_args(['run']).output_dir)
        self.assertEquals(
            parse_args(['run', '--output-dir', '/log']).output_dir, '/log')

    def test_check_supports_vebose_output(self):
        self.assertFalse(parse_args(['check']).verbose)
        self.assertTrue(parse_args(['check', '-v']).verbose)
        self.assertTrue(parse_args(['check', '--verbose']).verbose)


class ArgsStub(object):
    def __init__(self):
        self.benchmarks = None
        self.filters = None
        self.tests = '/benchmarks/'
        self.optimization = 'O'


class SubprocessMock(Mock):
    """Mock for subprocess module's `check_output` method."""
    STDOUT = object()

    def __init__(self, responses=None):
        super(SubprocessMock, self).__init__(responses)

        def _check_output(args, stdin=None, stdout=None, stderr=None,
                          shell=False):
            return self.record_and_respond(args, stdin, stdout, stderr, shell)
        self.check_output = _check_output

    def record_and_respond(self, args, stdin, stdout, stderr, shell):
        # _ = stdin, stdout, shell  # ignored in mock
        assert stderr == self.STDOUT, 'Errors are NOT redirected to STDOUT'
        args = tuple(args)
        self.calls.append(args)
        return self.respond.get(args, '')


class TestBenchmarkDriverInitialization(unittest.TestCase):
    def setUp(self):
        self.args = ArgsStub()
        self.subprocess_mock = SubprocessMock()

    def test_test_harness(self):
        self.assertEquals(
            BenchmarkDriver(self.args, tests=['ignored']).test_harness,
            '/benchmarks/Benchmark_O')
        self.args.tests = '/path'
        self.args.optimization = 'Suffix'
        self.assertEquals(
            BenchmarkDriver(self.args, tests=['ignored']).test_harness,
            '/path/Benchmark_Suffix')

    def test_gets_list_of_precommit_benchmarks(self):
        self.subprocess_mock.expect(
            '/benchmarks/Benchmark_O --list --delim=\t'.split(' '),
            '#\tTest\t[Tags]\n1\tBenchmark1\t[t1, t2]\n1\tBenchmark2\t[t3]\n')
        driver = BenchmarkDriver(
            self.args, _subprocess=self.subprocess_mock)
        self.subprocess_mock.assert_called_all_expected()
        self.assertEquals(driver.tests,
                          ['Benchmark1', 'Benchmark2'])
        self.assertEquals(driver.all_tests,
                          ['Benchmark1', 'Benchmark2'])

    list_all_tests = (
        '/benchmarks/Benchmark_O --list --delim=\t --skip-tags='.split(' '),
        """#	Test	[Tags]
1	Benchmark1	[t1, t2]
2	Benchmark2	[t3]
3	Benchmark3	[t3, t4]
""")

    def test_gets_list_of_all_benchmarks_when_benchmarks_args_exist(self):
        """Filters tests by name or test number, ignoring unknown."""
        self.args.benchmarks = '1 Benchmark3 1 bogus'.split()
        self.subprocess_mock.expect(*self.list_all_tests)
        driver = BenchmarkDriver(
            self.args, _subprocess=self.subprocess_mock)
        self.subprocess_mock.assert_called_all_expected()
        self.assertEquals(driver.tests, ['Benchmark1', 'Benchmark3'])
        self.assertEquals(driver.all_tests,
                          ['Benchmark1', 'Benchmark2', 'Benchmark3'])

    def test_filters_benchmarks_by_pattern(self):
        self.args.filters = '-f .+3'.split()
        self.subprocess_mock.expect(*self.list_all_tests)
        driver = BenchmarkDriver(
            self.args, _subprocess=self.subprocess_mock)
        self.subprocess_mock.assert_called_all_expected()
        self.assertEquals(driver.tests, ['Benchmark3'])
        self.assertEquals(driver.all_tests,
                          ['Benchmark1', 'Benchmark2', 'Benchmark3'])

    def test_log_file(self):
        """When swift-repo is set, log is tied to Git branch and revision."""
        self.assertIsNone(BenchmarkDriver(
            Stub(output_dir=None, tests='/bin/'), tests=['ignored']).log_file)

        now = time.strftime('%Y%m%d%H%M%S', time.localtime())
        driver = BenchmarkDriver(
            Stub(output_dir='/path', tests='/bin/', optimization='Suffix',
                 swift_repo=None,), tests=['ignored'])
        self.assertEquals(driver.log_file,
                          '/path/Benchmark_Suffix-' + now + '.log')

        r = '/repo/'
        subprocess_mock = SubprocessMock(responses=[
            ('git -C {0} rev-parse --abbrev-ref HEAD'.format(r).split(' '),
             'branch\n'),
            ('git -C {0} rev-parse --short HEAD'.format(r).split(' '),
             'short_hash\n'),
        ])
        driver = BenchmarkDriver(
            Stub(output_dir='/log/', tests='', optimization='S', swift_repo=r),
            tests=['ignored'], _subprocess=subprocess_mock)
        self.assertEquals(driver.log_file,
                          '/log/branch/Benchmark_S-' + now + '-short_hash.log')
        subprocess_mock.assert_called_all_expected()


class LogParserStub(object):
    results_from_string_called = False

    @staticmethod
    def results_from_string(log_contents):
        LogParserStub.results_from_string_called = True
        r = PerformanceTestResult('3,b1,1,123,123,123,0,123'.split(','))
        return {'b1': r}


class TestBenchmarkDriverRunningTests(unittest.TestCase):
    def setUp(self):
        self.args = ArgsStub()
        self.parser_stub = LogParserStub()
        self.subprocess_mock = SubprocessMock()
        self.subprocess_mock.expect(
            '/benchmarks/Benchmark_O --list --delim=\t'.split(' '),
            '#\tTest\t[Tags]\n1\tb1\t[tag]\n')
        self.driver = BenchmarkDriver(
            self.args, _subprocess=self.subprocess_mock,
            parser=self.parser_stub)

    def test_run_benchmark_with_multiple_samples(self):
        self.driver.run('b1')
        self.subprocess_mock.assert_called_with(
            ('/benchmarks/Benchmark_O', 'b1'))
        self.driver.run('b2', num_samples=5)
        self.subprocess_mock.assert_called_with(
            ('/benchmarks/Benchmark_O', 'b2', '--num-samples=5'))

    def test_run_benchmark_with_specified_number_of_iterations(self):
        self.driver.run('b', num_iters=1)
        self.subprocess_mock.assert_called_with(
            ('/benchmarks/Benchmark_O', 'b', '--num-iters=1'))

    def test_run_benchmark_in_verbose_mode(self):
        self.driver.run('b', verbose=True)
        self.subprocess_mock.assert_called_with(
            ('/benchmarks/Benchmark_O', 'b', '--verbose'))

    def test_parse_results_from_running_benchmarks(self):
        self.driver.run('b')
        self.assertTrue(self.parser_stub.results_from_string_called)

    def test_measure_memory(self):
        self.driver.run('b', measure_memory=True)
        self.subprocess_mock.assert_called_with(
            ('/benchmarks/Benchmark_O', 'b', '--memory'))

    def test_run_benchmark_independent_samples(self):
        self.driver.args.independent_samples = 3
        r = self.driver.run_independent_samples('b1')
        self.assertEquals(self.subprocess_mock.calls.count(
            ('/benchmarks/Benchmark_O', 'b1', '--memory')), 3)
        self.assertEquals(r.num_samples, 3)  # results are merged

    def test_run_and_log(self):
        def mock_run(test):
            self.assertEquals(test, 'b1')
            return PerformanceTestResult(
                '3,b1,1,123,123,123,0,123,888'.split(','))
        driver = BenchmarkDriver(tests=['b1'], args=Stub(output_dir=None))
        driver.run_independent_samples = mock_run  # patching

        with captured_output() as (out, _):
            log = driver.run_and_log()

        csv_log = '3,b1,1,123,123,123,0,123,888\n'
        self.assertEquals(log, None)
        self.assertEquals(
            out.getvalue(),
            '#,TEST,SAMPLES,MIN(μs),MAX(μs),MEAN(μs),SD(μs),MEDIAN(μs),' +
            'MAX_RSS(B)\n' +
            csv_log +
            '\n' +
            'Total performance tests executed: 1\n')

        with captured_output() as (out, _):
            log = driver.run_and_log(csv_console=False)

        self.assertEquals(log, csv_log)
        self.assertEquals(
            out.getvalue(),
            '  # TEST                      SAMPLES MIN(μs) MAX(μs)' +
            ' MEAN(μs) SD(μs) MEDIAN(μs) MAX_RSS(B)\n' +
            '  3 b1                              1     123     123' +
            '      123      0        123        888\n' +
            '\n' +
            'Total performance tests executed: 1\n')

    def test_log_results(self):
        """Create log directory if it doesn't exist and write the log file."""
        def assert_log_written(out, log_file, content):
            self.assertEquals(out.getvalue(),
                              'Logging results to: ' + log_file + '\n')
            with open(log_file, 'rU') as f:
                text = f.read()
            self.assertEquals(text, "formatted output")

        try:
            import tempfile  # setUp
            temp_dir = tempfile.mkdtemp()
            log_dir = os.path.join(temp_dir, 'sub-dir/')
            driver = BenchmarkDriver(Stub(), tests=[''])

            self.assertFalse(os.path.exists(log_dir))
            content = "formatted output"
            log_file = os.path.join(log_dir, '1.log')
            with captured_output() as (out, _):
                driver.log_results(content, log_file=log_file)
            assert_log_written(out, log_file, content)

            self.assertTrue(os.path.exists(log_dir))
            log_file = os.path.join(log_dir, '2.log')
            with captured_output() as (out, _):
                driver.log_results(content, log_file=log_file)
            assert_log_written(out, log_file, content)

        finally:
            import shutil  # tearDown
            shutil.rmtree(temp_dir)

    def test_deterministing_hashing(self):
        cmd = ['printenv', 'SWIFT_DETERMINISTIC_HASHING']
        driver = BenchmarkDriver(['no args'], tests=['ignored'])
        self.assertEquals(driver._invoke(cmd).strip(), '1')


class BenchmarkDriverMock(Mock):
    """Mock for BenchmarkDriver's `run` method"""
    def __init__(self, tests, responses=None):
        super(BenchmarkDriverMock, self).__init__(responses)
        self.tests = tests
        self.args = ArgsStub()

        def _run(test, num_samples=None, num_iters=None,
                 verbose=None, measure_memory=False):
            return self.record_and_respond(test, num_samples, num_iters,
                                           verbose, measure_memory)
        self.run = _run

    def record_and_respond(self, test, num_samples, num_iters,
                           verbose, measure_memory):
        args = (test, num_samples, num_iters, verbose, measure_memory)
        self.calls.append(args)
        return self.respond.get(args, _PTR(min=700))


class TestLoggingReportFormatter(unittest.TestCase):
    def test_plain_log_format(self):
        lr = logging.makeLogRecord({
            'name': 'Base.category', 'level': logging.DEBUG,
            'levelname': 'DEBUG', 'msg': 'Hi!'})
        f = LoggingReportFormatter()
        self.assertEquals(f.format(lr), 'DEBUG category: Hi!')

    def test_colored_log_format(self):
        def record(level, level_name):
            return logging.makeLogRecord({
                'name': 'Base.category', 'levelno': level,
                'levelname': level_name, 'msg': 'Hi!'})
        f = LoggingReportFormatter(use_color=True)
        self.assertEquals(f.format(record(logging.DEBUG, 'DEBUG')),
                          '\x1b[1;39mcategory: Hi!\x1b[1;0m')
        self.assertEquals(f.format(record(logging.INFO, 'INFO')),
                          '\x1b[1;32mcategory: Hi!\x1b[1;0m')
        self.assertEquals(f.format(record(logging.WARNING, 'WARNING')),
                          '\x1b[1;33mcategory: Hi!\x1b[1;0m')
        self.assertEquals(f.format(record(logging.ERROR, 'ERROR')),
                          '\x1b[1;31mcategory: Hi!\x1b[1;0m')
        self.assertEquals(f.format(record(logging.CRITICAL, 'CRITICAL')),
                          '\x1b[1;35mcategory: Hi!\x1b[1;0m')

    def test_no_prefix_for_base_logging(self):
        lr = logging.makeLogRecord({
            'name': 'Base', 'level': logging.INFO,
            'levelname': 'INFO', 'msg': 'Hi!'})
        f = LoggingReportFormatter()
        self.assertEquals(f.format(lr), 'INFO Hi!')


def _PTR(min=700, mem_pages=1000):
    """Create PerformanceTestResult Stub."""
    return Stub(min=min, mem_pages=mem_pages)


def _run(test, num_samples=None, num_iters=None, verbose=None,
         measure_memory=False):
    """Helper function that constructs tuple with arguments for run method."""
    return (
        test, num_samples, num_iters, verbose, measure_memory)


class TestBenchmarkDoctor(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        super(TestBenchmarkDoctor, cls).setUpClass()
        doctor_log = logging.getLogger('BenchmarkDoctor')
        cls._doctor_log_handler = MockLoggingHandler(level='DEBUG')
        doctor_log.addHandler(cls._doctor_log_handler)

    def setUp(self):
        super(TestBenchmarkDoctor, self).setUp()
        self.args = Stub(verbose=False)
        self._doctor_log_handler.reset()
        self.logs = self._doctor_log_handler.messages

    def assert_contains(self, texts, output):
        assert not isinstance(texts, str)
        for text in texts:
            self.assertIn(text, output)

    def test_uses_logging(self):
        driver = BenchmarkDriverMock(tests=['B1', 'B2'])
        with captured_output() as (out, _):
            BenchmarkDoctor(self.args, driver)
        self.assert_contains(['Checking tests: B1, B2'], self.logs['debug'])
        self.assertEquals(out.getvalue(), '')

    def test_supports_verbose_output(self):
        driver = BenchmarkDriverMock(tests=['B1', 'B2'])
        driver.verbose = True
        with captured_output() as (out, _):
            BenchmarkDoctor(Stub(verbose=True), driver)
        self.assert_contains(['Checking tests: B1, B2'], out.getvalue())

    def test_uses_report_formatter(self):
        doctor = BenchmarkDoctor(self.args, BenchmarkDriverMock(tests=['B1']))
        console_handler = logging.getLogger('BenchmarkDoctor').handlers[1]
        self.assertTrue(doctor)
        self.assertTrue(isinstance(console_handler, logging.StreamHandler))
        self.assertTrue(isinstance(console_handler.formatter,
                                   LoggingReportFormatter))

    def test_measure_10_independent_1s_benchmark_series(self):
        """Measurement strategy takes 5 i2 and 5 i1 series.

        Num-samples for Benchmark Driver are calibrated to be powers of two,
        take measurements for approximately 1s
        based on short initial runtime sampling. Capped at 2k samples.
        """
        driver = BenchmarkDriverMock(tests=['B1'], responses=([
            # calibration run, returns a stand-in for PerformanceTestResult
            (_run('B1', num_samples=3, num_iters=1), _PTR(min=300))] +
            # 5x i1 series, with 300 μs runtime its possible to take 4098
            # samples/s, but it should be capped at 2k
            ([(_run('B1', num_samples=2048, num_iters=1,
                    verbose=True, measure_memory=True), _PTR(min=300))] * 5) +
            # 5x i2 series
            ([(_run('B1', num_samples=2048, num_iters=2,
                    verbose=True, measure_memory=True), _PTR(min=300))] * 5)
        ))
        doctor = BenchmarkDoctor(self.args, driver)
        with captured_output() as (out, _):
            measurements = doctor.measure('B1')

        driver.assert_called_all_expected()
        self.assert_contains(
            ['name',
             'B1 O i1a', 'B1 O i1b', 'B1 O i1c', 'B1 O i1d', 'B1 O i1e',
             'B1 O i2a', 'B1 O i2b', 'B1 O i2c', 'B1 O i2d', 'B1 O i2e'],
            measurements.keys())
        self.assertEquals(measurements['name'], 'B1')
        self.assert_contains(
            ['Calibrating num-samples for B1:',
             'Runtime 300 μs yields 4096 adjusted samples per second.',
             'Measuring B1, 5 x i1 (2048 samples), 5 x i2 (2048 samples)'],
            self.logs['debug'])

    def test_benchmark_name_matches_capital_words_conventions(self):
        driver = BenchmarkDriverMock(tests=[
            'BenchmarkName', 'CapitalWordsConvention', 'ABBRName',
            'wrongCase', 'Wrong_convention'])
        with captured_output() as (out, _):
            doctor = BenchmarkDoctor(self.args, driver)
            doctor.check()
        output = out.getvalue()

        self.assertIn('naming: ', output)
        self.assertNotIn('BenchmarkName', output)
        self.assertNotIn('CapitalWordsConvention', output)
        self.assertNotIn('ABBRName', output)
        self.assert_contains(
            ["'wrongCase' name doesn't conform to UpperCamelCase convention.",
             "'Wrong_convention' name doesn't conform to UpperCamelCase "
             "convention."], self.logs['error'])
        self.assert_contains(
            ['See http://bit.ly/UpperCamelCase'], self.logs['info'])

    def test_benchmark_name_is_at_most_40_chars_long(self):
        driver = BenchmarkDriverMock(tests=[
            'BenchmarkName',
            'ThisTestNameIsTooLongAndCausesOverflowsInReports'])
        with captured_output() as (out, _):
            doctor = BenchmarkDoctor(self.args, driver)
            doctor.check()
        output = out.getvalue()

        self.assertIn('naming: ', output)
        self.assertNotIn('BenchmarkName', output)
        self.assert_contains(
            ["'ThisTestNameIsTooLongAndCausesOverflowsInReports' name is "
             "48 characters long."], self.logs['error'])
        self.assert_contains(
            ["Benchmark name should not be longer than 40 characters."],
            self.logs['info'])

    def test_benchmark_runtime_range(self):
        """Optimized benchmark should run in less then 2500 μs.

        With runtimes less than 2500 μs there is better than 1:4 chance of
        being interrupted in the middle of measurement due to elapsed 10 ms
        quantum used by macos scheduler.

        Warn about longer runtime. Runtimes over half a second are an error.
        """
        def measurements(name, runtime):
            return {'name': name,
                    name + ' O i1a': _PTR(min=runtime + 2),
                    name + ' O i2a': _PTR(min=runtime)}

        with captured_output() as (out, _):
            doctor = BenchmarkDoctor(self.args, BenchmarkDriverMock([]))
            doctor.analyze(measurements('Cheetah', 200))
            doctor.analyze(measurements('Hare', 2501))
            doctor.analyze(measurements('Tortoise', 500000))
            doctor.analyze({'name': 'OverheadTurtle',
                            'OverheadTurtle O i1a': _PTR(min=800000),
                            'OverheadTurtle O i2a': _PTR(min=700000)})
        output = out.getvalue()

        self.assertIn('runtime: ', output)
        self.assertNotIn('Cheetah', output)
        self.assert_contains(["'Hare' execution took at least 2501 μs."],
                             self.logs['warning'])
        self.assert_contains(
            ["Decrease the workload of 'Hare' by a factor of 2, "
             "to be less than 2500 μs."], self.logs['info'])
        self.assert_contains(
            ["'Tortoise' execution took at least 500000 μs."],
            self.logs['error'])
        self.assert_contains(
            ["Decrease the workload of 'Tortoise' by a factor of 256, "
             "to be less than 2500 μs."], self.logs['info'])
        self.assert_contains(
            ["'OverheadTurtle' execution took at least 600000 μs"
             " (excluding the setup overhead)."],
            self.logs['error'])

    def test_benchmark_has_no_significant_setup_overhead(self):
        with captured_output() as (out, _):
            doctor = BenchmarkDoctor(self.args, BenchmarkDriverMock([]))
            doctor.analyze({
                'name': 'NoOverhead',  # not 'significant' enough
                # Based on DropFirstArray a10/e10: overhead 3.7% (6 μs)
                'NoOverhead O i1a': _PTR(min=162),
                'NoOverhead O i2a': _PTR(min=159)})
            doctor.analyze({
                'name': 'SO',  # Setup Overhead
                # Based on SuffixArrayLazy a10/e10: overhead 5.8% (4 μs)
                'SO O i1a': _PTR(min=69), 'SO O i1b': _PTR(min=70),
                'SO O i2a': _PTR(min=67), 'SO O i2b': _PTR(min=68)})
            doctor.analyze({'name': 'Zero', 'Zero O i1a': _PTR(min=0),
                            'Zero O i2a': _PTR(min=0)})
        output = out.getvalue()

        self.assertIn('runtime: ', output)
        self.assertNotIn('NoOverhead', output)
        self.assertNotIn('ZeroRuntime', output)
        self.assert_contains(
            ["'SO' has setup overhead of 4 μs (5.8%)."],
            self.logs['error'])
        self.assert_contains(
            ["Move initialization of benchmark data to the `setUpFunction` "
             "registered in `BenchmarkInfo`."], self.logs['info'])

    def test_benchmark_has_constant_memory_use(self):
        """Benchmark's memory footprint must not vary with num-iters."""
        with captured_output() as (out, _):
            doctor = BenchmarkDoctor(self.args, BenchmarkDriverMock([]))
            doctor.analyze({
                # The threshold of 15 pages was estimated from previous
                # measurements. The normal range should be probably aproximated
                # by a function instead of a simple constant.
                # TODO: re-evaluate normal range from whole SBS
                'name': 'ConstantMemory',
                'ConstantMemory O i1a': _PTR(mem_pages=1460),
                'ConstantMemory O i2a': _PTR(mem_pages=(1460 + 15))})
            doctor.analyze({
                'name': 'VariableMemory',  # ObserverForwardStruct
                'VariableMemory O i1a': _PTR(mem_pages=1460),
                'VariableMemory O i1b': _PTR(mem_pages=1472),
                # i2 series start at 290 pages higher
                'VariableMemory O i2a': _PTR(mem_pages=1750),
                'VariableMemory O i2b': _PTR(mem_pages=1752)})
            measurements = dict([
                ('HighVariance O i{0}{1}'.format(num_iters, suffix),
                 _PTR(mem_pages=num_pages))
                for num_iters, pages in [
                    (1, [6200, 5943, 4818, 5612, 5469]),
                    (2, [6244, 5832, 4674, 5176, 5490])]
                for num_pages, suffix in zip(pages, list('abcde'))])
            measurements['name'] = 'HighVariance'  # Array2D
            doctor.analyze(measurements)
        output = out.getvalue()

        self.assertIn('memory: ', output)
        self.assertNotIn('ConstantMemory', output)
        self.assert_contains(
            ["'VariableMemory' varies the memory footprint of the base "
             "workload depending on the `num-iters`."],
            self.logs['error'])
        self.assert_contains(
            ["'HighVariance' has very wide range of memory used between "
             "independent, repeated measurements."],
            self.logs['warning'])


if __name__ == '__main__':
    unittest.main()
