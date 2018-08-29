#!/usr/bin/python
# -*- coding: utf-8 -*-

# ===--- compare_perf_tests.py -------------------------------------------===//
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
"""
This script compares performance test logs and issues a formatted report.

Invoke `$ compare_perf_tests.py -h ` for complete list of options.

class `Sample` is single benchmark measurement.
class `PerformanceTestSamples` is collection of `Sample`s and their statistics.
class `PerformanceTestResult` is a summary of performance test execution.
class `LogParser` converts log files into `PerformanceTestResult`s.
class `ResultComparison` compares new and old `PerformanceTestResult`s.
class `TestComparator` analyzes changes betweeen the old and new test results.
class `ReportFormatter` creates the test comparison report in specified format.

"""

from __future__ import print_function

import argparse
import re
import sys
from bisect import bisect, bisect_left, bisect_right
from collections import namedtuple
from math import sqrt


class Sample(namedtuple('Sample', 'i num_iters runtime')):
    u"""Single benchmark measurement.

    Initialized with:
    `i`: ordinal number of the sample taken,
    `num-num_iters`:  number or iterations used to compute it,
    `runtime`: in microseconds (μs).
    """

    def __repr__(self):
        """Shorter Sample formating for debugging purposes."""
        return 's({0.i!r}, {0.num_iters!r}, {0.runtime!r})'.format(self)


class PerformanceTestSamples(object):
    """Collection of runtime samples from the benchmark execution.

    Computes the sample population statistics.
    """

    def __init__(self, name, samples=None):
        """Initialize with benchmark name and optional list of Samples."""
        self.name = name  # Name of the performance test
        self.samples = []
        self.outliers = []
        self._runtimes = []
        self.mean = 0.0
        self.S_runtime = 0.0  # For computing running variance
        for sample in samples or []:
            self.add(sample)

    def __str__(self):
        """Text summary of benchmark statisctics."""
        return (
            '{0.name!s} n={0.count!r} '
            'Min={0.min!r} Q1={0.q1!r} M={0.median!r} Q3={0.q3!r} '
            'Max={0.max!r} '
            'R={0.range!r} {0.spread:.2%} IQR={0.iqr!r} '
            'Mean={0.mean:.0f} SD={0.sd:.0f} CV={0.cv:.2%}'
            .format(self) if self.samples else
            '{0.name!s} n=0'.format(self))

    def add(self, sample):
        """Add sample to collection and recompute statistics."""
        assert isinstance(sample, Sample)
        self._update_stats(sample)
        i = bisect(self._runtimes, sample.runtime)
        self._runtimes.insert(i, sample.runtime)
        self.samples.insert(i, sample)

    def _update_stats(self, sample):
        old_stats = (self.count, self.mean, self.S_runtime)
        _, self.mean, self.S_runtime = (
            self.running_mean_variance(old_stats, sample.runtime))

    def exclude_outliers(self, top_only=False):
        """Exclude outliers by applying Interquartile Range Rule.

        Moves the samples outside of the inner fences
        (Q1 - 1.5*IQR and Q3 + 1.5*IQR) into outliers list and recomputes
        statistics for the remaining sample population. Optionally apply
        only the top inner fence, preserving the small outliers.

        Experimentally, this rule seems to perform well-enough on the
        benchmark runtimes in the microbenchmark range to filter out
        the environment noise caused by preemtive multitasking.
        """
        lo = (0 if top_only else
              bisect_left(self._runtimes, int(self.q1 - 1.5 * self.iqr)))
        hi = bisect_right(self._runtimes, int(self.q3 + 1.5 * self.iqr))

        outliers = self.samples[:lo] + self.samples[hi:]
        samples = self.samples[lo:hi]

        self.__init__(self.name)  # re-initialize
        for sample in samples:  # and
            self.add(sample)  # re-compute stats
        self.outliers = outliers

    @property
    def count(self):
        """Number of samples used to compute the statistics."""
        return len(self.samples)

    @property
    def num_samples(self):
        """Number of all samples in the collection."""
        return len(self.samples) + len(self.outliers)

    @property
    def all_samples(self):
        """List of all samples in ascending order."""
        return sorted(self.samples + self.outliers, key=lambda s: s.i)

    @property
    def min(self):
        """Minimum sampled value."""
        return self.samples[0].runtime

    @property
    def max(self):
        """Maximum sampled value."""
        return self.samples[-1].runtime

    @property
    def median(self):
        """Median sampled value."""
        return self.samples[self.count / 2].runtime

    @property
    def q1(self):
        """First Quartile (25th Percentile)."""
        return self.samples[self.count / 4].runtime

    @property
    def q3(self):
        """Third Quartile (75th Percentile)."""
        return self.samples[(self.count / 2) + (self.count / 4)].runtime

    @property
    def iqr(self):
        """Interquartile Range."""
        return self.q3 - self.q1

    @property
    def sd(self):
        u"""Standard Deviation (μs)."""
        return (0 if self.count < 2 else
                sqrt(self.S_runtime / (self.count - 1)))

    @staticmethod
    def running_mean_variance((k, M_, S_), x):
        """Compute running variance, B. P. Welford's method.

        See Knuth TAOCP vol 2, 3rd edition, page 232, or
        https://www.johndcook.com/blog/standard_deviation/
        M is mean, Standard Deviation is defined as sqrt(S/k-1)
        """
        k = float(k + 1)
        M = M_ + (x - M_) / k
        S = S_ + (x - M_) * (x - M)
        return (k, M, S)

    @property
    def cv(self):
        """Coeficient of Variation (%)."""
        return (self.sd / self.mean) if self.mean else 0

    @property
    def range(self):
        """Range of samples values (Max - Min)."""
        return self.max - self.min

    @property
    def spread(self):
        """Sample Spread; i.e. Range as (%) of Min."""
        return self.range / float(self.min) if self.min else 0


class PerformanceTestResult(object):
    u"""Result from executing an individual Swift Benchmark Suite benchmark.

    Reported by the test driver (Benchmark_O, Benchmark_Onone, Benchmark_Osize
    or Benchmark_Driver).

    It depends on the log format emitted by the test driver in the form:
    #,TEST,SAMPLES,MIN(μs),MAX(μs),MEAN(μs),SD(μs),MEDIAN(μs),MAX_RSS(B)

    The last column, MAX_RSS, is emitted only for runs instrumented by the
    Benchmark_Driver to measure rough memory use during the execution of the
    benchmark.
    """

    def __init__(self, csv_row):
        """Initialize from a row with 8 or 9 columns with benchmark summary.

        The row is an iterable, such as a row provided by the CSV parser.
        """
        self.test_num = csv_row[0]      # Ordinal number of the test
        self.name = csv_row[1]          # Name of the performance test
        self.num_samples = (            # Number of measurement samples taken
            int(csv_row[2]))
        self.min = int(csv_row[3])      # Minimum runtime (μs)
        self.max = int(csv_row[4])      # Maximum runtime (μs)
        self.mean = float(csv_row[5])   # Mean (average) runtime (μs)
        self.sd = float(csv_row[6])     # Standard Deviation (μs)
        self.median = int(csv_row[7])   # Median runtime (μs)
        self.max_rss = (                # Maximum Resident Set Size (B)
            int(csv_row[8]) if len(csv_row) > 8 else None)
        self.samples = None

    def __repr__(self):
        """Short summary for debugging purposes."""
        return (
            '<PerformanceTestResult name:{0.name!r} '
            'samples:{0.num_samples!r} min:{0.min!r} max:{0.max!r} '
            'mean:{0.mean:.0f} sd:{0.sd:.0f} median:{0.median!r}>'
            .format(self))

    def merge(self, r):
        """Merge two results.

        Recomputes min, max and mean statistics. If all `samples` are
        avaliable, it recomputes all the statistics.
        The use case here is comparing test results parsed from concatenated
        log files from multiple runs of benchmark driver.
        """
        if self.samples and r.samples:
            map(self.samples.add, r.samples.samples)
            sams = self.samples
            self.num_samples = sams.num_samples
            self.min, self.max, self.median, self.mean, self.sd = \
                sams.min, sams.max, sams.median, sams.mean, sams.sd
        else:
            self.min = min(self.min, r.min)
            self.max = max(self.max, r.max)
            self.mean = (  # pooled mean is the weighted sum of means
                (self.mean * self.num_samples) + (r.mean * r.num_samples)
            ) / float(self.num_samples + r.num_samples)
            self.num_samples += r.num_samples
            self.max_rss = min(self.max_rss, r.max_rss)
            self.median, self.sd = 0, 0


class ResultComparison(object):
    """ResultComparison compares MINs from new and old PerformanceTestResult.

    It computes speedup ratio and improvement delta (%).
    """

    def __init__(self, old, new):
        """Initialize with old and new `PerformanceTestResult`s to compare."""
        self.old = old
        self.new = new
        assert old.name == new.name
        self.name = old.name  # Test name, convenience accessor

        # Speedup ratio
        self.ratio = (old.min + 0.001) / (new.min + 0.001)

        # Test runtime improvement in %
        ratio = (new.min + 0.001) / (old.min + 0.001)
        self.delta = ((ratio - 1) * 100)

        # Indication of dubious changes: when result's MIN falls inside the
        # (MIN, MAX) interval of result they are being compared with.
        self.is_dubious = ((old.min < new.min and new.min < old.max) or
                           (new.min < old.min and old.min < new.max))


class LogParser(object):
    """Converts log outputs into `PerformanceTestResult`s.

    Supports various formats produced by the `Benchmark_Driver` and
    `Benchmark_O`('Onone', 'Osize'). It can also merge together the
    results from concatenated log files.
    """

    def __init__(self):
        """Create instance of `LogParser`."""
        self.results = []
        self._reset()

    def _reset(self):
        """Reset parser to the default state for reading a new result."""
        self.samples, self.num_iters = [], 1
        self.max_rss, self.mem_pages = None, None
        self.voluntary_cs, self.involuntary_cs = None, None

    # Parse lines like this
    # #,TEST,SAMPLES,MIN(μs),MAX(μs),MEAN(μs),SD(μs),MEDIAN(μs)
    results_re = re.compile(r'( *\d+[, \t]*[\w.]+[, \t]*' +
                            r'[, \t]*'.join([r'[\d.]+'] * 6) +
                            r'[, \t]*[\d.]*)')  # optional MAX_RSS(B)

    def _append_result(self, result):
        columns = result.split(',')
        if len(columns) < 8:
            columns = result.split()
        r = PerformanceTestResult(columns)
        if self.max_rss:
            r.max_rss = self.max_rss
            r.mem_pages = self.mem_pages
            r.voluntary_cs = self.voluntary_cs
            r.involuntary_cs = self.involuntary_cs
        if self.samples:
            r.samples = PerformanceTestSamples(r.name, self.samples)
            r.samples.exclude_outliers()
        self.results.append(r)
        self._reset()

    def _store_memory_stats(self, max_rss, mem_pages):
        self.max_rss = int(max_rss)
        self.mem_pages = int(mem_pages)

    # Regular expression and action to take when it matches the parsed line
    state_actions = {
        results_re: _append_result,

        # Verbose mode adds new productions:
        # Adaptively determined N; test loop multiple adjusting runtime to ~1s
        re.compile(r'\s+Measuring with scale (\d+).'):
        (lambda self, num_iters: setattr(self, 'num_iters', num_iters)),

        re.compile(r'\s+Sample (\d+),(\d+)'):
        (lambda self, i, runtime:
         self.samples.append(
             Sample(int(i), int(self.num_iters), int(runtime)))),

        # Environmental statistics: memory usage and context switches
        re.compile(r'\s+MAX_RSS \d+ - \d+ = (\d+) \((\d+) pages\)'):
        _store_memory_stats,

        re.compile(r'\s+VCS \d+ - \d+ = (\d+)'):
        (lambda self, vcs: setattr(self, 'voluntary_cs', int(vcs))),

        re.compile(r'\s+ICS \d+ - \d+ = (\d+)'):
        (lambda self, ics: setattr(self, 'involuntary_cs', int(ics))),
    }

    def parse_results(self, lines):
        """Parse results from the lines of the log output from Benchmark*.

        Returns a list of `PerformanceTestResult`s.
        """
        for line in lines:
            for regexp, action in LogParser.state_actions.items():
                match = regexp.match(line)
                if match:
                    action(self, *match.groups())
                    break  # stop after 1st match
            else:  # If none matches, skip the line.
                # print('skipping: ' + line.rstrip('\n'))
                continue
        return self.results

    @staticmethod
    def _results_from_lines(lines):
        tests = LogParser().parse_results(lines)

        def add_or_merge(names, r):
            if r.name not in names:
                names[r.name] = r
            else:
                names[r.name].merge(r)
            return names

        return reduce(add_or_merge, tests, dict())

    @staticmethod
    def results_from_string(log_contents):
        """Parse `PerformanceTestResult`s from the supplied string.

        Returns dictionary of test names and `PerformanceTestResult`s.
        """
        return LogParser._results_from_lines(log_contents.splitlines())

    @staticmethod
    def results_from_file(log_file):
        """Parse `PerformanceTestResult`s from the log file.

        Returns dictionary of test names and `PerformanceTestResult`s.
        """
        with open(log_file) as f:
            return LogParser._results_from_lines(f.readlines())


class TestComparator(object):
    """Analyzes changes betweeen the old and new test results.

    It determines which tests were `added`, `removed` and which can be
    compared. It then splits the `ResultComparison`s into 3 groups according to
    the `delta_threshold` by the change in performance: `increased`,
    `descreased` and `unchanged`. Whole computaion is performed during
    initialization and results are provided as properties on this object.

    The lists of `added`, `removed` and `unchanged` tests are sorted
    alphabetically. The `increased` and `decreased` lists are sorted in
    descending order by the amount of change.
    """

    def __init__(self, old_results, new_results, delta_threshold):
        """Initialize with dictionaries of old and new benchmark results.

        Dictionary keys are benchmark names, values are
        `PerformanceTestResult`s.
        """
        old_tests = set(old_results.keys())
        new_tests = set(new_results.keys())
        comparable_tests = new_tests.intersection(old_tests)
        added_tests = new_tests.difference(old_tests)
        removed_tests = old_tests.difference(new_tests)

        self.added = sorted([new_results[t] for t in added_tests],
                            key=lambda r: r.name)
        self.removed = sorted([old_results[t] for t in removed_tests],
                              key=lambda r: r.name)

        def compare(name):
            return ResultComparison(old_results[name], new_results[name])

        comparisons = map(compare, comparable_tests)

        def partition(l, p):
            return reduce(lambda x, y: x[not p(y)].append(y) or x, l, ([], []))

        decreased, not_decreased = partition(
            comparisons, lambda c: c.ratio < (1 - delta_threshold))
        increased, unchanged = partition(
            not_decreased, lambda c: c.ratio > (1 + delta_threshold))

        # sorted partitions
        names = [c.name for c in comparisons]
        comparisons = dict(zip(names, comparisons))
        self.decreased = [comparisons[c.name]
                          for c in sorted(decreased, key=lambda c: -c.delta)]
        self.increased = [comparisons[c.name]
                          for c in sorted(increased, key=lambda c: c.delta)]
        self.unchanged = [comparisons[c.name]
                          for c in sorted(unchanged, key=lambda c: c.name)]


class ReportFormatter(object):
    """Creates the report from perfromance test comparison in specified format.

    `ReportFormatter` formats the `PerformanceTestResult`s and
    `ResultComparison`s provided by `TestComparator` into report table.
    Supported formats are: `markdown` (used for displaying benchmark results on
    GitHub), `git` and `html`.
    """

    def __init__(self, comparator, old_branch, new_branch, changes_only,
                 single_table=False):
        """Initialize with `TestComparator` and names of branches."""
        self.comparator = comparator
        self.old_branch = old_branch
        self.new_branch = new_branch
        self.changes_only = changes_only
        self.single_table = single_table

    MARKDOWN_DETAIL = """
<details {3}>
  <summary>{0} ({1})</summary>
  {2}
</details>
"""
    GIT_DETAIL = """
{0} ({1}): {2}"""

    PERFORMANCE_TEST_RESULT_HEADER = ('TEST', 'MIN', 'MAX', 'MEAN', 'MAX_RSS')
    RESULT_COMPARISON_HEADER = ('TEST', 'OLD', 'NEW', 'DELTA', 'SPEEDUP')

    @staticmethod
    def header_for(result):
        """Column labels for header row in results table."""
        return (ReportFormatter.PERFORMANCE_TEST_RESULT_HEADER
                if isinstance(result, PerformanceTestResult) else
                # isinstance(result, ResultComparison)
                ReportFormatter.RESULT_COMPARISON_HEADER)

    @staticmethod
    def values(result):
        """Format values from PerformanceTestResult or ResultComparison.

        Returns tuple of strings to display in the results table.
        """
        return (
            (result.name,
             str(result.min), str(result.max), str(int(result.mean)),
             str(result.max_rss) if result.max_rss else '—')
            if isinstance(result, PerformanceTestResult) else
            # isinstance(result, ResultComparison)
            (result.name,
             str(result.old.min), str(result.new.min),
             '{0:+.1f}%'.format(result.delta),
             '{0:.2f}x{1}'.format(result.ratio,
                                  ' (?)' if result.is_dubious else ''))
        )

    def markdown(self):
        """Report results of benchmark comparisons in Markdown format."""
        return self._formatted_text(
            ROW='{0} | {1} | {2} | {3} | {4} \n',
            HEADER_SEPARATOR='---',
            DETAIL=self.MARKDOWN_DETAIL)

    def git(self):
        """Report results of benchmark comparisons in 'git' format."""
        return self._formatted_text(
            ROW='{0}   {1}   {2}   {3}   {4} \n',
            HEADER_SEPARATOR='   ',
            DETAIL=self.GIT_DETAIL)

    def _column_widths(self):
        changed = self.comparator.decreased + self.comparator.increased
        results = (changed if self.changes_only else
                   changed + self.comparator.unchanged)
        results += self.comparator.added + self.comparator.removed

        widths = [
            map(len, columns) for columns in
            [ReportFormatter.PERFORMANCE_TEST_RESULT_HEADER,
             ReportFormatter.RESULT_COMPARISON_HEADER] +
            [ReportFormatter.values(r) for r in results]
        ]

        def max_widths(maximum, widths):
            return tuple(map(max, zip(maximum, widths)))

        return reduce(max_widths, widths, tuple([0] * 5))

    def _formatted_text(self, ROW, HEADER_SEPARATOR, DETAIL):
        widths = self._column_widths()
        self.header_printed = False

        def justify_columns(contents):
            return tuple([c.ljust(w) for w, c in zip(widths, contents)])

        def row(contents):
            return ROW.format(*justify_columns(contents))

        def header(header):
            return '\n' + row(header) + row(tuple([HEADER_SEPARATOR] * 5))

        def format_columns(r, strong):
            return (r if not strong else
                    r[:-1] + ('**{0}**'.format(r[-1]), ))

        def table(title, results, is_strong=False, is_open=False):
            rows = [
                row(format_columns(ReportFormatter.values(r), is_strong))
                for r in results
            ]
            if not rows:
                return ''

            if self.single_table:
                t = ''
                if not self.header_printed:
                    t += header(ReportFormatter.header_for(results[0]))
                    self.header_printed = True
                t += row(('**' + title + '**', '', '', '', ''))
                t += ''.join(rows)
                return t

            return DETAIL.format(
                *[
                    title, len(results),
                    (header(ReportFormatter.header_for(results[0])) +
                     ''.join(rows)),
                    ('open' if is_open else '')
                ])

        return ''.join([
            # FIXME print self.old_branch, self.new_branch
            table('Regression', self.comparator.decreased, True, True),
            table('Improvement', self.comparator.increased, True),
            ('' if self.changes_only else
             table('No Changes', self.comparator.unchanged)),
            table('Added', self.comparator.added, is_open=True),
            table('Removed', self.comparator.removed, is_open=True)
        ])

    HTML = """
<!DOCTYPE html>
<html>
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <style>
        body {{ font-family: -apple-system, sans-serif; font-size: 14px; }}
        table {{ border-spacing: 2px; border-color: gray; border-spacing: 0;
                border-collapse: collapse; }}
        table tr {{ background-color: #fff; border-top: 1px solid #c6cbd1; }}
        table th, table td {{ padding: 6px 13px; border: 1px solid #dfe2e5; }}
        th {{ text-align: center; padding-top: 130px; }}
        td {{ text-align: right; }}
        table td:first-child {{ text-align: left; }}
        tr:nth-child(even) {{ background-color: #000000; }}
        tr:nth-child(2n) {{ background-color: #f6f8fa; }}
    </style>
</head>
<body>
<table>
{0}
</table>
</body>
</html>"""

    HTML_HEADER_ROW = """
        <tr>
                <th align='left'>{0} ({1})</th>
                <th align='left'>{2}</th>
                <th align='left'>{3}</th>
                <th align='left'>{4}</th>
                <th align='left'>{5}</th>
        </tr>
"""

    HTML_ROW = """
        <tr>
                <td align='left'>{0}</td>
                <td align='left'>{1}</td>
                <td align='left'>{2}</td>
                <td align='left'>{3}</td>
                <td align='left'><font color='{4}'>{5}</font></td>
        </tr>
"""

    def html(self):
        """Report results of benchmark comparisons in HTML format."""
        def row(name, old, new, delta, speedup, speedup_color):
            return self.HTML_ROW.format(
                name, old, new, delta, speedup_color, speedup)

        def header(contents):
            return self.HTML_HEADER_ROW.format(* contents)

        def table(title, results, speedup_color):
            rows = [
                row(*(ReportFormatter.values(r) + (speedup_color,)))
                for r in results
            ]
            return ('' if not rows else
                    header((title, len(results)) +
                           ReportFormatter.header_for(results[0])[1:]) +
                    ''.join(rows))

        return self.HTML.format(
            ''.join([
                # FIXME print self.old_branch, self.new_branch
                table('Regression', self.comparator.decreased, 'red'),
                table('Improvement', self.comparator.increased, 'green'),
                ('' if self.changes_only else
                 table('No Changes', self.comparator.unchanged, 'black')),
                table('Added', self.comparator.added, ''),
                table('Removed', self.comparator.removed, '')
            ]))


def parse_args(args):
    """Parse command line arguments and set default values."""
    parser = argparse.ArgumentParser(description='Compare Performance tests.')
    parser.add_argument('--old-file',
                        help='Baseline performance test suite (csv file)',
                        required=True)
    parser.add_argument('--new-file',
                        help='New performance test suite (csv file)',
                        required=True)
    parser.add_argument('--format',
                        choices=['markdown', 'git', 'html'],
                        help='Output format. Default is markdown.',
                        default="markdown")
    parser.add_argument('--output', help='Output file name')
    parser.add_argument('--changes-only',
                        help='Output only affected tests', action='store_true')
    parser.add_argument(
        '--single-table',
        help='Combine data in a single table in git and markdown formats',
        action='store_true')
    parser.add_argument('--new-branch',
                        help='Name of the new branch', default='NEW_MIN')
    parser.add_argument('--old-branch',
                        help='Name of the old branch', default='OLD_MIN')
    parser.add_argument('--delta-threshold',
                        help='Delta threshold. Default 0.05.',
                        type=float, default=0.05)
    return parser.parse_args(args)


def main():
    """Compare benchmarks for changes in a formatted report."""
    args = parse_args(sys.argv[1:])
    comparator = TestComparator(LogParser.results_from_file(args.old_file),
                                LogParser.results_from_file(args.new_file),
                                args.delta_threshold)
    formatter = ReportFormatter(comparator, args.old_branch, args.new_branch,
                                args.changes_only, args.single_table)
    formats = {
        'markdown': formatter.markdown,
        'git': formatter.git,
        'html': formatter.html
    }

    report = formats[args.format]()
    print(report)

    if args.output:
        with open(args.output, 'w') as f:
            f.write(report)


if __name__ == '__main__':
    sys.exit(main())
