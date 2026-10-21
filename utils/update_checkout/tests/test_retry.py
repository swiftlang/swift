# ===--- test_retry.py ----------------------------------------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2026 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ===----------------------------------------------------------------------===#

import contextlib
from io import StringIO
import unittest
from unittest.mock import MagicMock, call, patch

from . import scheme_mock
from update_checkout.git_command import GitException
from update_checkout.retry import exponential_retry
from update_checkout.update_checkout import main


def _quietly(fn, *args, **kwargs):
    with contextlib.redirect_stdout(StringIO()):
        return fn(*args, **kwargs)


class ExponentialRetryTestCase(unittest.TestCase):
    """Unit coverage for the exponential_retry decorator itself."""

    @patch("update_checkout.retry.time")
    def test_success_returns_without_sleeping(self, mock_time):
        fn = MagicMock(return_value=0)

        result = _quietly(exponential_retry(max_retries=3)(fn))

        self.assertEqual(result, 0)
        fn.assert_called_once_with()
        mock_time.sleep.assert_not_called()

    @patch("update_checkout.retry.time")
    def test_none_return_counts_as_success(self, mock_time):
        # The per-repository clone/update workers return None on success and
        # report failures by raising.
        fn = MagicMock(return_value=None)

        result = _quietly(exponential_retry(max_retries=3)(fn))

        self.assertIsNone(result)
        fn.assert_called_once_with()
        mock_time.sleep.assert_not_called()

    @patch("update_checkout.retry.time")
    def test_nonzero_return_is_retried_with_exponential_backoff(self, mock_time):
        fn = MagicMock(side_effect=[1, 1, 0])

        result = _quietly(exponential_retry(max_retries=3)(fn))

        self.assertEqual(result, 0)
        self.assertEqual(fn.call_count, 3)
        mock_time.sleep.assert_has_calls([call(10.0), call(40.0)])
        self.assertEqual(mock_time.sleep.call_count, 2)

    @patch("update_checkout.retry.time")
    def test_exception_is_reraised_after_exhausting_retries(self, mock_time):
        fn = MagicMock(side_effect=RuntimeError("transient network failure"))

        with self.assertRaisesRegex(RuntimeError, "transient network failure"):
            _quietly(exponential_retry(max_retries=2)(fn))

        # One initial attempt plus two retries.
        self.assertEqual(fn.call_count, 3)
        mock_time.sleep.assert_has_calls([call(10.0), call(40.0)])
        self.assertEqual(mock_time.sleep.call_count, 2)

    @patch("update_checkout.retry.time")
    def test_zero_max_retries_disables_retrying(self, mock_time):
        fn = MagicMock(side_effect=RuntimeError("boom"))

        with self.assertRaises(RuntimeError):
            _quietly(exponential_retry(max_retries=0)(fn))

        fn.assert_called_once_with()
        mock_time.sleep.assert_not_called()


class PerRepoRetryTestCase(scheme_mock.SchemeMockTestCase):
    """A transient failure of one repository's clone/update operation must be
    retried for that repository alone, with backoff, without re-running the
    whole checkout for every other repository.

    The workers are patched with plain functions (rather than MagicMocks)
    because ParallelRunner reads ``fn.__name__``, which mocks reject."""

    def _run_main(self, extra_args):
        with patch("sys.argv", ["update-checkout"] + extra_args):
            with contextlib.redirect_stdout(StringIO()):
                main()

    @patch("update_checkout.retry.time")
    @patch("sys.exit", return_value=None)
    def test_clone_failure_is_retried_without_rerunning_other_repos(
        self, mock_exit, mock_time
    ):
        calls = {"repo1": 0, "repo2": 0}

        def flaky_clone(pool_args):
            calls[pool_args.repo_name] += 1
            if pool_args.repo_name == "repo1" and calls["repo1"] == 1:
                raise GitException(
                    128,
                    ["clone", "origin"],
                    "repo1",
                    "simulated transient network failure",
                )
            return None

        with patch(
            "update_checkout.update_checkout.obtain_additional_swift_sources",
            new=flaky_clone,
        ):
            self._run_main(
                [
                    "--config",
                    self.config_path,
                    "--source-root",
                    self.source_root,
                    "--clone",
                    "--max-retries",
                    "1",
                ]
            )

        # repo1 was attempted twice (initial + one retry with backoff) and
        # repo2 only once: a whole-run retry would have attempted repo2 twice.
        self.assertEqual(calls, {"repo1": 2, "repo2": 1})
        mock_time.sleep.assert_called_once_with(10.0)
        mock_exit.assert_called_once_with(0)

    @patch("update_checkout.retry.time")
    @patch("sys.exit", return_value=None)
    def test_update_failure_is_retried_without_rerunning_other_repos(
        self, mock_exit, mock_time
    ):
        calls = {"repo1": 0, "repo2": 0}

        def flaky_update(pool_args):
            calls[pool_args.repo_name] += 1
            if pool_args.repo_name == "repo1" and calls["repo1"] == 1:
                raise GitException(
                    128,
                    ["fetch", "origin"],
                    "repo1",
                    "simulated transient network failure",
                )
            return None

        # Start from a fully cloned source root so the update phase runs.
        self.call(self.base_args + ["--clone"])

        with patch(
            "update_checkout.update_checkout.update_single_repository",
            new=flaky_update,
        ):
            self._run_main(
                [
                    "--config",
                    self.config_path,
                    "--source-root",
                    self.source_root,
                    "--max-retries",
                    "1",
                ]
            )

        self.assertEqual(calls, {"repo1": 2, "repo2": 1})
        mock_time.sleep.assert_called_once_with(10.0)
        mock_exit.assert_called_once_with(0)
