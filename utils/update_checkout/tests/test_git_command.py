import subprocess
import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch

from update_checkout.git_command import Git, GitException
from update_checkout.update_checkout import confirm_tag_in_repo

FAKE_PATH = Path("/fake_path")

SSH_WARNING = (
    "** WARNING: connection is not using a post-quantum key exchange algorithm.\n"
    "** This session may be vulnerable to \"store now, decrypt later\" attacks.\n"
    "** The server may need to be upgraded. See https://openssh.com/pq.html\n"
)


def _make_process_result(stdout="", stderr="", returncode=0):
    result = MagicMock()
    result.stdout = stdout
    result.stderr = stderr
    result.returncode = returncode
    if returncode != 0:
        result.check_returncode.side_effect = subprocess.CalledProcessError(
            returncode, ["git"]
        )
    return result


class TestGitRunStderrIsolation(unittest.TestCase):
    @patch("subprocess.run")
    def test_ssh_warning_on_stderr_not_returned_as_output(self, mock_run):
        """SSH warnings emitted to stderr must not appear in the returned output."""
        mock_run.return_value = _make_process_result(stdout="", stderr=SSH_WARNING)

        output, _, _ = Git.run(FAKE_PATH, ["ls-remote", "--tags", "origin", "some-tag"])

        self.assertEqual(output, "")

    @patch("subprocess.run")
    def test_stdout_returned_when_tag_exists(self, mock_run):
        """Real tag data in stdout is returned correctly even with SSH warnings."""
        tag_line = "abc123\trefs/tags/fake-tag\n"
        mock_run.return_value = _make_process_result(
            stdout=tag_line, stderr=SSH_WARNING
        )

        output, _, _ = Git.run(FAKE_PATH, ["ls-remote", "--tags", "origin", "some-tag"])

        self.assertIn("refs/tags/", output)

    @patch("subprocess.run")
    def test_stderr_included_in_exception_on_failure(self, mock_run):
        """On failure, GitException must include stderr content for diagnostics."""
        mock_run.return_value = _make_process_result(
            stdout="", stderr=SSH_WARNING, returncode=128
        )

        with self.assertRaises(GitException) as ctx:
            Git.run(FAKE_PATH, ["fetch", "origin", "some-tag"])

        self.assertIn("post-quantum", ctx.exception.stderr)


class TestConfirmTagInRepo(unittest.TestCase):
    @patch("builtins.print")
    @patch("subprocess.run")
    def test_returns_none_when_tag_absent_with_ssh_warning(self, mock_run, _mock_print):
        """confirm_tag_in_repo must return None when the tag does not exist,
        even if an SSH warning is present on stderr.
        """
        mock_run.return_value = _make_process_result(stdout="", stderr=SSH_WARNING)

        result = confirm_tag_in_repo(FAKE_PATH, "fake-tag", "llbuild")

        self.assertIsNone(result)

    @patch("subprocess.run")
    def test_returns_tag_when_tag_exists_with_ssh_warning(self, mock_run):
        """confirm_tag_in_repo must return the tag name when it exists,
        regardless of SSH warnings on stderr.
        """
        tag_line = "abc123\trefs/tags/fake-tag\n"
        mock_run.return_value = _make_process_result(
            stdout=tag_line, stderr=SSH_WARNING
        )

        result = confirm_tag_in_repo(FAKE_PATH, "fake-tag", "llbuild")

        self.assertEqual(result, "fake-tag")

    @patch("builtins.print")
    @patch("subprocess.run")
    def test_returns_none_when_tag_absent_without_ssh_warning(self, mock_run, _mock_print):
        """confirm_tag_in_repo must return None for a missing tag (baseline case)."""
        mock_run.return_value = _make_process_result(stdout="", stderr="")

        result = confirm_tag_in_repo(FAKE_PATH, "fake-tag", "llbuild")

        self.assertIsNone(result)
