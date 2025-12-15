from pathlib import Path
import unittest
from unittest.mock import patch

from update_checkout.update_checkout import UpdateArguments
from update_checkout.git_command import is_any_repository_locked

FAKE_PATH = Path("/fake_path")


def _update_arguments_with_fake_path(repo_name: str, path: Path) -> UpdateArguments:
    return UpdateArguments(
        repo_name=repo_name,
        source_root=path,
        config={},
        scheme_name="",
        scheme_map=None,
        tag="",
        timestamp=None,
        reset_to_remote=False,
        clean=False,
        stash=False,
        cross_repos_pr={},
        output_prefix="",
        verbose=False,
    )


class TestIsAnyRepositoryLocked(unittest.TestCase):
    @patch("pathlib.Path.exists", autospec=True)
    @patch("pathlib.Path.is_dir", autospec=True)
    @patch("pathlib.Path.iterdir", autospec=True)
    def test_repository_with_lock_file(self, mock_iterdir, mock_is_dir, mock_exists):
        pool_args = [
            _update_arguments_with_fake_path("repo1", FAKE_PATH),
            _update_arguments_with_fake_path("repo2", FAKE_PATH),
        ]

        def iterdir_side_effect(path: Path):
            if "repo1" in path.as_posix():
                return [path.joinpath("index.lock"), path.joinpath("config")]
            elif "repo2" in path.as_posix():
                return [path.joinpath("HEAD"), path.joinpath("config")]
            return []

        mock_exists.return_value = True
        mock_is_dir.return_value = True
        mock_iterdir.side_effect = iterdir_side_effect

        result = is_any_repository_locked(pool_args)
        self.assertEqual(result, {"repo1"})

    @patch("pathlib.Path.exists")
    @patch("pathlib.Path.is_dir")
    @patch("pathlib.Path.iterdir")
    def test_repository_without_git_dir(self, mock_iterdir, mock_is_dir, mock_exists):
        pool_args = [
            _update_arguments_with_fake_path("repo1", FAKE_PATH),
        ]

        mock_exists.return_value = False
        mock_is_dir.return_value = False
        mock_iterdir.return_value = []

        result = is_any_repository_locked(pool_args)
        self.assertEqual(result, set())

    @patch("pathlib.Path.exists")
    @patch("pathlib.Path.is_dir")
    @patch("pathlib.Path.iterdir")
    def test_repository_with_git_file(self, mock_iterdir, mock_is_dir, mock_exists):
        pool_args = [
            _update_arguments_with_fake_path("repo1", FAKE_PATH),
        ]

        mock_exists.return_value = True
        mock_is_dir.return_value = False
        mock_iterdir.return_value = []

        result = is_any_repository_locked(pool_args)
        self.assertEqual(result, set())

    @patch("pathlib.Path.exists")
    @patch("pathlib.Path.is_dir")
    @patch("pathlib.Path.iterdir")
    def test_repository_with_multiple_lock_files(
        self, mock_iterdir, mock_is_dir, mock_exists
    ):
        pool_args = [
            _update_arguments_with_fake_path("repo1", FAKE_PATH),
        ]

        mock_exists.return_value = True
        mock_is_dir.return_value = True
        mock_iterdir.return_value = [
            FAKE_PATH.joinpath(x) for x in ("index.lock", "merge.lock", "HEAD")
        ]

        result = is_any_repository_locked(pool_args)
        self.assertEqual(result, {"repo1"})

    @patch("pathlib.Path.exists")
    @patch("pathlib.Path.is_dir")
    @patch("pathlib.Path.iterdir")
    def test_repository_with_no_lock_files(
        self, mock_iterdir, mock_is_dir, mock_exists
    ):
        pool_args = [
            _update_arguments_with_fake_path("repo1", FAKE_PATH),
        ]

        mock_exists.return_value = True
        mock_is_dir.return_value = True
        mock_iterdir.return_value = [
            FAKE_PATH.joinpath(x) for x in ("HEAD", "config", "logs")
        ]

        result = is_any_repository_locked(pool_args)
        self.assertEqual(result, set())
