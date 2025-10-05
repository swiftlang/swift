import unittest
from unittest.mock import patch

from update_checkout.update_checkout import UpdateArguments, _is_any_repository_locked


def _update_arguments_with_fake_path(repo_name: str, path: str) -> UpdateArguments:
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
        cross_repos_pr=False,
        output_prefix="",
        verbose=False,
    )


class TestIsAnyRepositoryLocked(unittest.TestCase):
    @patch("os.path.exists")
    @patch("os.path.isdir")
    @patch("os.listdir")
    def test_repository_with_lock_file(self, mock_listdir, mock_isdir, mock_exists):
        pool_args = [
            _update_arguments_with_fake_path("repo1", "/fake_path"),
            _update_arguments_with_fake_path("repo2", "/fake_path"),
        ]

        def listdir_side_effect(path):
            if "repo1" in path:
                return ["index.lock", "config"]
            elif "repo2" in path:
                return ["HEAD", "config"]
            return []

        mock_exists.return_value = True
        mock_isdir.return_value = True
        mock_listdir.side_effect = listdir_side_effect

        result = _is_any_repository_locked(pool_args)
        self.assertEqual(result, {"repo1"})

    @patch("os.path.exists")
    @patch("os.path.isdir")
    @patch("os.listdir")
    def test_repository_without_git_dir(self, mock_listdir, mock_isdir, mock_exists):
        pool_args = [
            _update_arguments_with_fake_path("repo1", "/fake_path"),
        ]

        mock_exists.return_value = False
        mock_isdir.return_value = False
        mock_listdir.return_value = []

        result = _is_any_repository_locked(pool_args)
        self.assertEqual(result, set())

    @patch("os.path.exists")
    @patch("os.path.isdir")
    @patch("os.listdir")
    def test_repository_with_git_file(self, mock_listdir, mock_isdir, mock_exists):
        pool_args = [
            _update_arguments_with_fake_path("repo1", "/fake_path"),
        ]

        mock_exists.return_value = True
        mock_isdir.return_value = False
        mock_listdir.return_value = []

        result = _is_any_repository_locked(pool_args)
        self.assertEqual(result, set())

    @patch("os.path.exists")
    @patch("os.path.isdir")
    @patch("os.listdir")
    def test_repository_with_multiple_lock_files(
        self, mock_listdir, mock_isdir, mock_exists
    ):
        pool_args = [
            _update_arguments_with_fake_path("repo1", "/fake_path"),
        ]

        mock_exists.return_value = True
        mock_isdir.return_value = True
        mock_listdir.return_value = ["index.lock", "merge.lock", "HEAD"]

        result = _is_any_repository_locked(pool_args)
        self.assertEqual(result, {"repo1"})

    @patch("os.path.exists")
    @patch("os.path.isdir")
    @patch("os.listdir")
    def test_repository_with_no_lock_files(self, mock_listdir, mock_isdir, mock_exists):
        pool_args = [
            _update_arguments_with_fake_path("repo1", "/fake_path"),
        ]

        mock_exists.return_value = True
        mock_isdir.return_value = True
        mock_listdir.return_value = ["HEAD", "config", "logs"]

        result = _is_any_repository_locked(pool_args)
        self.assertEqual(result, set())
