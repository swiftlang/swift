import unittest
from unittest.mock import patch

from update_checkout.update_checkout import _is_any_repository_locked

class TestIsAnyRepositoryLocked(unittest.TestCase):
    @patch("os.path.exists")
    @patch("os.path.isdir")
    @patch("os.listdir")
    def test_repository_with_lock_file(self, mock_listdir, mock_isdir, mock_exists):
        pool_args = [
            ("/fake_path", None, "repo1"),
            ("/fake_path", None, "repo2"),
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
            ("/fake_path", None, "repo1"),
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
            ("/fake_path", None, "repo1"),
        ]

        mock_exists.return_value = True
        mock_isdir.return_value = False
        mock_listdir.return_value = []

        result = _is_any_repository_locked(pool_args)
        self.assertEqual(result, set())

    @patch("os.path.exists")
    @patch("os.path.isdir")
    @patch("os.listdir")
    def test_repository_with_multiple_lock_files(self, mock_listdir, mock_isdir, mock_exists):
        pool_args = [
            ("/fake_path", None, "repo1"),
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
            ("/fake_path", None, "repo1"),
        ]

        mock_exists.return_value = True
        mock_isdir.return_value = True
        mock_listdir.return_value = ["HEAD", "config", "logs"]

        result = _is_any_repository_locked(pool_args)
        self.assertEqual(result, set())

