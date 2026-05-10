import subprocess
import tempfile
from typing import List
import unittest
from pathlib import Path

from .scheme_mock import UPDATE_CHECKOUT_PATH


class TestStatusCommand(unittest.TestCase):
    def call(self, args: List[str], cwd: Path):
        subprocess.check_call(
            args, cwd=cwd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL
        )

    def init_repo(self, path: Path, with_changes: bool):
        self.call(args=["git", "init"], cwd=path)
        self.call(
            args=["git", "config", "--local", "user.name", "swift_test"], cwd=path
        )
        self.call(
            args=["git", "config", "--local", "user.email", "no-reply@swift.org"],
            cwd=path,
        )
        self.call(args=["git", "config", "commit.gpgsign", "false"], cwd=path)

        (path / "file.txt").write_text("initial\n")
        self.call(args=["git", "add", "file.txt"], cwd=path)
        self.call(args=["git", "commit", "-m", "initial commit"], cwd=path)

        if with_changes:
            (path / "file.txt").write_text("modified\n")

    def test_repositories_with_active_changes(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            root = Path(tmpdir)

            repo_1 = root / "repo_1"
            repo_2 = root / "repo_2"
            repo_1.mkdir()
            repo_2.mkdir()

            self.init_repo(repo_1, True)
            self.init_repo(repo_2, True)

            output = subprocess.run(
                [UPDATE_CHECKOUT_PATH, "--source-root", str(root), "status"],
                capture_output=True,
                text=True,
            ).stdout

            self.assertIn("The following repositories have active changes:", output)
            self.assertIn("  - [repo_1] 1 active change", output)
            self.assertIn("  - [repo_2] 1 active change", output)

    def test_repositories_without_active_changes(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            root = Path(tmpdir)

            repo_1 = root / "repo_1"
            repo_2 = root / "repo_2"
            repo_1.mkdir()
            repo_2.mkdir()

            self.init_repo(repo_1, False)
            self.init_repo(repo_2, False)

            output = subprocess.run(
                [UPDATE_CHECKOUT_PATH, "--source-root", str(root), "status"],
                capture_output=True,
                text=True,
            ).stdout

            self.assertIn("No repositories have active changes.", output)


if __name__ == "__main__":
    unittest.main()
