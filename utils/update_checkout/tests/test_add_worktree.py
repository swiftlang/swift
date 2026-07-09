import json
import os

from . import scheme_mock


ADD_WORKTREE_PATH = os.path.abspath(
    os.path.join(
        scheme_mock.CURRENT_FILE_DIR,
        os.path.pardir,
        os.path.pardir,
        "add-worktree",
    )
)


class AddWorktreeTestCase(scheme_mock.SchemeMockTestCase):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.worktree_dir = os.path.join(self.workspace, "worktrees")

    def clone_source_root(self):
        self.call(self.base_args + ["--clone"])

    def head_commit(self, repo_path):
        return self.call(["git", "rev-parse", "HEAD"], cwd=repo_path).strip()

    def is_detached(self, repo_path):
        ref = self.call(
            ["git", "rev-parse", "--abbrev-ref", "HEAD"], cwd=repo_path
        ).strip()
        return ref == "HEAD"

    def test_creates_detached_worktrees(self):
        self.clone_source_root()
        self.call(
            [ADD_WORKTREE_PATH, "--source-root", self.source_root, self.worktree_dir]
        )

        for repo in self.repo_names:
            worktree_path = os.path.join(self.worktree_dir, repo)
            self.assertTrue(os.path.isdir(worktree_path))
            # A worktree's '.git' is a file, not a directory.
            self.assertTrue(os.path.isfile(os.path.join(worktree_path, ".git")))
            self.assertTrue(self.is_detached(worktree_path))
            self.assertEqual(
                self.head_commit(worktree_path),
                self.head_commit(os.path.join(self.source_root, repo)),
            )

    def test_idempotent(self):
        self.clone_source_root()
        command = [
            ADD_WORKTREE_PATH,
            "--source-root",
            self.source_root,
            self.worktree_dir,
        ]
        self.call(command)
        # A second run must not fail even though the worktrees already exist.
        output = self.call(command)
        self.assertIn("add-worktree succeeded", output)

    def test_scheme_switch_detaches_on_collision(self):
        self.clone_source_root()
        # The scheme's branches match the primary checkout's branches, so every
        # worktree collides on checkout and must fall back to a detached HEAD.
        output = self.call(
            [
                ADD_WORKTREE_PATH,
                "--source-root",
                self.source_root,
                "--config",
                self.config_path,
                "--scheme",
                "main",
                self.worktree_dir,
            ]
        )
        self.assertIn("add-worktree succeeded", output)
        for repo in self.repo_names:
            self.assertTrue(self.is_detached(os.path.join(self.worktree_dir, repo)))


class AddWorktreeSchemeTestCase(scheme_mock.SchemeMockTestCase):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.worktree_dir = os.path.join(self.workspace, "worktrees")

    def setUp(self):
        super().setUp()

        # Advance 'main' so the primary checkout and the target scheme end up at
        # different commits, letting us prove the scheme actually moves the
        # worktree.
        local_repo1 = os.path.join(self.local_path, "repo1")
        self.call(["git", "commit", "--allow-empty", "-m", "second"], cwd=local_repo1)
        self.call(["git", "push", "origin", "main"], cwd=local_repo1)
        self.call(self.base_args + ["--clone"])

        remote_repo1 = self.remote_path(repo_name="repo1")
        commits = self.call(["git", "rev-list", "main"], cwd=remote_repo1).split()
        self.old_commit = commits[-1]

        self.scheme_name = "commit-scheme"
        self.add_branch_scheme(
            self.scheme_name,
            {
                "aliases": [self.scheme_name],
                "repos": {"repo1": self.old_commit, "repo2": "main"},
            },
        )
        with open(self.config_path, "w") as f:
            json.dump(self.config, f)

    def test_scheme_switches_commit(self):
        self.call(
            [
                ADD_WORKTREE_PATH,
                "--source-root",
                self.source_root,
                "--config",
                self.config_path,
                "--scheme",
                self.scheme_name,
                self.worktree_dir,
            ]
        )

        worktree_repo1 = os.path.join(self.worktree_dir, "repo1")
        current_commit = self.call(
            ["git", "rev-parse", "HEAD"], cwd=worktree_repo1
        ).strip()
        self.assertEqual(current_commit, self.old_commit)


class AddWorktreeBranchTestCase(scheme_mock.SchemeMockTestCase):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.worktree_dir = os.path.join(self.workspace, "worktrees")

        # '--llvm-branch' and '--swift-branch' operate on repositories named
        # 'llvm-project' and 'swift', so add them to the mock configuration.
        for repo_name in ["llvm-project", "swift"]:
            self.config["repos"][repo_name] = {"remote": {"id": repo_name}}
            self.config["branch-schemes"]["main"]["repos"][repo_name] = "main"

    def current_branch(self, repo_path):
        return self.call(
            ["git", "rev-parse", "--abbrev-ref", "HEAD"], cwd=repo_path
        ).strip()

    def test_creates_and_checks_out_branches(self):
        self.call(self.base_args + ["--clone"])
        command = [
            ADD_WORKTREE_PATH,
            "--source-root",
            self.source_root,
            "--llvm-branch",
            "my-llvm",
            "--swift-branch",
            "my-swift",
            self.worktree_dir,
        ]
        self.call(command)

        self.assertEqual(
            self.current_branch(os.path.join(self.worktree_dir, "llvm-project")),
            "my-llvm",
        )
        self.assertEqual(
            self.current_branch(os.path.join(self.worktree_dir, "swift")),
            "my-swift",
        )
        # A repository without a requested branch stays detached.
        self.assertEqual(
            self.current_branch(os.path.join(self.worktree_dir, "repo1")),
            "HEAD",
        )

        # A second run checks out the now-existing branches rather than failing.
        output = self.call(command)
        self.assertIn("add-worktree succeeded", output)
        self.assertEqual(
            self.current_branch(os.path.join(self.worktree_dir, "swift")),
            "my-swift",
        )


class AddWorktreeSharedSchemeTestCase(scheme_mock.SchemeMockTestCase):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.worktree_dir_a = os.path.join(self.workspace, "worktrees-a")
        self.worktree_dir_b = os.path.join(self.workspace, "worktrees-b")

    def setUp(self):
        super().setUp()

        # Create a 'feature' branch on each remote that the primary checkout
        # will not have checked out, so the scheme's branch differs from the
        # primary's branch. This lets the first worktree claim the branch while
        # a second worktree must fall back to a detached HEAD.
        for repo_name in self.repo_names:
            local_repo = os.path.join(self.local_path, repo_name)
            self.call(["git", "checkout", "-b", "feature"], cwd=local_repo)
            self.call(["git", "commit", "--allow-empty", "-m", "F"], cwd=local_repo)
            self.call(["git", "push", "origin", "feature"], cwd=local_repo)
            self.call(["git", "checkout", "main"], cwd=local_repo)

        self.scheme_name = "feature-scheme"
        self.add_branch_scheme(
            self.scheme_name,
            {
                "aliases": [self.scheme_name],
                "repos": {repo: "feature" for repo in self.repo_names},
            },
        )
        with open(self.config_path, "w") as f:
            json.dump(self.config, f)

        # Populate the primary checkout on 'main', not 'feature'.
        self.call(self.base_args + ["--clone"])

    def current_branch(self, repo_path):
        return self.call(
            ["git", "rev-parse", "--abbrev-ref", "HEAD"], cwd=repo_path
        ).strip()

    def head_commit(self, repo_path):
        return self.call(["git", "rev-parse", "HEAD"], cwd=repo_path).strip()

    def test_second_worktree_detaches_from_scheme_branch(self):
        scheme_args = [
            ADD_WORKTREE_PATH,
            "--source-root",
            self.source_root,
            "--config",
            self.config_path,
            "--scheme",
            self.scheme_name,
        ]

        # The first worktree claims the scheme's branch.
        self.call(scheme_args + [self.worktree_dir_a])
        repo1_a = os.path.join(self.worktree_dir_a, "repo1")
        self.assertEqual(self.current_branch(repo1_a), "feature")

        # The second worktree can't check out a branch already used by the
        # first, so it falls back to a detached HEAD at the same commit and
        # still succeeds.
        output = self.call(scheme_args + [self.worktree_dir_b])
        self.assertIn("add-worktree succeeded", output)
        repo1_b = os.path.join(self.worktree_dir_b, "repo1")
        self.assertEqual(self.current_branch(repo1_b), "HEAD")
        self.assertEqual(self.head_commit(repo1_a), self.head_commit(repo1_b))
