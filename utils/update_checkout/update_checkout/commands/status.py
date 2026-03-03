from typing import List, Tuple

from ..parallel_runner import (
    ParallelRunner,
    RunnerArguments,
)
from ..runner_arguments import UpdateArguments
from ..cli_arguments import CliArguments
from ..git_command import (
    Git,
    is_any_repository_locked,
    is_git_repository,
)


class StatusCommand:
    """
    The status command prints the list of repositories with active changes
    (uncommitted, untracked and/or staged).

    Its goal is to help users determine if it's safe to run the script with
    --reset-to-remote.
    """
    _args: CliArguments
    _pool_args: List[UpdateArguments]

    def __init__(self, args: CliArguments):
        self._args = args
        self._pool_args = []
        for repo_path in args.source_root.iterdir():
            if not is_git_repository(repo_path):
                continue
            repo_name = repo_path.name

            my_args = RunnerArguments(
                repo_name=repo_name,
                output_prefix="Checking the status of",
                source_root=args.source_root,
                verbose=args.verbose,
            )
            self._pool_args.append(my_args)

    def run(self):
        # TODO: If we add more commands, make error handling more generic
        locked_repositories = is_any_repository_locked(self._pool_args)
        if len(locked_repositories) > 0:
            results = [
                Exception(f"'{repo_name}' is locked by git. Cannot update it.")
                for repo_name in locked_repositories
            ]
        else:
            results = ParallelRunner(
                self._get_uncommitted_changes_of_repository,
                self._pool_args,
                self._args.n_processes,
            ).run()

        return self._handle_results(results)

    def _handle_results(self, results) -> int:
        ret = ParallelRunner.check_results(results, "STATUS")
        if ret > 0:
            return ret

        repos_with_active_changes: List[Tuple[str, int]] = []
        for result, pool_arg in zip(results, self._pool_args):
            if result is None or result[0] == "":
                continue
            status_output: str = result[0]
            repos_with_active_changes.append((pool_arg.repo_name, len(status_output.splitlines())))

        if len(repos_with_active_changes) == 0:
            print("No repositories have active changes.")
        else:
            print("The following repositories have active changes:")
            for repo_name, file_count in repos_with_active_changes:
                msg = f"  - [{repo_name}] {file_count} active change"
                if file_count > 1:
                    msg += "s"
                print(msg)

        return 0

    @staticmethod
    def _get_uncommitted_changes_of_repository(pool_args: RunnerArguments):
        repo_path = pool_args.source_root.joinpath(pool_args.repo_name)
        return Git.run(repo_path, ["status", "--short"], fatal=True)
