from pathlib import Path
import re
import shlex
import subprocess
import sys
from typing import List, Any, Optional, Dict, Set, Tuple

from .runner_arguments import RunnerArguments


class GitException(Exception):
    """
    Exception raised when a Git command execution fails.

    Attributes
    ----------
    returncode : int
        The return code from the failed Git command.
    command : List[str]
        The Git command that was executed.
    repo_name : str
        The name of the Git repository.
    stderr : str
        The output of the failed Git command.
    """

    def __init__(
        self,
        returncode: int,
        command: List[str],
        repo_name: str,
        output: str,
    ):
        super().__init__()
        self.returncode = returncode
        self.command = command
        self.repo_name = repo_name
        self.stderr = output

    def __str__(self):
        return (
            f"[{self.repo_name}] '{Git._quote_command(self.command)}' "
            f"returned ({self.returncode}) with the following {self.stderr}."
        )


class Git:
    @staticmethod
    def run(
        repo_path: Path,
        args: List[str],
        echo: bool = False,
        env: Optional[Dict[str, Any]] = None,
        prefix: str = "",
        allow_non_zero_exit: bool = False,
        fatal: bool = False,
        **kwargs,
    ) -> Tuple[str, int, List[str]]:
        command = Git._build_command(args)
        output = ""
        try:
            result = subprocess.run(
                command,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                universal_newlines=True,
                encoding="utf-8",
                env=env,
                cwd=repo_path,
                **kwargs,
            )
            output = result.stdout
            if echo:
                Git._echo_command(command, output, env, prefix)
            if not allow_non_zero_exit:
                result.check_returncode()
        except subprocess.CalledProcessError as e:
            if fatal:
                sys.exit(
                    f"command `{command}` terminated with a non-zero exit "
                    f"status {str(e.returncode)}, aborting"
                )
            raise GitException(e.returncode, command, repo_path.name, output)
        except OSError as e:
            if fatal:
                sys.exit(
                    f"could not execute '{Git._quote_command(command)}': {e.strerror}"
                )
        return (output.strip(), result.returncode, command)

    @staticmethod
    def _echo_command(
        command: List[str],
        output: Optional[str] = None,
        env: Optional[Dict[str, Any]] = None,
        prefix: str = "",
    ):
        sys.stdout.flush()
        sys.stderr.flush()
        command_str = []
        if env is not None:
            command_str += ["env"] + [
                Git._quote(f"{k}={v}") for (k, v) in sorted(env.items())
            ]
        command_str.append(Git._quote_command(command))
        print(f"{prefix}+ {' '.join(command_str)}", file=sys.stderr)
        if output:
            for line in output.splitlines():
                print(prefix + line)
        sys.stdout.flush()
        sys.stderr.flush()

    @staticmethod
    def _build_command(args: List[str]) -> List[str]:
        return ["git"] + args

    @staticmethod
    def _quote(arg: Any) -> str:
        return shlex.quote(str(arg))

    @staticmethod
    def _quote_command(command: List[Any]) -> str:
        return " ".join(Git._quote(arg) for arg in command)


def is_git_repository(path: Path) -> bool:
    """Returns whether a Path object is a Git repository.

    Args:
        path (Path): The path object to check.

    Returns:
        bool: Whether the path is a Git repository.
    """

    if not path.is_dir():
        return False
    git_dir_path = path.joinpath(".git")
    return git_dir_path.exists() and git_dir_path.is_dir()


def is_any_repository_locked(pool_args: List[RunnerArguments]) -> Set[str]:
    """Returns the set of locked repositories.

    A repository is considered to be locked if its .git directory contains a
    file ending in ".lock".

    Args:
        pool_args (List[RunnerArguments]): List of arguments passed to the
        `update_single_repository` function.

    Returns:
        Set[str]: The names of the locked repositories if any.
    """

    repos = [(x.source_root, x.repo_name) for x in pool_args]
    locked_repositories = set()
    for source_root, repo_name in repos:
        dot_git_path = source_root.joinpath(repo_name, ".git")
        if not dot_git_path.exists() or not dot_git_path.is_dir():
            continue
        for file in dot_git_path.iterdir():
            if file.suffix == ".lock":
                locked_repositories.add(repo_name)
    return locked_repositories

def is_commit_hash(ref: str):
    """Check if ref looks like a commit hash"""
    return bool(re.match(r'^[0-9a-f]{7,40}$', ref, re.IGNORECASE))
