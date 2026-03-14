# utils/update_checkout.py - Utility to update local checkouts --*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import json
import os
from pathlib import Path
import platform
import re
import subprocess
import sys
import traceback
from typing import Any, Dict, Hashable, Optional, List, Tuple, Union

from .cli_arguments import CliArguments
from .git_command import Git, GitException, is_any_repository_locked, is_commit_hash
from .retry import exponential_retry
from .runner_arguments import AdditionalSwiftSourcesArguments, UpdateArguments
from .parallel_runner import ParallelRunner
from .commands import status


SCRIPT_FILE = Path(__file__).absolute()
SCRIPT_DIR = SCRIPT_FILE.parent


class SkippedReason:
    def __init__(self, repo_name: str, reason: str):
        self.repo_name = repo_name
        self.reason = reason

    @staticmethod
    def print_skipped_repositories(skipped_reasons: List["SkippedReason"], step: str):
        if not skipped_reasons:
            return
        print(f"Skipped {step}:")
        for reason in skipped_reasons:
            print(f"  '{reason.repo_name}' - {reason.reason}")


def confirm_tag_in_repo(repo_path: Path, tag: str, repo_name: str) -> Optional[str]:
    """Confirm that a given tag exists in a git repository. This function
    assumes that the repository is already a current working directory before
    it's called.

    Args:
        repo_path (Path): path to the repository
        tag (str): tag to look up in the repository
        repo_name (str): name the repository for the look up, used for logging

    Returns:
        str | None: returns `tag` argument value or `None` if the tag doesn't
        exist.
    """

    tag_exists, _, _ = Git.run(
        repo_path, ["ls-remote", "--tags", "origin", tag], fatal=True
    )
    if not tag_exists:
        print(
            "Tag '"
            + tag
            + "' does not exist for '"
            + repo_name
            + "', just updating regularly"
        )
        return None
    return tag


def find_rev_by_timestamp(
    repo_path: Path, timestamp: str, repo_name: str, refspec: str
) -> str:
    refspec_exists = True
    try:
        Git.run(repo_path, ["rev-parse", "--verify", refspec])
    except Exception:
        refspec_exists = False
    args = ["log", "-1", "--format=%H", "--first-parent", "--before=" + timestamp]
    if refspec_exists:
        args.append(refspec)
    rev, _, _ = Git.run(repo_path, args, fatal=True)
    if rev:
        return rev
    else:
        raise RuntimeError("No rev in %s before timestamp %s" % (repo_name, timestamp))


def get_branch_for_repo(
    repo_path: Path,
    config: Dict[str, Any],
    repo_name: str,
    scheme_name: str,
    scheme_map: Optional[Dict[str, str]],
    cross_repos_pr: Dict[str, str],
):
    """Infer, fetch, and return a branch corresponding to a given PR, otherwise
    return a branch found in the config for this repository name.

    Args:
        repo_path (Path): path to the repository
        config (Dict[str, Any]): deserialized `update-checkout-config.json`
        repo_name (str): name of the repository for checking out the branch
        scheme_name (str): name of the scheme to look up in the config
        scheme_map (Dict[str, str] | None): map of repo names to branches to check out
        cross_repos_pr (Dict[str, str]): map of repo ids to PRs to check out

    Returns:
        Tuple[str, bool]: a pair of a checked out branch and a boolean
        indicating whether this repo matched any `cross_repos_pr`.
    """

    cross_repo = False
    repo_branch = scheme_name
    if scheme_map:
        scheme_branch = scheme_map[repo_name]
        repo_branch = scheme_branch
        remote_repo_id = config["repos"][repo_name]["remote"]["id"]
        if remote_repo_id in cross_repos_pr:
            cross_repo = True
            pr_id = cross_repos_pr[remote_repo_id]
            repo_branch = "ci_pr_{0}".format(pr_id)
            Git.run(repo_path, ["checkout", scheme_branch], echo=True)
            Git.run(
                repo_path,
                ["branch", "-D", repo_branch],
                echo=True,
                allow_non_zero_exit=True,
                fatal=True,
            )
            Git.run(
                repo_path,
                [
                    "fetch",
                    "origin",
                    "pull/{0}/merge:{1}".format(pr_id, repo_branch),
                    "--tags",
                ],
                echo=True,
            )
    return repo_branch, cross_repo


def update_single_repository(pool_args: UpdateArguments):
    verbose = pool_args.verbose
    repo_name = pool_args.repo_name

    repo_path = pool_args.source_root.joinpath(repo_name)
    if not repo_path.is_dir() or repo_path.is_symlink():
        return

    try:
        prefix = f"[{repo_path.name}] ".ljust(40)
        if verbose:
            print(f"{prefix}Updating '{repo_path}'")

        cross_repo = False
        checkout_target = None
        if pool_args.tag:
            checkout_target = confirm_tag_in_repo(repo_path, pool_args.tag, repo_name)
        elif pool_args.scheme_name:
            checkout_target, cross_repo = get_branch_for_repo(
                repo_path,
                pool_args.config,
                repo_name,
                pool_args.scheme_name,
                pool_args.scheme_map,
                pool_args.cross_repos_pr,
            )
            if pool_args.timestamp:
                checkout_target = find_rev_by_timestamp(
                    repo_path, pool_args.timestamp, repo_name, checkout_target
                )

        # The '--clean' and '--stash' options
        # 1. clear the index and working tree ('--stash' stashes those
        #   changes rather than discarding them)
        # 2. delete ignored files
        # 3. abort an ongoing rebase
        if pool_args.clean or pool_args.stash:

            def run_for_repo_and_each_submodule_rec(args: List[str]):
                Git.run(repo_path, args, echo=verbose, prefix=prefix)
                Git.run(
                    repo_path,
                    ["submodule", "foreach", "--recursive", "git"] + args,
                    echo=verbose,
                    prefix=prefix,
                )

            if pool_args.clean:
                # Stash tracked and untracked changes.
                run_for_repo_and_each_submodule_rec(["stash", "-u"])
            elif pool_args.stash:
                # Delete tracked changes.
                run_for_repo_and_each_submodule_rec(["reset", "--hard", "HEAD"])

            # Delete untracked changes and ignored files.
            run_for_repo_and_each_submodule_rec(["clean", "-fdx"])
            del run_for_repo_and_each_submodule_rec

            # It is possible to reset --hard and still be mid-rebase.
            try:
                Git.run(repo_path, ["rebase", "--abort"], echo=verbose, prefix=prefix)
            except Exception:
                pass

        if checkout_target:
            Git.run(repo_path, ["status", "--porcelain", "-uno"])

            # Some of the projects switch branches/tags when they
            # are updated. Local checkout might not have that tag/branch
            # fetched yet, so let's attempt to fetch before attempting
            # checkout.
            try:
                Git.run(
                    repo_path, ["rev-parse", "--verify", checkout_target], echo=verbose
                )
            except Exception:
                Git.run(
                    repo_path,
                    ["fetch", "--recurse-submodules=yes", "--tags"],
                    echo=verbose,
                    prefix=prefix,
                )

            try:
                Git.run(
                    repo_path,
                    ["checkout", checkout_target],
                    echo=verbose,
                    prefix=prefix,
                )
            except Exception:
                try:
                    revision, _, _ = Git.run(repo_path, ["rev-parse", checkout_target])
                    Git.run(
                        repo_path, ["checkout", revision], echo=verbose, prefix=prefix
                    )
                except Exception:
                    raise

        # It's important that we checkout, fetch, and rebase, in order.
        # .git/FETCH_HEAD updates the not-for-merge attributes based on
        # which branch was checked out during the fetch.
        Git.run(
            repo_path,
            ["fetch", "--recurse-submodules=yes", "--tags"],
            echo=verbose,
            prefix=prefix,
        )

        # If we were asked to reset to the specified branch, do the hard
        # reset and return.
        if checkout_target and pool_args.reset_to_remote and not cross_repo:
            full_target = full_target_name(repo_path, "origin", checkout_target)
            Git.run(
                repo_path, ["reset", "--hard", full_target], echo=verbose, prefix=prefix
            )
            return

        # Query whether we have a "detached HEAD", which will mean that
        # we previously checked out a tag rather than a branch.
        detached_head = False
        try:
            # This git command returns error code 1 if HEAD is detached.
            # Otherwise there was some other error, and we need to handle
            # it like other command errors.
            Git.run(repo_path, ["symbolic-ref", "-q", "HEAD"])
        except GitException as e:
            if e.returncode == 1:
                detached_head = True
            else:
                raise  # Pass this error up the chain.

        # If we have a detached HEAD in this repository, we don't want
        # to rebase. With a detached HEAD, the fetch will have marked
        # all the branches in FETCH_HEAD as not-for-merge, and the
        # "git rebase FETCH_HEAD" will try to rebase the tree from the
        # default branch's current head, making a mess.

        # Prior to Git 2.6, this is the way to do a "git pull
        # --rebase" that respects rebase.autostash.  See
        # http://stackoverflow.com/a/30209750/125349
        if not cross_repo and not detached_head:
            Git.run(repo_path, ["rebase", "FETCH_HEAD"], echo=verbose, prefix=prefix)
        elif detached_head and verbose:
            print(
                prefix + "Detached HEAD; probably checked out a tag. No need "
                "to rebase."
            )

        Git.run(
            repo_path,
            ["submodule", "update", "--recursive"],
            echo=verbose,
            prefix=prefix,
        )
    except Exception:
        if verbose:
            print('Error on repo "%s": %s' % (repo_path, traceback.format_exc()))
        raise


def get_timestamp_to_match(match_timestamp: bool, source_root: Path):
    """Computes a timestamp of the last commit on the current branch in
    the `swift` repository.

    Args:
        match_timestamp (bool): value of `--match-timestamp` to check.
        source_root (Path): directory that contains sources of the Swift project.

    Returns:
        str | None: a timestamp of the last commit of `swift` repository if
        `match_timestamp` argument has a value, `None` if `match_timestamp` is
        falsy.
    """
    if not match_timestamp:
        return None
    swift_repo_path = source_root.joinpath("swift")
    output, _, _ = Git.run(swift_repo_path, ["log", "-1", "--format=%cI"], fatal=True)
    return output


def get_scheme_map(
    config: Dict[str, Any], scheme_name: str
) -> Optional[Dict[str, str]]:
    """Find a mapping from repository IDs to branches in the config.

    Args:
        config (Dict[str, Any]): deserialized `update-checkout-config.json`
        scheme_name (str): name of the scheme to look up in `config`

    Returns:
        Dict[str, str]: a mapping from repos to branches for the given scheme.
    """

    if scheme_name:
        # This loop is only correct, since we know that each alias set has
        # unique contents. This is checked by validate_config. Thus the first
        # branch scheme data that has scheme_name as one of its aliases is
        # the only possible correct answer.
        for v in config["branch-schemes"].values():
            if scheme_name in v["aliases"]:
                return v["repos"]

    return None


def _check_missing_clones(
    args: CliArguments, config: Dict[str, Any], scheme_map: Dict[str, Any]
):
    """
    Verify that all repositories defined in the scheme map are present in the
    source root directory. If a repository is missing—and not explicitly skipped—
    the user is prompted to re-run the script with the `--clone` option.

    This function also respects per-repository platform restrictions: if the
    current platform is not listed for a repo, that repo is ignored.

    Args:
        args (CliArguments): Parsed CLI arguments.
        config (Dict[str, Any]): deserialized `update-checkout-config.json`.
        scheme_map (Dict[str, str] | None): map of repo names to branches to check out.
    """

    for repo in scheme_map:
        if _should_skip_repo(args, config, repo):
            continue

        if not args.source_root.joinpath(repo).exists():
            print(
                "You don't have all swift sources. "
                "Call this script with --clone to get them."
            )
            return


def _check_git_config(
    args: CliArguments, config: Dict[str, Any], scheme_map: Dict[str, Any]
):
    """
    Verify git configuration for all cloned repositories.
    Warns if core.symlinks or core.autocrlf are not set correctly.

    Args:
        args (CliArguments): Parsed CLI arguments.
        config (Dict[str, Any]): deserialized `update-checkout-config.json`.
        scheme_map (Dict[str, str] | None): map of repo names to branches to check out.
    """

    git_configs = {
        "core.symlinks": "true",
        "core.autocrlf": "false",
    }

    for repo in scheme_map:
        if _should_skip_repo(args, config, repo):
            continue

        repo_path = args.source_root.joinpath(repo)
        if not repo_path.exists():
            continue

        for config_key, expected_value in git_configs.items():
            try:
                output = subprocess.check_output(
                    ["git", "-C", str(repo_path), "config", "--get", config_key],
                    text=True,
                    stderr=subprocess.DEVNULL,
                )
                if expected_value not in output:
                    print(
                        f"[WARNING] '{repo}' was not cloned with "
                        f"'{config_key}={expected_value}'. "
                        "This can cause build/tests failures."
                    )
            except subprocess.CalledProcessError:
                pass


def _should_skip_repo(args: CliArguments, config: Dict[str, Any], repo: str) -> bool:
    """Check if a repository should be skipped based on platform or skip list."""
    if repo in args.skip_repository_list:
        return True

    repo_config = config["repos"].get(repo, {})
    if "platforms" in repo_config:
        current_platform = platform.system()
        if current_platform not in repo_config["platforms"]:
            return True

    return False


def _move_llvm_project_to_first_index(
    pool_args: Union[List[UpdateArguments], List[AdditionalSwiftSourcesArguments]],
):
    llvm_project_idx = None
    for i in range(len(pool_args)):
        if pool_args[i].repo_name == "llvm-project":
            llvm_project_idx = i
            break
    if llvm_project_idx is not None:
        pool_args.insert(0, pool_args.pop(llvm_project_idx))


def update_all_repositories(
    args: CliArguments,
    config: Dict[str, Any],
    scheme_name: str,
    scheme_map: Optional[Dict[str, Any]],
    cross_repos_pr: Dict[str, str],
) -> Tuple[List[SkippedReason], List[Union[Exception, None]]]:
    skipped_repositories = []
    pool_args: List[UpdateArguments] = []
    timestamp = get_timestamp_to_match(args.match_timestamp, args.source_root)
    for repo_name in config["repos"].keys():
        if repo_name in args.skip_repository_list:
            skipped_repositories.append(
                SkippedReason(
                    repo_name,
                    "requested by user",
                )
            )
            continue

        # If the repository is not listed in the branch-scheme, skip it.
        if scheme_map and repo_name not in scheme_map:
            # If the repository exists locally, notify we are skipping it.
            if args.source_root.joinpath(repo_name).is_dir():
                skipped_repositories.append(
                    SkippedReason(
                        repo_name,
                        f"repository not listed in the {scheme_name} branch-scheme",
                    )
                )
            continue

        my_args = UpdateArguments(
            source_root=args.source_root,
            config=config,
            repo_name=repo_name,
            scheme_name=scheme_name,
            scheme_map=scheme_map,
            tag=args.tag,
            timestamp=timestamp,
            reset_to_remote=args.reset_to_remote,
            clean=args.clean,
            stash=args.stash,
            cross_repos_pr=cross_repos_pr,
            output_prefix="Updating",
            verbose=args.verbose,
        )
        pool_args.append(my_args)

    locked_repositories = is_any_repository_locked(pool_args)
    if len(locked_repositories) > 0:
        return skipped_repositories, [
            Exception(f"'{repo_name}' is locked by git. Cannot update it.")
            for repo_name in locked_repositories
        ]
    _move_llvm_project_to_first_index(pool_args)
    return (
        skipped_repositories,
        ParallelRunner(update_single_repository, pool_args, args.n_processes).run(),
    )


def obtain_additional_swift_sources(pool_args: AdditionalSwiftSourcesArguments):
    args = pool_args.args
    repo_name = pool_args.repo_name
    repo_branch = pool_args.repo_branch
    verbose = pool_args.verbose
    skip_tags = args.skip_tags
    remote = pool_args.remote

    env = dict(os.environ)
    env.update({"GIT_TERMINAL_PROMPT": "0"})

    if verbose:
        print("Cloning '" + pool_args.repo_name + "'")

    if args.skip_history:
        if is_commit_hash(repo_branch):
            Git.run(
                args.source_root,
                [
                    "clone",
                    "--config",
                    "core.symlinks=true",
                    "--config",
                    "core.autocrlf=false",
                    "--depth",
                    "1",
                    remote,
                    repo_name,
                ]
                + (["--no-tags"] if skip_tags else []),
                env=env,
                echo=verbose,
            )
            repo_path = args.source_root.joinpath(repo_name)
            Git.run(
                repo_path,
                ["fetch", "--depth", "1", "origin", repo_branch],
                env=env,
                echo=verbose,
            )
            Git.run(repo_path, ["checkout", repo_branch], env=env, echo=verbose)
        else:
            Git.run(
                args.source_root,
                [
                    "clone",
                    "--config",
                    "core.symlinks=true",
                    "--config",
                    "core.autocrlf=false",
                    "--recursive",
                    "--depth",
                    "1",
                    "--branch",
                    repo_branch,
                    remote,
                    repo_name,
                ]
                + (["--no-tags"] if skip_tags else []),
                env=env,
                echo=verbose,
            )
    elif args.use_submodules:
        Git.run(
            args.source_root,
            ["submodule", "add", remote, repo_name]
            + (["--no-tags"] if skip_tags else []),
            env=env,
            echo=verbose,
        )
    else:
        Git.run(
            args.source_root,
            [
                "clone",
                "--config",
                "core.symlinks=true",
                "--config",
                "core.autocrlf=false",
                "--recursive",
                remote,
                repo_name,
            ]
            + (["--no-tags"] if skip_tags else []),
            env=env,
            echo=verbose,
        )

    repo_path = args.source_root.joinpath(repo_name)
    if pool_args.scheme_name:
        src_path = repo_path.joinpath(".git")
        Git.run(
            args.source_root,
            [
                "--git-dir",
                str(src_path),
                "--work-tree",
                str(repo_path),
                "checkout",
                repo_branch,
            ],
            env=env,
        )
    Git.run(repo_path, ["submodule", "update", "--recursive"], env=env)


def obtain_all_additional_swift_sources(
    args: CliArguments,
    config: Dict[str, Any],
    scheme_name: str,
    skip_repository_list: List[str],
):
    skipped_repositories = []
    pool_args = []
    for repo_name, repo_info in config["repos"].items():
        repo_path = args.source_root.joinpath(repo_name)
        if repo_name in skip_repository_list:
            skipped_repositories.append(SkippedReason(repo_name, "requested by user"))
            continue

        if args.use_submodules:
            repo_exists = False
            submodules_status, _, _ = Git.run(
                repo_path, ["submodule", "status"], fatal=True
            )
            if submodules_status:
                for line in submodules_status.splitlines():
                    if line[0].endswith(repo_name):
                        repo_exists = True
                        break

        else:
            repo_exists = repo_path.joinpath(".git").is_dir()

        if repo_exists:
            skipped_repositories.append(
                SkippedReason(repo_name, "directory already exists")
            )
            continue

        # If we have a url override, use that url instead of
        # interpolating.
        remote_repo_info = repo_info["remote"]
        if "url" in remote_repo_info:
            remote = remote_repo_info["url"]
        else:
            remote_repo_id = remote_repo_info["id"]
            if args.clone_with_ssh is True or "https-clone-pattern" not in config:
                remote = config["ssh-clone-pattern"] % remote_repo_id
            else:
                remote = config["https-clone-pattern"] % remote_repo_id

        repo_branch: Optional[str] = None
        repo_not_in_scheme = False
        if scheme_name:
            for v in config["branch-schemes"].values():
                if scheme_name not in v["aliases"]:
                    continue
                # If repo is not specified in the scheme, skip cloning it.
                if repo_name not in v["repos"]:
                    repo_not_in_scheme = True
                    continue
                repo_branch = v["repos"][repo_name]
                break
            else:
                repo_branch = scheme_name
        if repo_not_in_scheme:
            continue

        if repo_branch is None:
            raise RuntimeError("repo_branch is None")

        new_args = AdditionalSwiftSourcesArguments(
            args=args,
            source_root=args.source_root,
            repo_name=repo_name,
            repo_info=repo_info,
            repo_branch=repo_branch,
            remote=remote,
            scheme_name=scheme_name,
            skip_repository_list=skip_repository_list,
            output_prefix="Cloning",
            verbose=args.verbose,
        )

        if args.use_submodules:
            obtain_additional_swift_sources(new_args)
        else:
            pool_args.append(new_args)

    # Only use `ParallelRunner` when submodules are not used, since `.git` dir
    # can't be accessed concurrently.
    if args.use_submodules:
        return [], None
    if not pool_args:
        print("Not cloning any repositories.")
        return [], None

    _move_llvm_project_to_first_index(pool_args)
    return (
        skipped_repositories,
        ParallelRunner(
            obtain_additional_swift_sources, pool_args, args.n_processes
        ).run(),
    )


def dump_repo_hashes(
    args: CliArguments, config: Dict[str, Any], branch_scheme_name: str = "repro"
):
    """
    Dumps the current state of the repo into a new config file that contains a
    main branch scheme with the relevant branches set to the appropriate
    hashes.
    """
    new_config = {}
    config_copy_keys = ["ssh-clone-pattern", "https-clone-pattern", "repos"]
    for config_copy_key in config_copy_keys:
        new_config[config_copy_key] = config[config_copy_key]
    repos = {}
    repos = repo_hashes(args, config)
    branch_scheme = {"aliases": [branch_scheme_name], "repos": repos}
    new_config["branch-schemes"] = {branch_scheme_name: branch_scheme}
    json.dump(new_config, sys.stdout, indent=4)


def repo_hashes(args: CliArguments, config: Dict[str, Any]) -> Dict[str, str]:
    repos = {}
    for repo_name, _ in sorted(config["repos"].items(), key=lambda x: x[0]):
        repo_path = args.source_root.joinpath(repo_name)
        if repo_path.exists():
            h, _, _ = Git.run(repo_path, ["rev-parse", "HEAD"], fatal=True)
        else:
            h = "skip"
        repos[repo_name] = str(h)
    return repos


def print_repo_hashes(args: CliArguments, config: Dict[str, Any]):
    repos = repo_hashes(args, config)
    max_length = max(len(name) for name in repos.keys())
    for repo_name, repo_hash in sorted(repos.items(), key=lambda x: x[0]):
        print(f"{repo_name:<{max_length + 1}}: {repo_hash}")


def merge_no_duplicates(
    a: Dict[Hashable, Any], b: Dict[Hashable, Any]
) -> Dict[Hashable, Any]:
    result = {**a}
    for key, value in b.items():
        if key in a:
            raise ValueError(f"Duplicate scheme {key}")

        result[key] = value
    return result


def merge_config(config: Dict[str, Any], new_config: Dict[str, Any]) -> Dict[str, Any]:
    """
    Merge two configs, with a 'last-wins' strategy.

    The branch-schemes are rejected if they define duplicate schemes.
    """

    result = {**config}
    for key, value in new_config.items():
        if key == "branch-schemes":
            # We reject duplicates here since this is the most conservative
            # behavior, so it can be relaxed in the future.
            # TODO: Another semantics might be nicer, define that as it is needed.
            result[key] = merge_no_duplicates(config.get(key, {}), value)
        elif key == "repos":
            # The "repos" object is last-wins on a key-by-key basis
            result[key] = {**config.get(key, {}), **value}
        else:
            # Anything else is just last-wins
            result[key] = value

    return result


def validate_config(config: Dict[str, Any]):
    # Make sure that our branch-names are unique.
    scheme_names = config["branch-schemes"].keys()
    if len(scheme_names) != len(set(scheme_names)):
        raise RuntimeError("Configuration file has duplicate schemes?!")

    # Ensure the branch-scheme name is also an alias
    # This guarantees sensible behavior of update_repository_to_scheme when
    # the branch-scheme is passed as the scheme name
    for scheme_name in config["branch-schemes"].keys():
        if scheme_name not in config["branch-schemes"][scheme_name]["aliases"]:
            raise RuntimeError(
                'branch-scheme name: "{0}" must be an alias ' "too.".format(scheme_name)
            )

    # Then make sure the alias names used by our branches are unique.
    seen: Dict[str, Any] = dict()
    for scheme_name, scheme in config["branch-schemes"].items():
        aliases = scheme["aliases"]
        for alias in aliases:
            if alias in seen:
                raise RuntimeError(
                    "Configuration file defines the alias {0} "
                    "in both the {1} scheme and the {2} scheme?!".format(
                        alias, seen[alias], scheme_name
                    )
                )
            else:
                seen[alias] = scheme_name


def full_target_name(repo_path: Path, remote: str, target: str) -> str:
    branch, _, _ = Git.run(repo_path, ["branch", "--list", target], fatal=True)
    branch = branch.replace("* ", "")
    if branch == target:
        name = "%s/%s" % (remote, target)
        return name

    # This is either a tag or commit hash -- we can use it as is
    return target


def skip_list_for_platform(config: Dict[str, Any], all_repos: bool) -> List[str]:
    """Computes a list of repositories to skip when updating or cloning, if not
    overridden by `--all-repositories` CLI argument.

    Args:
        config (Dict[str, Any]): deserialized `update-checkout-config.json`
        all_repos (bool): include all repositories.

    Returns:
        List[str]: a resulting list of repositories to skip or empty list if
        `all_repos` is not empty.
    """

    if all_repos:
        return []  # Do not skip any platform-specific repositories

    # If there is a platforms key only include the repo if the
    # platform is in the list
    skip_list = []
    platform_name = platform.system()

    for repo_name, repo_info in config["repos"].items():
        if "platforms" in repo_info:
            if platform_name not in repo_info["platforms"]:
                print("Skipping", repo_name, "on", platform_name)
                skip_list.append(repo_name)
            else:
                print("Including", repo_name, "on", platform_name)

    return skip_list


def main() -> int:
    args = CliArguments.parse_args()

    if args.command == "status":
        return status.StatusCommand(args).run()

    if not args.scheme:
        if args.reset_to_remote:
            print(
                "update-checkout usage error: --reset-to-remote must "
                "specify --scheme=foo"
            )
            sys.exit(1)
        if args.match_timestamp:
            # without a scheme, we won't be able match timestamps forward in
            # time, which is an annoying footgun for bisection etc.
            print(
                "update-checkout usage error: --match-timestamp must "
                "specify --scheme=foo"
            )
            sys.exit(1)

    # Set the default config path if none are specified
    if not args.configs:
        default_path = SCRIPT_DIR.parent.joinpath("update-checkout-config.json")
        args.configs.append(str(default_path))
    config: Dict[str, Any] = {}
    for config_path in args.configs:
        with open(config_path) as f:
            config = merge_config(config, json.load(f))
    validate_config(config)

    cross_repos_pr: Dict[str, str] = {}
    if args.github_comment:
        regex_pr = (
            r"(apple/[-a-zA-Z0-9_]+/pull/\d+"
            r"|apple/[-a-zA-Z0-9_]+#\d+"
            r"|swiftlang/[-a-zA-Z0-9_]+/pull/\d+"
            r"|swiftlang/[-a-zA-Z0-9_]+#\d+)"
        )
        repos_with_pr = re.findall(regex_pr, args.github_comment)
        print("Found related pull requests:", str(repos_with_pr))
        repos_with_pr = [pr.replace("/pull/", "#") for pr in repos_with_pr]
        cross_repos_pr = dict(pr.split("#") for pr in repos_with_pr)

    # If branch is None, default to using the default branch alias
    # specified by our configuration file.
    scheme_name = args.scheme
    if scheme_name is None:
        scheme_name = config["default-branch-scheme"]

    scheme_map = get_scheme_map(config, scheme_name)

    @exponential_retry(max_retries=args.max_retries)
    def do_checkout() -> int:
        nonlocal config, scheme_map
        clone_results = None
        skip_repo_list = []
        if args.clone or args.clone_with_ssh:
            skip_repo_list = skip_list_for_platform(config, args.all_repositories)
            skip_repo_list.extend(args.skip_repository_list)
            skipped_repositories, clone_results = obtain_all_additional_swift_sources(
                args, config, scheme_name, skip_repo_list
            )
            _check_git_config(args, config, scheme_map)

            SkippedReason.print_skipped_repositories(skipped_repositories, "clone")

        swift_repo_path = args.source_root.joinpath("swift")
        if "swift" not in skip_repo_list and swift_repo_path.exists():
            # Check if `swift` repo itself needs to switch to a cross-repo branch.
            branch_name, cross_repo = get_branch_for_repo(
                swift_repo_path,
                config,
                "swift",
                scheme_name,
                scheme_map,
                cross_repos_pr,
            )

            if cross_repo:
                Git.run(
                    swift_repo_path,
                    ["checkout", branch_name],
                    echo=True,
                    prefix="[swift] ",
                )

                # Re-read the config after checkout.
                config = {}
                for config_path in args.configs:
                    with open(config_path) as f:
                        config = merge_config(config, json.load(f))
                validate_config(config)
                scheme_map = get_scheme_map(config, scheme_name)

        if args.dump_hashes:
            dump_repo_hashes(args, config)
            sys.exit(0)

        if args.dump_hashes_config:
            dump_repo_hashes(args, config, args.dump_hashes_config)
            sys.exit(0)

        _check_missing_clones(args=args, config=config, scheme_map=scheme_map)

        skipped_repositories, update_results = update_all_repositories(
            args, config, scheme_name, scheme_map, cross_repos_pr
        )
        SkippedReason.print_skipped_repositories(skipped_repositories, "update")

        fail_count = 0
        fail_count += ParallelRunner.check_results(clone_results, "CLONE")
        fail_count += ParallelRunner.check_results(update_results, "UPDATE")
        return fail_count

    fail_count = do_checkout()
    if fail_count > 0:
        print("update-checkout failed, fix errors and try again")
    else:
        print("update-checkout succeeded")
        print_repo_hashes(args, config)
    sys.exit(fail_count)
