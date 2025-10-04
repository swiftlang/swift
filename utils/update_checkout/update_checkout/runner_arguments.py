from dataclasses import dataclass
from typing import Any, Dict

@dataclass
class RunnerArguments:
    repo_name: str
    scheme_name: str
    output_prefix: str
    verbose: bool

@dataclass
class UpdateArguments(RunnerArguments):
    source_root: str
    config: Dict[str, Any]
    scheme_map: Any
    tag: str
    timestamp: Any
    reset_to_remote: bool
    clean: bool
    stash: bool
    cross_repos_pr: bool

@dataclass
class AdditionalSwiftSourcesArguments(RunnerArguments):
    args: RunnerArguments
    repo_info: str
    repo_branch: str
    remote: str
    with_ssh: bool
    skip_history: bool
    skip_tags: bool
    skip_repository_list: bool
    use_submodules: bool
