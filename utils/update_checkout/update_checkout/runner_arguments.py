from dataclasses import dataclass
from typing import Any, Dict, List

from .cli_arguments import CliArguments

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
    args: CliArguments
    repo_info: str
    repo_branch: str
    remote: str
    skip_repository_list: List[str]
