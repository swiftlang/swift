from dataclasses import dataclass
from typing import Any, Dict, List

from .cli_arguments import CliArguments


@dataclass
class RunnerArguments:
    args: CliArguments
    scheme_name: str
    repo_name: str
    output_prefix: str


@dataclass
class UpdateArguments(RunnerArguments):
    config: Dict[str, Any]
    scheme_map: Any
    timestamp: Any
    cross_repos_pr: Dict[str, str]


@dataclass
class AdditionalSwiftSourcesArguments(RunnerArguments):
    repo_info: str
    repo_branch: str
    remote: str
    skip_repository_list: List[str]
