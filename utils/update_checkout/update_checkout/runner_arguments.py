from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Optional

from .cli_arguments import CliArguments


@dataclass
class RunnerArguments:
    repo_name: str
    output_prefix: str
    source_root: Path
    verbose: bool


@dataclass
class UpdateArguments(RunnerArguments):
    scheme_name: str
    config: Dict[str, Any]
    scheme_map: Any
    tag: Optional[str]
    timestamp: Any
    reset_to_remote: bool
    clean: bool
    stash: bool
    cross_repos_pr: Dict[str, str]


@dataclass
class AdditionalSwiftSourcesArguments(RunnerArguments):
    scheme_name: str
    args: CliArguments
    repo_info: str
    repo_branch: str
    remote: str
    skip_repository_list: List[str]
