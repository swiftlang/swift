import argparse
from typing import List, Optional

from build_swift.build_swift.constants import SWIFT_SOURCE_ROOT


class CliArguments(argparse.Namespace):
    clone: bool
    clone_with_ssh: bool
    skip_history: bool
    skip_tags: bool
    skip_repository_list: List[str]
    all_repositories: bool
    scheme: Optional[str]
    reset_to_remote: bool
    clean: bool
    stash: bool
    configs: List[str]
    github_comment: Optional[str]
    dump_hashes: bool
    dump_hashes_config: Optional[str]
    tag: Optional[str]
    match_timestamp: bool
    n_processes: int
    source_root: str
    use_submodules: bool
    verbose: bool

    @staticmethod
    def parse_args() -> "CliArguments":
        parser = argparse.ArgumentParser(
            formatter_class=argparse.RawDescriptionHelpFormatter,
            description="""
By default, updates your checkouts of Swift, SourceKit, LLDB, and SwiftPM
repositories.
    """,
        )
        parser.add_argument(
            "--clone",
            help="obtain sources for Swift and related projects",
            action="store_true",
        )
        parser.add_argument(
            "--clone-with-ssh",
            help="Obtain sources for Swift and related projects via SSH",
            action="store_true",
        )
        parser.add_argument(
            "--skip-history",
            help="Skip histories when obtaining sources",
            action="store_true",
        )
        parser.add_argument(
            "--skip-tags", help="Skip tags when obtaining sources", action="store_true"
        )
        parser.add_argument(
            "--skip-repository",
            metavar="DIRECTORY",
            default=[],
            help="Skip the specified repository",
            dest="skip_repository_list",
            action="append",
        )
        parser.add_argument(
            "--all-repositories",
            help="""Includes repositories not required for current platform.
            This will not override '--skip-repositories'""",
            action="store_true",
        )
        parser.add_argument(
            "--scheme",
            help='Use branches from the specified branch-scheme. A "branch-scheme"'
            " is a list of (repo, branch) pairs.",
            metavar="BRANCH-SCHEME",
        )
        parser.add_argument(
            "--reset-to-remote",
            help="Reset each branch to the remote state.",
            action="store_true",
        )
        parser.add_argument(
            "--clean",
            help="""Delete tracked and untracked changes, ignored files, and abort
            an ongoing rebase, if any, before updating a repository.""",
            action="store_true",
        )
        parser.add_argument(
            "--stash",
            help="""Stash tracked and untracked changes, delete ignored files, and
            abort an ongoing rebase, if any, before updating a repository.""",
            action="store_true",
        )
        parser.add_argument(
            "--config",
            help="""The configuration file to use. Can be specified multiple times,
            each config will be merged together with a 'last-wins' strategy.
            Overwriting branch-schemes is not allowed.""",
            action="append",
            default=[],
            dest="configs",
        )
        parser.add_argument(
            "--github-comment",
            help="""Check out related pull requests referenced in the given
            free-form GitHub-style comment.""",
            metavar="GITHUB-COMMENT",
        )
        parser.add_argument(
            "--dump-hashes",
            action="store_true",
            help="Dump the git hashes of all repositories being tracked",
        )
        parser.add_argument(
            "--dump-hashes-config",
            help="Dump the git hashes of all repositories packaged into "
            "update-checkout-config.json",
            metavar="BRANCH-SCHEME-NAME",
        )
        parser.add_argument(
            "--tag",
            help="""Check out each repository to the specified tag.""",
            metavar="TAG-NAME",
        )
        parser.add_argument(
            "--match-timestamp",
            help="Check out adjacent repositories to match timestamp of "
            " current swift checkout.",
            action="store_true",
        )
        parser.add_argument(
            "-j",
            "--jobs",
            type=int,
            help="Number of threads to run at once",
            default=0,
            dest="n_processes",
        )
        parser.add_argument(
            "--source-root",
            help="The root directory to checkout repositories",
            default=SWIFT_SOURCE_ROOT,
        )
        parser.add_argument(
            "--use-submodules",
            help="Checkout repositories as git submodules.",
            action="store_true",
        )
        parser.add_argument(
            "-v",
            "--verbose",
            help="Increases the script's verbosity.",
            action="store_true",
        )
        return parser.parse_args()
