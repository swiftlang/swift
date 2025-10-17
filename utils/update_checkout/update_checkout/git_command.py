from typing import List, Any, Optional, Union

from swift_build_support.swift_build_support import shell


class Git:
    @staticmethod
    def run(repo_path: Optional[str], args: List[str], **kwargs):
        cmd = ["git"]
        if repo_path is not None:
            cmd += ["-C", repo_path]
        kwargs["repo_path"] = repo_path
        # FIXME: The way we are passing args below is broken. shell.run takes 
        # *args as list arguments and sometimes treats them as one positional 
        # argument or as a list of arguments.
        return shell.run(cmd+args, **kwargs)

    @staticmethod
    def capture(
        repo_path: str,
        args: List[str],
        stderr=None,
        env=None,
        dry_run=None,
        echo=True,
        optional=False,
        allow_non_zero_exit=False,
    ) -> Union[str, Any, None]:
        return shell.capture(
            ["git", "-C", repo_path] + args,
            stderr=stderr,
            env=env,
            dry_run=dry_run,
            echo=echo,
            optional=optional,
            allow_non_zero_exit=allow_non_zero_exit,
        )
