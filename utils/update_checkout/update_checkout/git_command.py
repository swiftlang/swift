import shlex
import subprocess
import sys
from typing import List, Any, Optional, Dict


class Git:
    @staticmethod
    def run(
        repo_path: str,
        args: List[str],
        echo: bool = False,
        env: Optional[Dict[str, Any]] = None,
        prefix: str = "",
        allow_non_zero_exit: bool = False,
        fatal: bool = False,
        **kwargs,
    ):
        command = Git._build_command(args)

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
                    f"status {str(e.returncode)}, aborting")
            eout = Exception(
                f"[{repo_path}] '{Git._quote_command(command)}' failed with '{output}'"
            )
            eout.ret = e.returncode
            eout.arguments = command
            eout.repo_path = repo_path
            eout.stderr = output
            raise eout
        except OSError as e:
            if fatal:
                sys.exit(
                    f"could not execute '{Git._quote_command(command)}': "
                    f"{e.strerror}"
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
                print(prefix+line)
        sys.stdout.flush()
        sys.stderr.flush()

    @staticmethod
    def _build_command(args: List[str]) -> List[str]:
        return ["git"] + args

    @staticmethod
    def _quote(arg: Any) -> str:
        return shlex.quote(str(arg))

    @staticmethod
    def _quote_command(command: Any) -> str:
        return " ".join(Git._quote(arg) for arg in command)
