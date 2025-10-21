from multiprocessing.managers import ListProxy, ValueProxy
import sys
from multiprocessing import cpu_count, Manager
import time
from typing import Callable, List, Any, Union
from threading import Lock, Thread, Event
from concurrent.futures import ThreadPoolExecutor
import shutil

from .runner_arguments import RunnerArguments, AdditionalSwiftSourcesArguments


class MonitoredFunction:
    def __init__(
        self,
        fn: Callable,
        running_tasks: ListProxy,
        updated_repos: ValueProxy,
        lock: Lock,
    ):
        self.fn = fn
        self.running_tasks = running_tasks
        self.updated_repos = updated_repos
        self._lock = lock

    def __call__(self, *args: Union[RunnerArguments, AdditionalSwiftSourcesArguments]):
        task_name = args[0].repo_name
        self.running_tasks.append(task_name)
        result = None
        try:
            result = self.fn(*args)
        except Exception as e:
            print(e)
        finally:
            self._lock.acquire()
            if task_name in self.running_tasks:
                self.running_tasks.remove(task_name)
            self.updated_repos.set(self.updated_repos.get() + 1)
            self._lock.release()
            return result


class ParallelRunner:
    def __init__(
        self,
        fn: Callable,
        pool_args: List[Union[RunnerArguments, AdditionalSwiftSourcesArguments]],
        n_threads: int = 0,
    ):
        if n_threads == 0:
            # Limit the number of threads as the performance regresses if the
            # number is too high.
            n_threads = min(cpu_count() * 2, 16)
        self._n_threads = n_threads
        self._monitor_polling_period = 0.1
        self._terminal_width = shutil.get_terminal_size().columns
        self._pool_args = pool_args
        self._fn = fn
        self._output_prefix = pool_args[0].output_prefix
        self._nb_repos = len(pool_args)
        self._stop_event = Event()
        self._verbose = pool_args[0].verbose
        if not self._verbose:
            manager = Manager()
            self._lock = manager.Lock()
            self._running_tasks = manager.list()
            self._updated_repos = manager.Value("i", 0)
            self._monitored_fn = MonitoredFunction(
                self._fn, self._running_tasks, self._updated_repos, self._lock
            )

    def run(self) -> List[Any]:
        print(f"Running ``{self._fn.__name__}`` with up to {self._n_threads} processes.")
        if self._verbose:
            with ThreadPoolExecutor(max_workers=self._n_threads) as pool:
                results = list(pool.map(self._fn, self._pool_args, timeout=1800))
        else:
            monitor_thread = Thread(target=self._monitor, daemon=True)
            monitor_thread.start()
            with ThreadPoolExecutor(max_workers=self._n_threads) as pool:
                results = list(pool.map(self._monitored_fn, self._pool_args, timeout=1800))
            self._stop_event.set()
            monitor_thread.join()
        return results

    def _monitor(self):
        last_output = ""
        while not self._stop_event.is_set():
            self._lock.acquire()
            current = list(self._running_tasks)
            current_line = ", ".join(current)
            updated_repos = self._updated_repos.get()
            self._lock.release()

            if current_line != last_output:
                truncated = f"{self._output_prefix} [{updated_repos}/{self._nb_repos}] ({current_line})"
                if len(truncated) > self._terminal_width:
                    ellipsis_marker = " ..."
                    truncated = (
                        truncated[: self._terminal_width - len(ellipsis_marker)]
                        + ellipsis_marker
                    )
                sys.stdout.write("\r" + truncated.ljust(self._terminal_width))
                sys.stdout.flush()
                last_output = current_line

            time.sleep(self._monitor_polling_period)

        sys.stdout.write("\r" + " " * len(last_output) + "\r\n")
        sys.stdout.flush()

    @staticmethod
    def check_results(results, op) -> int:
        """Function used to check the results of ParallelRunner.

        NOTE: This function was originally located in the shell module of
        swift_build_support and should eventually be replaced with a better
        parallel implementation.
        """

        fail_count = 0
        if results is None:
            return 0
        for r in results:
            if r is None:
                continue
            if fail_count == 0:
                print("======%s FAILURES======" % op)
            fail_count += 1
            if isinstance(r, str):
                print(r)
                continue
            if not hasattr(r, "repo_path"):
                # TODO: create a proper Exception class with these attributes
                continue
            print("%s failed (ret=%d): %s" % (r.repo_path, r.ret, r))
            if r.stderr:
                print(r.stderr.decode())
        return fail_count
