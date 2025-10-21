import sys
from multiprocessing import cpu_count
import time
from typing import Callable, List, Any, Tuple, Union
from threading import Lock, Thread, Event
from concurrent.futures import ThreadPoolExecutor
import shutil

from .runner_arguments import RunnerArguments, AdditionalSwiftSourcesArguments


class TaskTracker:
    _running_tasks: List[str]
    _done_task_counter: int
    _lock: Lock

    def __init__(self):
        self._running_tasks = []
        self._done_task_counter = 0
        self._lock = Lock()

    def mark_task_as_running(self, task_name: str):
        self._lock.acquire()
        self._running_tasks.append(task_name)
        self._lock.release()

    def mark_task_as_done(self, task_name: str):
        self._lock.acquire()
        if task_name in self._running_tasks:
            self._running_tasks.remove(task_name)
        self._done_task_counter += 1
        self._lock.release()

    def status(self) -> Tuple[List[str], int]:
        self._lock.acquire()
        running_tasks_str = ", ".join(self.running_tasks)
        done_tasks = self.done_task_counter
        self._lock.release()
        return running_tasks_str, done_tasks

    @property
    def running_tasks(self) -> List[str]:
        return self._running_tasks

    @property
    def done_task_counter(self) -> int:
        return self._done_task_counter


class MonitoredFunction:
    def __init__(
        self,
        fn: Callable,
        task_tracker: TaskTracker,
    ):
        self.fn = fn
        self._task_tracker = task_tracker

    def __call__(self, *args: Union[RunnerArguments, AdditionalSwiftSourcesArguments]):
        task_name = args[0].repo_name
        self._task_tracker.mark_task_as_running(task_name)
        result = None
        try:
            result = self.fn(*args)
        except Exception as e:
            print(e)
        finally:
            self._task_tracker.mark_task_as_done(task_name)
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
            self._task_tracker = TaskTracker()
            self._monitored_fn = MonitoredFunction(self._fn, self._task_tracker)

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
            current_line, updated_repos = self._task_tracker.status()
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

        sys.stdout.write("\r" + " " * self._terminal_width + "\r")
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
