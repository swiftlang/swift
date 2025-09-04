from multiprocessing.managers import ListProxy, ValueProxy
import sys
from multiprocessing import Pool, cpu_count, Manager
import time
from typing import Callable, List, Any
from threading import Thread, Event, Lock
import shutil

class MonitoredFunction:
    def __init__(self, fn: Callable, running_tasks: ListProxy, updated_repos: ValueProxy, lock: Lock):
        self.fn = fn
        self.running_tasks = running_tasks
        self.updated_repos = updated_repos
        self._lock = lock

    def __call__(self, *args):
        task_name = args[0][2]
        self.running_tasks.append(task_name)
        try:
            return self.fn(*args)
        finally:
            self._lock.acquire()
            self.running_tasks.remove(task_name)
            self.updated_repos.set(self.updated_repos.get() + 1)
            self._lock.release()


class ParallelRunner:
    def __init__(self, fn: Callable, pool_args: List[List[Any]], n_processes: int = 0):
        self._monitor_polling_period = 0.1
        if n_processes == 0:
            n_processes = cpu_count() * 2
        self._terminal_width = shutil.get_terminal_size().columns
        self._n_processes = n_processes
        self._pool_args = pool_args
        self._fn = fn
        self._lock = Manager().Lock()
        self._pool = Pool(
            processes=self._n_processes, initializer=self._child_init, initargs=(self._lock,)
        )
        self._verbose = pool_args[0][len(pool_args[0]) - 1]
        self._nb_repos = len(pool_args)
        self._stop_event = Event()
        self._running_tasks = Manager().list()
        self._updated_repos = Manager().Value('i', 0)
        self._monitored_fn = MonitoredFunction(self._fn, self._running_tasks, self._updated_repos, self._lock)

    def run(self) -> List[Any]:
        print(
            "Running ``%s`` with up to %d processes."
            % (self._fn.__name__, self._n_processes)
        )

        if self._verbose:
            results = self._pool.map_async(
                func=self._fn, iterable=self._pool_args
            ).get()
            self._pool.close()
            self._pool.join()
        else:
            monitor_thread = Thread(target=self._monitor, daemon=True)
            monitor_thread.start()
            results = self._pool.map_async(
                func=self._monitored_fn, iterable=self._pool_args
            ).get()
            self._pool.close()
            self._pool.join()
            self._stop_event.set()
            monitor_thread.join()
        return results

    def _monitor(self):
        last_output = ""
        while not self._stop_event.is_set():
            current = list(self._running_tasks)
            current_line = ", ".join(current)

            if current_line != last_output:
                truncated = f"Updating [{self._updated_repos.get()}/{self._nb_repos}] ({current_line})"
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

        sys.stdout.write("\r" + " " * len(last_output) + "\r")
        sys.stdout.flush()

    @staticmethod
    def _clear_lines(n):
        for _ in range(n):
            sys.stdout.write("\x1b[1A")
            sys.stdout.write("\x1b[2K")

    @staticmethod
    def check_results(results, op):
        """Function used to check the results of ParallelRunner.

        NOTE: This function was originally located in the shell module of
        swift_build_support and should eventually be replaced with a better
        parallel implementation.
        """

        fail_count = 0
        if results is None:
            return 0
        for r in results:
            if r is not None:
                if fail_count == 0:
                    print("======%s FAILURES======" % op)
                fail_count += 1
                if isinstance(r, str):
                    print(r)
                    continue
                print("%s failed (ret=%d): %s" % (r.repo_path, r.ret, r))
                if r.stderr:
                    print(r.stderr.decode())
        return fail_count

    @staticmethod
    def _child_init(lck):
        global lock
        lock = lck
