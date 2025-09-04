from multiprocessing.managers import ListProxy
import sys
from multiprocessing import Lock, Pool, cpu_count, Manager
import time
from typing import Callable, List, Any
from threading import Thread, Event


class MonitoredFunction:
    def __init__(self, fn: Callable, running_tasks: ListProxy):
        self.fn = fn
        self.running_tasks = running_tasks

    def __call__(self, *args):
        task_name = args[0][2]
        self.running_tasks.append(task_name)
        try:
            return self.fn(*args)
        finally:
            self.running_tasks.remove(task_name)


class ParallelRunner:
    def __init__(self, fn: Callable, pool_args: List[List[Any]], n_processes: int = 0):
        self._monitor_polling_period = 0.1
        if n_processes == 0:
            n_processes = cpu_count() * 2
        self._n_processes = n_processes
        self._pool_args = pool_args
        self._fn = fn
        lock = Lock()
        self._pool = Pool(
            processes=self._n_processes, initializer=self._child_init, initargs=(lock,)
        )
        self._quiet = pool_args[0][len(pool_args[0]) - 1]
        self._stop_event = Event()
        self._running_tasks = Manager().list()
        self._monitored_fn = MonitoredFunction(self._fn, self._running_tasks)

    def run(self) -> List[Any]:
        print(
            "Running ``%s`` with up to %d processes."
            % (self._fn.__name__, self._n_processes)
        )

        if self._quiet:
            monitor_thread = Thread(target=self._monitor, daemon=True)
            monitor_thread.start()
            results = self._pool.map_async(
                func=self._monitored_fn, iterable=self._pool_args
            ).get()
            self._pool.close()
            self._pool.join()
            self._stop_event.set()
            monitor_thread.join()
        else:
            results = self._pool.map_async(
                func=self._fn, iterable=self._pool_args
            ).get()
            self._pool.close()
            self._pool.join()
        return results

    def _monitor(self):
        last = None
        last_printed_lines = 0
        while not self._stop_event.is_set():
            current = list(self._running_tasks)
            if current != last:
                self._clear_lines(last_printed_lines)
                last_printed_lines = 0
                for repo in current:
                    sys.stdout.write(f"[{repo}]")
                    sys.stdout.write("\n")
                    last_printed_lines += 1
                sys.stdout.flush()
                last = current
            time.sleep(self._monitor_polling_period)
        self._clear_lines(last_printed_lines)

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
                print("%s failed (ret=%d): %s" % (r.repo_path, r.ret, r))
                fail_count += 1
                if r.stderr:
                    print(r.stderr)
        return fail_count

    @staticmethod
    def _child_init(lck):
        global lock
        lock = lck
