import os
import threading
import time
import traceback
try:
    import Queue as queue
except ImportError:
    import queue

try:
    import win32api
except ImportError:
    win32api = None

try:
    import multiprocessing
except ImportError:
    multiprocessing = None

import lit.Test

###
# Test Execution Implementation

class LockedValue(object):
    def __init__(self, value):
        self.lock = threading.Lock()
        self._value = value

    def _get_value(self):
        self.lock.acquire()
        try:
            return self._value
        finally:
            self.lock.release()

    def _set_value(self, value):
        self.lock.acquire()
        try:
            self._value = value
        finally:
            self.lock.release()

    value = property(_get_value, _set_value)

class TestProvider(object):
    def __init__(self, tests, num_jobs, queue_impl, canceled_flag):
        self.canceled_flag = canceled_flag

        # Create a shared queue to provide the test indices.
        self.queue = queue_impl()
        for i in range(len(tests)):
            self.queue.put(i)
        for i in range(num_jobs):
            self.queue.put(None)

    def cancel(self):
        self.canceled_flag.value = 1

    def get(self):
        # Check if we are canceled.
        if self.canceled_flag.value:
          return None

        # Otherwise take the next test.
        return self.queue.get()

class Tester(object):
    def __init__(self, run_instance, provider, consumer):
        self.run_instance = run_instance
        self.provider = provider
        self.consumer = consumer

    def run(self):
        while True:
            item = self.provider.get()
            if item is None:
                break
            self.run_test(item)
        self.consumer.task_finished()

    def run_test(self, test_index):
        test = self.run_instance.tests[test_index]
        try:
            self.run_instance.execute_test(test)
        except KeyboardInterrupt:
            # This is a sad hack. Unfortunately subprocess goes
            # bonkers with ctrl-c and we start forking merrily.
            print('\nCtrl-C detected, goodbye.')
            os.kill(0,9)
        self.consumer.update(test_index, test)

class ThreadResultsConsumer(object):
    def __init__(self, display):
        self.display = display
        self.lock = threading.Lock()

    def update(self, test_index, test):
        self.lock.acquire()
        try:
            self.display.update(test)
        finally:
            self.lock.release()

    def task_finished(self):
        pass

    def handle_results(self):
        pass

class MultiprocessResultsConsumer(object):
    def __init__(self, run, display, num_jobs):
        self.run = run
        self.display = display
        self.num_jobs = num_jobs
        self.queue = multiprocessing.Queue()

    def update(self, test_index, test):
        # This method is called in the child processes, and communicates the
        # results to the actual display implementation via an output queue.
        self.queue.put((test_index, test.result))

    def task_finished(self):
        # This method is called in the child processes, and communicates that
        # individual tasks are complete.
        self.queue.put(None)

    def handle_results(self):
        # This method is called in the parent, and consumes the results from the
        # output queue and dispatches to the actual display. The method will
        # complete after each of num_jobs tasks has signalled completion.
        completed = 0
        while completed != self.num_jobs:
            # Wait for a result item.
            item = self.queue.get()
            if item is None:
                completed += 1
                continue

            # Update the test result in the parent process.
            index,result = item
            test = self.run.tests[index]
            test.result = result

            self.display.update(test)

def run_one_tester(run, provider, display):
    tester = Tester(run, provider, display)
    tester.run()

###

class Run(object):
    """
    This class represents a concrete, configured testing run.
    """

    def __init__(self, lit_config, tests):
        self.lit_config = lit_config
        self.tests = tests

    def execute_test(self, test):
        result = None
        start_time = time.time()
        try:
            result = test.config.test_format.execute(test, self.lit_config)

            # Support deprecated result from execute() which returned the result
            # code and additional output as a tuple.
            if isinstance(result, tuple):
                code, output = result
                result = lit.Test.Result(code, output)
            elif not isinstance(result, lit.Test.Result):
                raise ValueError("unexpected result from test execution")
        except KeyboardInterrupt:
            raise
        except:
            if self.lit_config.debug:
                raise
            output = 'Exception during script execution:\n'
            output += traceback.format_exc()
            output += '\n'
            result = lit.Test.Result(lit.Test.UNRESOLVED, output)
        result.elapsed = time.time() - start_time

        test.setResult(result)

    def execute_tests(self, display, jobs, max_time=None,
                      use_processes=False):
        """
        execute_tests(display, jobs, [max_time])

        Execute each of the tests in the run, using up to jobs number of
        parallel tasks, and inform the display of each individual result. The
        provided tests should be a subset of the tests available in this run
        object.

        If max_time is non-None, it should be a time in seconds after which to
        stop executing tests.

        The display object will have its update method called with each test as
        it is completed. The calls are guaranteed to be locked with respect to
        one another, but are *not* guaranteed to be called on the same thread as
        this method was invoked on.

        Upon completion, each test in the run will have its result
        computed. Tests which were not actually executed (for any reason) will
        be given an UNRESOLVED result.
        """

        # Choose the appropriate parallel execution implementation.
        consumer = None
        if jobs != 1 and use_processes and multiprocessing:
            try:
                task_impl = multiprocessing.Process
                queue_impl = multiprocessing.Queue
                canceled_flag =  multiprocessing.Value('i', 0)
                consumer = MultiprocessResultsConsumer(self, display, jobs)
            except:
                # multiprocessing fails to initialize with certain OpenBSD and
                # FreeBSD Python versions: http://bugs.python.org/issue3770
                # Unfortunately the error raised also varies by platform.
                self.lit_config.note('failed to initialize multiprocessing')
                consumer = None
        if not consumer:
            task_impl = threading.Thread
            queue_impl = queue.Queue
            canceled_flag = LockedValue(0)
            consumer = ThreadResultsConsumer(display)

        # Create the test provider.
        provider = TestProvider(self.tests, jobs, queue_impl, canceled_flag)

        # Install a console-control signal handler on Windows.
        if win32api is not None:
            def console_ctrl_handler(type):
                provider.cancel()
                return True
            win32api.SetConsoleCtrlHandler(console_ctrl_handler, True)

        # Install a timeout handler, if requested.
        if max_time is not None:
            def timeout_handler():
                provider.cancel()
            timeout_timer = threading.Timer(max_time, timeout_handler)
            timeout_timer.start()

        # If not using multiple tasks, just run the tests directly.
        if jobs == 1:
            run_one_tester(self, provider, consumer)
        else:
            # Otherwise, execute the tests in parallel
            self._execute_tests_in_parallel(task_impl, provider, consumer, jobs)

        # Cancel the timeout handler.
        if max_time is not None:
            timeout_timer.cancel()

        # Update results for any tests which weren't run.
        for test in self.tests:
            if test.result is None:
                test.setResult(lit.Test.Result(lit.Test.UNRESOLVED, '', 0.0))

    def _execute_tests_in_parallel(self, task_impl, provider, consumer, jobs):
        # Start all of the tasks.
        tasks = [task_impl(target=run_one_tester,
                           args=(self, provider, consumer))
                 for i in range(jobs)]
        for t in tasks:
            t.start()

        # Allow the consumer to handle results, if necessary.
        consumer.handle_results()

        # Wait for all the tasks to complete.
        for t in tasks:
            t.join()
