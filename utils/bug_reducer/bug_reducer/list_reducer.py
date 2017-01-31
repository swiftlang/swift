
import random

TESTRESULT_NOFAILURE = "NoFailure"
TESTRESULT_KEEPSUFFIX = "KeepSuffix"
TESTRESULT_KEEPPREFIX = "KeepPrefix"
TESTRESULTS = set([TESTRESULT_NOFAILURE, TESTRESULT_KEEPSUFFIX,
                   TESTRESULT_KEEPPREFIX])


class ListReducer(object):
    """Reduce lists of objects. Inspired by llvm bugpoint"""

    def __init__(self, l):
        self.target_list = l
        # Maximal number of allowed splitting iterations,
        # before the elements are randomly shuffled.
        self.max_iters_without_progress = 3

        # Maximal number of allowed single-element trim iterations. We add a
        # threshold here as single-element reductions may otherwise take a
        # very long time to complete.
        self.max_trim_iterations_without_back_jump = 3
        self.shuffling_enabled = True

        self.num_iters_without_progress = 0
        self.mid_top = 0
        self.max_iters = self.max_iters_without_progress

    def _reset_progress(self):
        self.max_iters = self.max_iters_without_progress
        self.num_iters_without_progress = 0

    def run_test(self, prefix, suffix):
        raise RuntimeError("Abstract method")

    def _should_continue(self, result):
        if result == TESTRESULT_KEEPPREFIX:
            # We are done, we have the base case and the base case fails.
            if len(self.target_list) == 1:
                return {'should_continue': False, 'result': True}
            else:
                # There is an error, and we can narrow it down further.
                return {'should_continue': True, 'result': None}

        if result == TESTRESULT_KEEPSUFFIX:
            raise RuntimeError("ListReducer internal error: Selected empty "
                               "set!")
        raise RuntimeError('Unknown test result: %s' % result)

    def _test_shuffle_slow_converging_list(self):
        if not self.shuffling_enabled or \
           self.num_iters_without_progress <= self.max_iters_without_progress:
            return

        print("*** Testing shuffled set...")
        shuffled_list = list(self.target_list)
        random.shuffle(shuffled_list)
        # TODO: Is this correct? I guess we are always doing something.
        self.num_iters_without_progress = 0

        # Check that the random shuffle does not lose the bug.
        (result, _, _) = self.run_test(shuffled_list, [])
        if result != TESTRESULT_KEEPPREFIX:
            # If we fail here, disable any further shuffling...
            self.shuffling_enabled = False
            print("*** Shuffling hides the bug...")
            return

        self.mid_top = len(shuffled_list)
        self.max_iters = self.max_iters + 2
        print("*** Shuffling does not hide the bug...")
        self.target_list = shuffled_list

    def _test_prefix_suffix(self, mid, prefix, suffix):
        (result, prefix, suffix) = self.run_test(prefix, suffix)

        if result == TESTRESULT_KEEPSUFFIX:
            # The property still holds. We can just drop the prefix
            # elements, and shorten the list to the "kept" elements.
            self.target_list = suffix
            self.mid_top = len(self.target_list)

            # Reset the progress threshold
            self._reset_progress()
            return False

        if result == TESTRESULT_KEEPPREFIX:
            # The predicate still holds, shorten the list to the prefix
            # elements.
            self.target_list = prefix
            self.mid_top = len(self.target_list)
            self._reset_progress()
            return False

        assert(result == TESTRESULT_NOFAILURE)
        # The property does not hold. Some of the elements we removed must
        # be necessary to maintain the property.
        self.mid_top = mid
        self.num_iters_without_progress = \
            self.num_iters_without_progress + 1
        return False

    def _trim_target_list(self):
        self.mid_top = len(self.target_list)
        self.max_iters = self.max_iters_without_progress

        # Binary split reduction loop
        while self.mid_top > 1:
            # If the loop doesn't make satisfying progress, try shuffling.
            # The purpose of shuffling is to avoid the heavy tails of the
            # distribution (improving the speed of convergence).
            self._test_shuffle_slow_converging_list()

            # Split the list into a prefix, suffix list and then run test on
            # those.
            mid = self.mid_top/2
            if not self._test_prefix_suffix(mid, self.target_list[:mid],
                                            self.target_list[mid:]):
                # If we returned false, then we did some sort of work and there
                # was not an error, so continue.
                continue

            # Otherwise, the test routine signaled an error, so return True to
            # signal error.
            return True
        # If we reach this point, return False, we have no further work we can
        # do.
        return False

    def _trim_try_backjump_and_trim_suffix(self):
        backjump_probability = 10
        if len(self.target_list) <= 2:
            return False
        changed = True
        trim_iters = 0

        # Trimming loop
        while changed:
            changed = False

            # If the binary split reduction loop made an unfortunate sequence
            # of splits, the trimming loop might be left off with a huge
            # number of remaining elements (large search space). Backjumping
            # out of that search space and attempting a different split can
            # significantly improve the convergence speed.
            if random.randint(0, 100) < backjump_probability:
                return True

            # Check interior elements, using an offset to make sure we do not
            # skip elements when we trim.
            offset = 0
            for i in range(1, len(self.target_list)-1):
                real_i = i + offset
                test_list = self.target_list[real_i:]
                (result, prefix, suffix) = self.run_test([], test_list)
                if result == TESTRESULT_KEEPSUFFIX:
                    # We can trim the list!
                    self.target_list = test_list
                    offset = offset - 1
                    changed = True
            if trim_iters >= self.max_trim_iterations_without_back_jump:
                return False
            trim_iters = trim_iters + 1

    def reduce_list(self):
        random.seed(0x6e5ea738)  # Seed the random number generator
        (result, self.target_list, kept) = self.run_test(self.target_list, [])
        assert(result in TESTRESULTS)
        (should_continue, result) = self._should_continue(result)
        if not should_continue:
            return result

        # Now try to trim the list.
        should_backjump = True
        while should_backjump:
            # If self._trim_target_list returns True, then we failed to
            # reduce. Bail!
            if self._trim_target_list():
                return False

            # Finally decide if we should back_jump
            should_backjump = self._trim_try_backjump_and_trim_suffix()

        # There are some failure and we've narrowed them down
        return True
