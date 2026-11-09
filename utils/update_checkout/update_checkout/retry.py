import time
import functools
from typing import Callable, TypeVar, Any

T = TypeVar("T")


def is_retryable(error: BaseException) -> bool:
    """
    Returns whether a failure might succeed if retried. Exceptions can opt out
    of retrying by defining an ``is_retryable`` attribute that is ``False``.
    """
    return getattr(error, "is_retryable", True)


class NonRetryableFailures(Exception):
    """
    Raised by a retried function whose failures are all non-retryable, to stop
    retrying without waiting. ``fail_count`` is the number of failures.
    """

    is_retryable = False

    def __init__(self, fail_count: int):
        super().__init__(f"{fail_count} non-retryable failure(s)")
        self.fail_count = fail_count


def exponential_retry(
    max_retries: int = 3,
    base_delay: float = 10.0,
    max_delay: float = 600.0,
):
    """
    Retries with exponential backoff if the method returns a value > 0.

    A ``None`` return counts as success. An exception for which
    ``is_retryable`` returns ``False`` is re-raised immediately, without
    waiting or retrying.

    Args:
        max_retries (int): Maximum number of retry attempts. 0 is for no
                           retries, -1 is for an unlimited amount.
        base_delay (float): Initial delay in seconds.
        max_delay (float): Maximum delay between retries in seconds.
    """

    def decorator(func: Callable[..., T]) -> Callable[..., T]:
        @functools.wraps(func)
        def wrapper(*args: Any, **kwargs: Any) -> T:
            retries = 0

            while max_retries == -1 or retries <= max_retries:
                caught_exception = None
                try:
                    fail_count = func(*args, **kwargs)
                except Exception as e:
                    caught_exception = e
                    fail_count = 1

                if caught_exception is None and (
                    fail_count is None or fail_count <= 0
                ):
                    return fail_count

                if caught_exception is not None and not is_retryable(
                    caught_exception
                ):
                    raise caught_exception

                if retries == max_retries:
                    if caught_exception is not None:
                        raise caught_exception
                    return fail_count

                delay = min(base_delay * (4**retries), max_delay)

                if caught_exception is not None:
                    print(
                        f"Retry {retries + 1}/{max_retries}: Caught exception: {caught_exception}"
                    )
                else:
                    print(
                        f"Retry {retries + 1}/{max_retries}: There were {fail_count} failure{'s' if fail_count > 1 else ''}."
                    )
                print(f"Waiting {delay:.2f}s before retrying...")

                time.sleep(delay)
                retries += 1

            return fail_count

        return wrapper

    return decorator
