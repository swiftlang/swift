import time
import functools
from typing import Callable, TypeVar, Any

T = TypeVar('T')

def exponential_retry(
    max_retries: int = 3,
    base_delay: float = 10.0,
    max_delay: float = 600.0,
):
    """
    Retries with exponential backoff if the method returns a value > 0.
    
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
                fail_count = func(*args, **kwargs)
                
                if fail_count <= 0:
                    return fail_count
                
                if retries == max_retries:
                    return fail_count
                
                delay = min(base_delay * (4 ** retries), max_delay)
                
                print(f"Retry {retries + 1}/{max_retries}: There were {fail_count} failure", end="")
                if fail_count > 1:
                    print("s", end="")
                print(".")
                print(f"Waiting {delay:.2f}s before retrying...")
                
                time.sleep(delay)
                retries += 1
            
            return fail_count
        
        return wrapper
    return decorator
