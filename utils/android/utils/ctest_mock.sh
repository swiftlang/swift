#!/system/bin/sh

if [ -z "$1" ]; then
    echo "Usage: $0 <sub-directory>"
    exit 1
fi

SUB_DIR=$1
if [ ! -d "$SUB_DIR" ]; then
    echo "Error: Directory $SUB_DIR does not exist"
    exit 1
fi

CTEST_FILE="$SUB_DIR/CTestTestfile.cmake"
if [ ! -f "$CTEST_FILE" ]; then
    echo "Error: $CTEST_FILE not found in $SUB_DIR"
    exit 1
fi

# TODO: Parse them from CTEST_FILE
TESTS="dispatch_apply dispatch_api dispatch_debug dispatch_queue_finalizer dispatch_overcommit dispatch_context_for_key dispatch_after dispatch_timer dispatch_timer_short dispatch_timer_timeout dispatch_sema dispatch_timer_bit31 dispatch_timer_bit63 dispatch_timer_set_time dispatch_data dispatch_io_muxed dispatch_io_net dispatch_io_pipe dispatch_io_pipe_close dispatch_select dispatch_c99 dispatch_plusplus"

COUNT=$(echo "$TESTS" | tr ' ' '\n' | wc -l)
echo "Found $COUNT test-cases in $SUB_DIR"

UTILITY="$SUB_DIR/bsdtestharness"
if [ ! -f "$UTILITY" ]; then
    echo "Utility not found: $UTILITY."
    exit 1
fi

chmod +x "$UTILITY"

# We need libdispatch.so and libBlocksRuntime.so
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$SUB_DIR"

# TODO: Parse the timeout as well from CTestTestfile.cmake?
TIMOUT=120
FAILURE=0

for TEST in $TESTS; do
    chmod +x "$SUB_DIR/$TEST"
    OUTPUT=$(timeout "${TIMOUT}s" "$UTILITY" "$SUB_DIR/$TEST" 2>&1)
    EC=$?
    if [ $EC -eq 124 ]; then
        echo "Error: $TEST timed out after $TIMOUT seconds"
        echo "************\nOutput:\n$OUTPUT\n************"
        FAILURE=1
    elif [ $EC -ne 0 ]; then
        echo "Error: $TEST failed"
        echo "************\nOutput:\n$OUTPUT\n************"
        FAILURE=1
    else
        echo "$TEST passed"
    fi
done

exit $FAILURE
