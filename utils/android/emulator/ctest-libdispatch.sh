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

UTILITY="$SUB_DIR/bsdtestharness"
if [ ! -f "$UTILITY" ]; then
    echo "Utility not found: $UTILITY."
    exit 1
fi

# FIXME: Add this script to libdispatch and let CMake inject actual test at configuration time
TESTS="apply api debug queue_finalizer overcommit context_for_key after timer timer_short timer_timeout sema timer_bit31 timer_bit63 timer_set_time data io_muxed io_net io_pipe io_pipe_close select c99 plusplus"
COUNT=$(echo "$TESTS" | tr ' ' '\n' | wc -l)
echo "Found $COUNT test-cases in $SUB_DIR"

# Tests depend on libdispatch.so and libBlocksRuntime.so
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$SUB_DIR"

TIMOUT=120
FAILURE=0
chmod +x "$UTILITY"

for TEST in $TESTS; do
    chmod +x "$SUB_DIR/dispatch_$TEST"
    OUTPUT=$(timeout "${TIMOUT}s" "$UTILITY" "$SUB_DIR/dispatch_$TEST" 2>&1)
    EC=$?
    if [ $EC -eq 124 ]; then
        echo "Error: dispatch_$TEST timed out after $TIMOUT seconds"
        echo "************\nOutput:\n$OUTPUT\n************"
        FAILURE=1
    elif [ $EC -ne 0 ]; then
        echo "Error: dispatch_$TEST failed"
        echo "************\nOutput:\n$OUTPUT\n************"
        FAILURE=1
    else
        echo "dispatch_$TEST passed"
    fi
done

exit $FAILURE
