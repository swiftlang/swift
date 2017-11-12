#!/usr/bin/env bash

# timeout.sh <seconds> <command...>

# watchdog PID SECONDS
function watchdog() {
    (( seconds = "$2" ))
    while (( seconds > 0 ))
    do
        kill -0 $1 || exit
        sleep 1
        (( seconds -= 1 ))
    done

    echo "** timeout.sh: Timed out" 1>&2
    kill -ALRM $1
}

watchdog $$ $1 &
shift
exec "$@"
