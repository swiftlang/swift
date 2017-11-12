#!/bin/bash

# Find the transitive closure of overlay dependencies for a single overlay.
# Runs the following command in a loop until the list stops growing:
# xcrun -sdk macosx clang -arch x86_64 -x objective-c - -M -fmodules < <(echo '@import SceneKit;@import AppKit;')
# There are two ways an overlay ``foo`` shows up in the xcrun output:
# 1) as Foo.framework
# 2) as /usr/include/foo/bar.h
# but not:
# 3) as /usr/include/foo/module.map or /usr/include/foo/modulemap.map
# Overlays that only show up as a module map are not a real dependency.
# Overlays that have a different name in Swift than in the output: Dispatch, ObjectiveC, XPC
# XCTest is hardcoded because this method doesn't work for it.

set -o pipefail
set -e

OVERLAY_NAME_ALTERNATION="AppKit|AssetsLibrary|AVFoundation|CallKit|CloudKit|Contacts|CoreAudio|CoreData|CoreGraphics|CoreImage|CoreLocation|CoreMedia|CryptoTokenKit|dispatch|Foundation|GameplayKit|GLKit|HomeKit|IOKit|Intents|MapKit|objc|OpenCL|os|Photos|QuartzCore|SafariServices|SceneKit|simd|SpriteKit|UIKit|WatchKit|XCTest|xpc"

function find_deps() {
    local OVERLAY_ARG=$1
    local SDK_ARG=$2
    local ARCH_ARG=$3

    local PROGRAM=""
    # shellcheck disable=SC2013
    for overlay in $(sed "s/;/ /g" <<< "$OVERLAY_ARG"); do
        regexp="ObjectiveC|objc|Dispatch|dispatch|XPC|xpc"
        if [[ ! $overlay =~ $regexp ]]; then
            PROGRAM+="@import $overlay;"
        fi
    done

    local DEPS
    DEPS=$(xcrun -sdk "$SDK_ARG" clang -arch "$ARCH_ARG" -x objective-c - -M -fmodules <<< "$PROGRAM" 2>&1)
    local ERROR_REGEX="(.*error:.*)"
    if [[ $DEPS =~ $ERROR_REGEX ]]; then
        echo "${BASH_REMATCH[1]}" >&2
        exit 123
    fi

    local REGEX="./Frameworks/(${OVERLAY_NAME_ALTERNATION}).framework/.*|.*/usr/include/(xpc|dispatch|os|objc|simd)/.*\.h"

    # shellcheck disable=SC1004
    IFS='\
    '
    local USED_OVERLAYS=""
    # shellcheck disable=SC2068
    for line in ${DEPS[@]}; do
        if [[ $line =~ $REGEX ]]; then
            if [[ ${BASH_REMATCH[1]} != "" ]]; then
                USED_OVERLAYS+="${BASH_REMATCH[1]};"
            elif [[ ${BASH_REMATCH[2]} == "dispatch" ]]; then
                USED_OVERLAYS+="Dispatch;"
            elif [[ ${BASH_REMATCH[2]} == "xpc" ]]; then
                USED_OVERLAYS+="XPC;"
            elif [[ ${BASH_REMATCH[2]} == "objc" ]]; then
                USED_OVERLAYS+="ObjectiveC;"
            elif [[ ${BASH_REMATCH[2]} != "" ]]; then
                USED_OVERLAYS+="${BASH_REMATCH[2]};"
            fi
        fi
    done

    # Remove last ;
    if [[ ${#USED_OVERLAYS} -gt 0 ]]; then
        USED_OVERLAYS=${USED_OVERLAYS%?}
    fi

    TRIMMED=$(tr ";" "\n" <<< "$USED_OVERLAYS" | sort | uniq | tr "\n" ";")
    echo "${TRIMMED%?}"
}

if [ "$#" -ne 3 ]; then
    echo "error: $0 needs 3 params, instead got $#"
    echo "usage: $0 Overlay sdk arch"
    echo "example: $0 CloudKit macosx x86_64"
    exit 234
fi

OVERLAY_ARG=$1
SDK_ARG=$2
ARCH_ARG=$3

OUT=$OVERLAY_ARG
LAST_OUT=$OVERLAY_ARG

# Hardcode XCTest because clang can't find its dependencies
if [[ $OVERLAY_ARG == "XCTest" ]]; then
    case "$SDK_ARG" in
        macosx)
            OUT="XCTest;AppKit;CoreData;CoreGraphics;CoreImage;Dispatch;Foundation;IOKit;ObjectiveC;QuartzCore;XPC;os"
            ;;
        iphoneos)
            OUT="XCTest;CoreAudio;CoreGraphics;CoreImage;CoreMedia;Dispatch;Foundation;ObjectiveC;QuartzCore;UIKit;os"
            ;;
        appletvos)
            OUT="XCTest;CoreGraphics;CoreImage;Dispatch;Foundation;ObjectiveC;QuartzCore;UIKit;os"
            ;;
        *)
            echo "Unknown SDK: $SDK_ARG"
            exit 345
            ;;
    esac
else
    while true; do
        OUT=$(find_deps "$LAST_OUT" "$SDK_ARG" "$ARCH_ARG")
        if [[ "$LAST_OUT" == "$OUT" ]]; then
            break
        fi
        LAST_OUT=$OUT
    done
fi

echo "${OUT}"
