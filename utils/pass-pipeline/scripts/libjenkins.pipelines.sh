#!/bin/bash

set +x
set +u

# This is a file which defines helper routines based off of libjenkins for
# working with pipelines. You must source libjenkins before you call this.

###########
# Helpers #
###########

map_array() {
    local ARRAY=( $1 )
    local PATTERN="$2"

    for p in ${ARRAY[@]}; do
        echo $p
    done | xargs -I elt printf "$PATTERN" elt
}

#################
# Main Routines #
#################

PIPELINES=( "PreSpecialize" "HighLevel" "EarlyLoopOpt" "MidLevelOpt" "Lower" "LowLevel" "LateLoopOpt" )
build_bni_with_slice_pipelines_disabled() {
    local SCRIPT=$1
    local OUTPUT_DIR=$2
    local PIPELINE_LENGTH="${#PIPELINES[@]}"
    local SEQ_END_POINT=$(($PIPELINE_LENGTH-1))

    for i in $(seq $SEQ_END_POINT); do
        PIPELINE_SLICE="${PIPELINES[@]:$i}"
        DATA_FILE=$(printf "$OUTPUT_DIR/pipeline-slice-%.2d-disabled-pipeline.json" $i)
        PIPELINE_SLICE_OPTIONS=$(map_array "${PIPELINE_SLICE[@]}" " --disable-passpipeline %s ")
        $SCRIPT $PIPELINE_SLICE_OPTIONS > $DATA_FILE
        local EXTRA_FLAGS="-DSWIFT_EXTRA_FLAGS='-Xfrontend;-external-pass-pipeline-filename;-Xfrontend;$DATA_FILE'"
        echo "Building Stdlib with Slice: ${PIPELINES[@]::$i}..."
        build_swift_stdlib SWIFT_USE_OPTIMIZED=1 SWIFT_USE_ASSERTIONS=1 SWIFT_EXTRA_CMAKE_FLAGS="$EXTRA_FLAGS" SWIFT_STDLIB_PLATFORMS=macosx-10.9-internal-x86_64
    done
}

build_bni_with_individual_pipelines_disabled() {
    local SCRIPT=$1
    local OUTPUT_DIR=$2
    local PIPELINE_LENGTH="${#PIPELINES[@]}"
    local SEQ_END_POINT=$(($PIPELINE_LENGTH-1))

    for pipeline in "${PIPELINES[@]}"; do
        DATA_FILE=$(printf "$OUTPUT_DIR/%s-disabled-pipeline.json" $pipeline)
        $SCRIPT --disable-passpipeline "$pipeline" > $DATA_FILE
        local EXTRA_FLAGS="-DSWIFT_EXTRA_FLAGS='-Xfrontend;-external-pass-pipeline-filename;-Xfrontend;$DATA_FILE'"
        echo "Building Stdlib with pass manager $pipeline disabled."
        build_swift_stdlib SWIFT_USE_OPTIMIZED=1 SWIFT_USE_ASSERTIONS=1 SWIFT_EXTRA_CMAKE_FLAGS="$EXTRA_FLAGS" SWIFT_STDLIB_PLATFORMS=macosx-10.9-internal-x86_64
    done
}

PASSES=( "ABCOpt" "AllocBoxToStack" "COWArrayOpts" "CSE" "CapturePromotion" "CapturePropagation" "ClosureSpecializer" "CodeMotion" "CopyForwarding" "DCE" "DeadFunctionElimination" "DeadObjectElimination" "Devirtualizer" "EarlyInliner" "FunctionSignatureOpts" "GenericSpecializer" "GlobalARCOpts" "GlobalLoadStoreOpts" "GlobalOpt" "InlineCaches" "InstCount" "LICM" "LateInliner" "LoopRotate" "LowerAggregateInstrs" "MandatoryInlining" "Mem2Reg" "NoReturnFolding" "PerfInliner" "PerformanceConstantPropagation" "SILCleanup" "SILCombine" "SILLinker" "SROA" "SimplifyCFG" "SwiftArrayOpts" )

build_bni_with_individual_passes_disabled() {
    local SCRIPT=$1
    local OUTPUT_DIR=$2
    local PIPELINE_LENGTH="${#PIPELINES[@]}"
    local SEQ_END_POINT=$(($PIPELINE_LENGTH-1))

    for pass in "${PASSES[@]}"; do
        DATA_FILE=$(printf "$OUTPUT_DIR/%s-disabled-pass.json" $pass)
        $SCRIPT --disable-pass "$pass" > $DATA_FILE
        local EXTRA_FLAGS="-DSWIFT_EXTRA_FLAGS='-Xfrontend;-external-pass-pipeline-filename;-Xfrontend;$DATA_FILE'"
        echo "Building Stdlib with pass $pass disabled."
        build_swift_stdlib SWIFT_USE_OPTIMIZED=1 SWIFT_USE_ASSERTIONS=1 SWIFT_EXTRA_CMAKE_FLAGS="$EXTRA_FLAGS" SWIFT_STDLIB_PLATFORMS=macosx-10.9-internal-x86_64
    done
}

set -u
set -x
