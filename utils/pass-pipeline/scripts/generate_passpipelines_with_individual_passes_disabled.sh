#!/bin/bash

# We should blow up if -x is enabled, but I am not sure how to check for it.
set -e
set -u

# We require an output directory to be passed in.
OUTPUT_DIR=$1

SCRIPT_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
SCRIPT="$SCRIPT_DIR/normal_pipeline.py"

PIPELINES=( "PreSpecialize" "HighLevel" "EarlyLoopOpt" "MidLevelOpt" "Lower" "LowLevel" "LateLoopOpt" )

PASSES=( "ABCOpt" "AllocBoxToStack" "COWArrayOpts" "CSE" "CapturePromotion" "CapturePropagation" "ClosureSpecializer" "CodeMotion" "CopyForwarding" "DCE" "DeadFunctionElimination" "DeadObjectElimination" "Devirtualizer" "EarlyInliner" "FunctionSignatureOpts" "GenericSpecializer" "GlobalARCOpts" "GlobalLoadStoreOpts" "GlobalOpt" "InlineCaches" "InstCount" "LICM" "LateInliner" "LoopRotate" "LowerAggregateInstrs" "MandatoryInlining" "Mem2Reg" "NoReturnFolding" "PerfInliner" "PerformanceConstantPropagation" "SILCleanup" "SILCombine" "SILLinker" "SROA" "SimplifyCFG" "SwiftArrayOpts" )

for pipeline in "${PIPELINES[@]}"; do
    DATA_FILE=$OUTPUT_DIR/$pipeline-disabled-pipeline.json
    $SCRIPT --disable-passpipeline $pipeline > $DATA_FILE
    echo "$DATA_FILE"
done

for pass in "${PASSES[@]}"; do
    DATA_FILE=$OUTPUT_DIR/$pass-disabled-pass.json
    $SCRIPT --disable-pass $pass > $DATA_FILE
    echo "$DATA_FILE"
done

set +e
set +u
set -x
