#!/usr/bin/env python

import os
import sys
import subprocess
import argparse

PIPELINES = ["PreSpecialize", "HighLevel", "EarlyLoopOpt", "MidLevelOpt", "Lower", "LowLevel", "LateLoopOpt"]
PASSES = ["ABCOpt", "AllocBoxToStack", "COWArrayOpts", "CSE", "CapturePromotion", "CapturePropagation", "ClosureSpecializer", "CodeMotion", "CopyForwarding", "DCE", "DeadFunctionElimination", "DeadObjectElimination", "Devirtualizer", "EarlyInliner", "FunctionSignatureOpts", "GenericSpecializer", "GlobalARCOpts", "GlobalLoadStoreOpts", "GlobalOpt", "InlineCaches", "InstCount", "LICM", "LateInliner", "LoopRotate", "LowerAggregateInstrs", "MandatoryInlining", "Mem2Reg", "NoReturnFolding", "PerfInliner", "PerformanceConstantPropagation", "SILCleanup", "SILCombine", "SILLinker", "SROA", "SimplifyCFG", "SwiftArrayOpts"]

DEFAULT_PRESENTS = "--preset=buildbot_incremental,tools=RA,stdlib=RA"

def run_build_script_with_data_file(build_script, data_file):
    build_script_args = [build_script, DEFAULT_PRESENTS, "--extra-swift-args='^Swift$;-Xfrontend\;-external-pass-pipeline-filename\;-Xfrontend\;%s'" % data_file]
    sys.stdout.write("Running build script with: %s\n" % ' '.join(build_script_args))
    subprocess.check_call(build_script_args)

def build_disable_slice_pipelines(pipeline_script, build_script, output_dir):
    pipeline_range = range(len(PIPELINES))
    for i in pipeline_range:
        # Create a list consisting of ['--disable-passpipeline', pipeline_range[0], '--disable-passpipeline', pipeline_range[1]
        pipeline_args = [pipeline_script] + zip(['--disable-passpipeline']*(i+1), pipeline_range[:i+1])
        data_file = os.path.join(output_dir, "pipeline-slice-%.2d-disabled-pipeline.json" % i)
        with open(data_file, 'w') as f:
            subprocess.check_call(pipeline_args, stdout=f)
        run_build_script_with_data_file(build_script, data_file)

def build_disable_individual_passes(pipeline_script, build_script, output_dir):
    for p in PASSES:
        data_file = os.path.join(output_dir, "%s-disabled-pass.json" % p)
        with open(data_file, 'w') as f:
            subprocess.check_call([pipeline_script, '--disable-pass', p], stdout=f)
        run_build_script_with_data_file(build_script, data_file)

def main():
    parser = argparse.ArgumentParser(description="Run build-script with various passes disabled")
    parser.add_argument('action', choices=['disable_slice_pipelines', 'disable_individual_passes'])
    parser.add_argument('pipeline_script')
    parser.add_argument('build_script')
    parser.add_argument('output_dir')

    args = parser.parse_args()

    # TODO: I think I can get argparse to do this for me.
    if args.action == "disable_slice_pipelines":
        build_disable_slice_pipelines(args.pipeline_script, args.build_script, args.output_dir)
    elif args.action == "disable_individual_passes":
        build_disable_individual_passes(args.pipeline_script, args.build_script, args.output_dir)
    raise RuntimeError("Out of sync with choices")

if __name__ == "__main__":
    main()
