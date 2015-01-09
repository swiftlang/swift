#!/usr/bin/env python

import os
import sys
import subprocess
import argparse

# Append the src dir
sys.path.append(os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 'src'))

import pass_pipeline_library
import passes

# TODO: This should not be hard coded.
PIPELINES = ["PreSpecialize", "HighLevel", "EarlyLoopOpt", "MidLevelOpt", "Lower", "LowLevel", "LateLoopOpt"]
PASSES = [p.name for p in passes.PASSES]
DEFAULT_PRESENTS = "--preset=buildbot_incremental_extra_swift_args,tools=RA,stdlib=RD"

def run_build_script_with_data_file(build_script, data_file):
    build_script_args = [build_script, DEFAULT_PRESENTS, 'extra_swift_args=^Swift$;-Xfrontend\;-external-pass-pipeline-filename\;-Xfrontend\;%s' % data_file]
    sys.stdout.write("Running build script with: %s\n" % ' '.join(build_script_args))
    subprocess.check_call(build_script_args)

def build_disable_slice_pipelines(pipeline_script, build_script, output_dir):
    pipeline_range = range(len(PIPELINES))

    def get_pipeline_args(script, iter):
        result = [script]
        for j in iter:
            result.extend(['--disable-passpipeline', PIPELINES[j]])
        return result

    for i in pipeline_range:
        pipeline_args = get_pipeline_args(pipeline_script, pipeline_range[:i+1])
        data_file = os.path.join(output_dir, "pipeline-slice-%.2d-disabled-pipeline.json" % i)
        with open(data_file, 'w') as f:
            f.write(subprocess.check_output(pipeline_args))
        run_build_script_with_data_file(build_script, data_file)

def build_disable_individual_pass(pass_name, pipeline_script, build_script, output_dir):
    data_file = os.path.join(output_dir, "%s-disabled-pass.json" % pass_name)
    with open(data_file, 'w') as f:
        f.write(subprocess.check_output([pipeline_script, '--disable-pass', pass_name]))
    run_build_script_with_data_file(build_script, data_file)

def build_disable_individual_passes(pipeline_script, build_script, output_dir):
    for p in PASSES:
        build_disable_individual_pass(p, pipeline_script, build_script, output_dir)

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
