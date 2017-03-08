#!/usr/bin/env python

import argparse
import os
import subprocess
import sys
import textwrap

# Append the src dir
sys.path.append(os.path.join(os.path.dirname(
    os.path.dirname(os.path.abspath(__file__))), 'src'))

import passes  # noqa (E402 module level import not at top of file)

# TODO: This should not be hard coded.
PIPELINES = ["PreSpecialize", "HighLevel", "EarlyLoopOpt",
             "MidLevelOpt", "Lower", "LowLevel", "LateLoopOpt"]
PASSES = [p.name for p in passes.PASSES]
DEFAULT_PRESENTS = \
    "--preset=buildbot_incremental_extra_swift_args,tools=RA,stdlib=RD"


def run_build_script_with_data_file(build_script, data_file, verbose=False):
    build_script_args = [
        build_script,
        DEFAULT_PRESENTS,
        'extra_swift_args=^Swift$;-Xfrontend\;' +
        '-external-pass-pipeline-filename\;-Xfrontend\;%s' % data_file]
    sys.stdout.write("Running build script with: %s..." %
                     ' '.join(build_script_args))
    sys.stdout.flush()

    if not verbose:
        p = subprocess.Popen(
            build_script_args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        output = p.stdout.readlines()
        status = p.wait()
        if status == 0:
            sys.stdout.write(" Success!\n")
        else:
            sys.stdout.write(" Failure:\n")
        sys.stdout.write(output)
        sys.stdout.flush()
    else:
        p = subprocess.Popen(build_script_args)
        status = p.wait()
        if status == 0:
            sys.stdout.write(" Success!\n")
        else:
            sys.stdout.write(" Failure:\n")


def build_disable_slice_pipelines(**kwargs):
    pipeline_range = range(len(PIPELINES))

    def get_pipeline_args(script, iter):
        result = [script]
        for j in iter:
            result.extend(['--disable-passpipeline', PIPELINES[j]])
        return result

    for i in pipeline_range:
        pipeline_args = get_pipeline_args(
            kwargs['pipeline_script'], pipeline_range[:i + 1])
        data_file = os.path.join(
            kwargs['output_dir'],
            "pipeline-slice-%.2d-disabled-pipeline.json" % i)
        with open(data_file, 'w') as f:
            f.write(subprocess.check_output(pipeline_args))
        run_build_script_with_data_file(
            kwargs['build_script'], data_file, verbose=kwargs['verbose'])


def build_disable_individual_pass(**kwargs):
    pass_name = kwargs['pass_name']
    data_file = os.path.join(
        kwargs['output_dir'], "%s-disabled-pass.json" % pass_name)
    with open(data_file, 'w') as f:
        f.write(subprocess.check_output(
            [kwargs['pipeline_script'], '--disable-pass', pass_name]))
    run_build_script_with_data_file(
        kwargs['build_script'], data_file, verbose=kwargs['verbose'])


def build_disable_individual_passes(**kwargs):
    for p in PASSES:
        d = dict(kwargs)
        d['pass_name'] = p
        build_disable_individual_pass(**d)


def add_default_parser_args(p):
    p.add_argument('pipeline_script', help=textwrap.dedent("""
    The path to normal_pipeline.py. In the future could be generalized to take
    other files.
    """))
    p.add_argument('build_script', help=textwrap.dedent("""
    The path to build-script.
    """))
    p.add_argument('output_dir', help=textwrap.dedent("""
    The output directory to use.
    """))
    p.add_argument('-v', action='store_true', dest='verbose',
                   help=textwrap.dedent("""
    Emit verbose output from build-script.
    """))


def main():
    parser = argparse.ArgumentParser(
        description="Run build-script with various passes disabled")
    subparsers = parser.add_subparsers(help="The specific action to perform")

    slice_pipeline_parser = subparsers.add_parser(
        'disable_slice_pipelines',
        description=textwrap.dedent("""
        Go through all predefined pass pipelines and run build_script with only
        specific slices enabled. Currently what this means is that we perform
        the normal pipeline order, stopping after N pipelines have run.
    """))
    slice_pipeline_parser.set_defaults(func=build_disable_slice_pipelines)
    add_default_parser_args(slice_pipeline_parser)

    disable_individual_passes_parser = subparsers.add_parser(
        'disable_individual_passes',
        description=textwrap.dedent("""
        Loop over all predefines passes and run build_script once for each pass
        with that pass disabled.
    """))
    disable_individual_passes_parser.set_defaults(
        func=build_disable_individual_passes)
    add_default_parser_args(disable_individual_passes_parser)

    disable_individual_pass_parser = subparsers.add_parser(
        'disable_individual_pass',
        description=textwrap.dedent("""
    Run build-script disabling only the specified passes.
    """))
    disable_individual_pass_parser.add_argument(
        'pass_name',
        help="The pass to disable",
        choices=PASSES,
        type=str)
    disable_individual_pass_parser.set_defaults(
        func=build_disable_individual_pass)
    add_default_parser_args(disable_individual_pass_parser)

    args = parser.parse_args()
    args.func(**vars(args))


if __name__ == "__main__":
    main()
