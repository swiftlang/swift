#!/usr/bin/env python

import argparse
import json
import os
import sys
import textwrap

# Append the src dir
sys.path.append(os.path.join(os.path.dirname(
    os.path.dirname(os.path.abspath(__file__))), 'src'))

import pass_pipeline_library  # noqa (E402 module level import not at top of file)

import passes                 # noqa (E402)

normal_pipeline = list(pass_pipeline_library.normal_passpipelines())
pass_pipelines = [x.identifier for x in normal_pipeline]

parser = argparse.ArgumentParser(description=textwrap.dedent("""
Generate pass pipelines based off of the normal swift pipeline.
"""))

parser.add_argument('--disable-pass', nargs='*', help='Disable this pass',
                    choices=[x.name for x in passes.PASSES], action='append',
                    default=[])
parser.add_argument('--disable-passpipeline', nargs='*',
                    help='Disable this pass list', choices=pass_pipelines,
                    default=[], action='append')

args = parser.parse_args()

disabled_passes = sum(args.disable_pass, [])
disabled_passpipelines = sum(args.disable_passpipeline, [])

# First filter out pipelines.
normal_pipeline_generated = [x.generate()
                             for x in normal_pipeline
                             if x.identifier not in disabled_passpipelines]

# Then filter out specific passes.
for i in range(len(normal_pipeline_generated)):
    normal_pipeline_generated[i] = [
        x for x in normal_pipeline_generated[i] if x not in disabled_passes]

json.dump(normal_pipeline_generated, sys.stdout,
          sort_keys=True, indent=4, separators=(',', ': '))
