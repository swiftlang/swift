
import json
import random
import subprocess
import sys

import bug_reducer_utils


def construct_pipeline(p):
    return (p[0], {'execution_kind': p[1], 'passes': p[2:]})


def random_bug_finder(args):
    """Given a path to a sib file with canonical sil, attempt to find a perturbed
list of passes that the perf pipeline"""
    tools = bug_reducer_utils.SwiftTools(args.swift_build_dir)

    json_data = json.loads(subprocess.check_output(
        [tools.sil_passpipeline_dumper, '-Performance']))
    passes = sum((p[2:] for p in json_data if p[0] != 'EarlyModulePasses'), [])
    passes = ['-' + x[1] for x in passes]

    # We assume we have an early module passes that runs until fix point and
    # that is strictly not what is causing the issue.
    #
    # Everything else runs one iteration.
    early_module_passes = [p[2:] for p in json_data
                           if p[0] == 'EarlyModulePasses'][0]
    early_module_passes = ['-' + x[1] for x in early_module_passes]

    sil_opt_invoker = bug_reducer_utils.SILOptInvoker(args, tools,
                                                      early_module_passes)

    # Make sure that the base case /does/ crash.
    max_count = args.max_count
    for count in range(max_count):
        print("Running round %i/%i" % (count, max_count))
        random.shuffle(passes)
        filename = sil_opt_invoker.get_suffixed_filename(str(count))
        result = sil_opt_invoker.invoke_with_passlist(filename, passes)
        if result == 0:
            print("Success with PassList: %s" % (' '.join(passes)))
        else:
            print("Fail with PassList: %s" % (' '.join(passes)))
            print("Output File: %s" % filename)
            sys.exit(-1)


def add_parser_arguments(parser):
    """Add parser arguments for random_bug_reducer"""
    parser.set_defaults(func=random_bug_finder)
    parser.add_argument('input_file', help='The input file to optimize')
    parser.add_argument('--module-cache', help='The module cache to use')
    parser.add_argument('--sdk', help='The sdk to pass to sil-opt')
    parser.add_argument('--target', help='The target to pass to sil-opt')
    parser.add_argument('--resource-dir',
                        help='The resource-dir to pass to sil-opt')
    parser.add_argument('--work-dir',
                        help='Working directory to use for temp files',
                        default='bug_reducer')
    parser.add_argument('--module-name',
                        help='The name of the module we are optimizing')
    parser.add_argument('--max-count',
                        help='Maximum number of permutations to try before'
                        ' exiting',
                        default=100)
