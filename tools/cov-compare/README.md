# Coverage Compare

Coverage Compare generates coverage diffs between two coverage snapshots. It has two subcommands:

```bash
compare [-f {html|markdown}] [-o output-file-or-dir] <path-to-old-coverage-yaml> <path-to-new-coverage-yaml>
```

- Generates either an HTML or Markdown representation of the difference in coverage between the two files
- If HTML, the `output-file-or-dir` must be a directory name; if markdown, it must be a file.
    - Markdown outputs a summary which lists coverage percentages for the files covered by the profile data
    - HTML outputs a directory containing a summary, but with a detailed function-level diff for each of those files.

```bash
yaml [-o output-file] <path-to-coverage-swiftc> <path-to-profdata>
```

- Generates a YAML file containing coverage information that's normally split between the binary and profdata
- Suitable for a comparison by the `compare` subcommand
