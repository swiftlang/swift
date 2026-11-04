# compare-solver-stat

Compiles Swift test files with several compilers and compares their `-stats-output-dir` statistics. It
prints a per-run table and summary tables, and also saves them as `report.md`.

```
utils/swift-dev-utils/run compare-solver-stat --help
utils/swift-dev-utils/run compare-solver-stat --out ../compare_stats --dir test/Sema
```

By default it compiles every `.swift` file under
`validation-test/Sema/type_checker_perf`. `--dir` picks another directory, and
a file argument compiles just that file.

## Columns

Each compiler has two columns, `NAME-off` and `NAME-on`. The on column adds the
experimental solver flags (`--on-flags`).

- `main`: the commit `main@origin` points to, built from source
- `wip`: the working directory as it was when the tool started
- extra compilers: prebuilt swift.org snapshots, from `--snapshot NAME=DATE`

Percentages and the summary are relative to the baseline column, `main-off`
unless `--baseline` says otherwise. A second summary compares pairs of columns,
`wip-on` against `main-on` unless `--compare MEASURED:REFERENCE` says
otherwise.

## Building

The tool builds `swift-frontend` twice, first the working directory and then
`main@origin`. It refuses to do so unless the repository is in a clean state,
and it puts the repository back afterwards, also when a build fails. It works
with jj and with git (`--vcs`). The build output goes to
`logs/build-wip.log` and `logs/build-main.log`.

`--skip-build` reuses the compilers saved by an earlier run.

`--swift-dir` sets the swift checkout. By default it is the checkout the tool
was built from, and the default test directory, build directory and
swift_snapshot_tool package are found from it.

## RUN lines

Each `RUN:` line at the top of a test file is one run, with that line's flags.
A file with several RUN lines has one row per line, named `file#1`, `file#2`,
and so on.

- `-verify` is dropped, and `-solver-scope-threshold=N` is not passed. N is
  shown instead, and each scope count is marked `over` or `under` it.
- Lines that run something other than the frontend, or that use substitutions
  such as `%t`, are skipped and listed when the tool starts.
- A compile that exits with an error still counts; the tests expect errors. A
  compile that writes no statistics shows `fail`, and one that the compiler
  rejects with an unknown argument shows `skip`. A column that skips every run
  is left out.

`--extra` and `--on-flags` take a list of flags, split at whitespace without
quote handling. A single flag, as in `--on-flags -x`, works too.

## Output

Everything goes into the `--out` directory, `compare_stats` by default:

- `compilers/main`, `compilers/wip`: the saved compilers
- `stats/COLUMN/RUN/`: the statistics of each compile
- `logs/COLUMN/RUN.log`: the command and output of each compile
- `report.md`: the report, unless `--no-report` is given

Like `crash-reduce`, the tool uses swift-subprocess, so it is not built when
`SWIFTCI_USE_LOCAL_DEPS` is set.
