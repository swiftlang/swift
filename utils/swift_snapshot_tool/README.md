
# Swift Snapshot Tool

Used to list and bisect nightly snapshots off of swift.org.

# Examples

## List snapshots

```
# xcrun swift run swift_snapshot_tool list
[INFO] Starting to download snapshot information from github. 
[INFO] Finished downloading snapshot information from github. 
0 swift-DEVELOPMENT-SNAPSHOT-2024-09-06-a
1 swift-DEVELOPMENT-SNAPSHOT-2024-09-05-a
2 swift-DEVELOPMENT-SNAPSHOT-2024-09-04-a
3 swift-DEVELOPMENT-SNAPSHOT-2024-08-29-a
...
```

The number to the left is just the number back through historical time that the
value is at. This allows for one to get a sense of the range of time in between
two bisect numbers since one can look at the range in between them and how time
varies.

## Bisect

```
xcrun swift run swift_snapshot_tool bisect --script $SCRIPT_NAME --workspace $DIR_TO_DOWNLOAD_TOOLCHAINS \
   --good-tag $OLDER_TAG_NAME_THAT_PASSES --bad-tag $NEWER_TAG_NAME_THAT_FAILS
```

bisect success is defined by `$SCRIPT_NAME` returning 0 as an exit code. All
other exit codes are considered a failure.

Options:

- workspace: This is the place where we will download toolchains to. Defaults to
  `/tmp/swift_snapshot_tool_workspace_v1`

- script: This is the script that should be run. We pass in the environment
  variables `SWIFT_EXEC` and `SWIFT_FRONTEND` to the subscript. If the script
  returns a zero exit code then the run is considered a succeess. If the script
  returns a non-zero exit code, then the run is considered a failure.

- good_tag: This is the older tag and is assumed to succeed by returning a zero
  exit code.

- bad_tag: This is the newer tag and is assumed to fail by returning a non-zero
  exit code.

- invert: This causes test.sh's result to be inverted. This allows one to bisect
  backwards from a good state to a bad state. This is useful to determine when
  an error was fixed.

- branch: This controls the specific branch of snapshots that are downloaded. By
  default uses development. Also supports the options 5.0 and 6.0.
