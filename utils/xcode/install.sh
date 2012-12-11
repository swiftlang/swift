#!/bin/bash

function printUsage {
	echo "Usage:" $0 "[-f] [--symlink | path/to/bin/swift]"
}

# Parse options
while true; do
	if [[ "$1" == "--symlink" ]]; then
		SYMLINK=1
		shift
		continue
	fi

	if [[ "$1" == "-f" ]]; then
		REMOVE_EXISTING=1
		shift
		continue
	fi

	if [[ "$1" == "-h" || "$1" == "--help" ]]; then
		printUsage
		exit 0
	fi

	break
done

if (( $# > 1 )); then
	printUsage
	exit 1
fi

REL_PATH=$(cd $(dirname $0) && pwd)

if (( $# == 1 )); then
	SWIFT_EXEC="$1"
	if [[ ! -e "$SWIFT_EXEC" ]]; then
		echo 'warning: no swift executable found at' "$SWIFT_EXEC"
	elif [[ -d "$SWIFT_EXEC" || ! -x "$SWIFT_EXEC" ]]; then
		printUsage
		exit 1
	fi
	if [[ ! -z "$SYMLINK" ]]; then
		echo 'warning: using --symlink with an explicit binary path'
		echo 'note: this will change the Swift.xcplugin in your repository'
		echo
	fi
	SWIFT_BUILD_BASE="$(dirname $(dirname "$SWIFT_EXEC"))"
fi

XC_PLUGIN_DST="$HOME/Library/Application Support/Developer/Shared/Xcode/Plug-ins/S.xcplugin"

if [[ -e "$XC_PLUGIN_DST" ]]; then
	if [[ -z "$REMOVE_EXISTING" ]]; then
		echo 'error: already installed at' "$XC_PLUGIN_DST"
		echo 'note: use -f to replace the existing install'
		exit 2
	else
		echo -n 'Moving existing plugin to trash...'
		osascript -s o -e "tell app \"Finder\" to delete POSIX file \"$XC_PLUGIN_DST\"" > /dev/null
		echo 'done'
	fi
fi

if [[ -z "$SYMLINK" ]]; then
	echo -n 'Copying to' "$XC_PLUGIN_DST..."
	cp -Rf "$REL_PATH/Swift.xcplugin" "$XC_PLUGIN_DST"
	echo 'done'
else
	echo -n 'Making a symbolic link at' "$XC_PLUGIN_DST..."
	ln -sf "$REL_PATH/Swift.xcplugin" "$XC_PLUGIN_DST"
	echo 'done'
fi

if [[ ! -z "$SWIFT_EXEC" ]]; then
	echo -n 'Setting default SWIFT_EXEC...'
	sed -i "" -e "s:\\\$(DEVELOPER_DIR)/Toolchains/XcodeDefault\.xctoolchain/usr/bin/swift:$SWIFT_EXEC:" "$XC_PLUGIN_DST/Contents/Resources/Swift.xcspec"

	SWIFT_BASE=$(dirname $(dirname "$SWIFT_EXEC"))
	sed -i "" -e "s:\\\$(DEVELOPER_DIR)/Toolchains/XcodeDefault\.xctoolchain/usr:$SWIFT_BASE:" "$XC_PLUGIN_DST/Contents/Resources/Swift.xcspec"
	echo 'done'
fi
