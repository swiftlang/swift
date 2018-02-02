function fail () {
   echo "Failed."
   exit 255
}

# Check System Integrity Protection status
function checkSIP
{
    local _STATUS=1

    # Doing a late check on csrutil so we can get proper OS check in main()
    CSRUTIL=/usr/bin/csrutil
    [[ ! -x "${CSRUTIL}" ]] && exitWithMsg "${CSRUTIL} not an executable"

    local _RESULT="$( "${CSRUTIL}" status )"
    if [[ "${_RESULT}" =~ "System Integrity Protection status: disabled." ]]; then
        # SIP disabled -- can install
        _STATUS=0
    elif [[ "${_RESULT}" =~ "System Integrity Protection status: disabled (Apple Internal)." ]]; then
        # SIP disabled (Apple Internal) -- can install
        _STATUS=0
    elif [[ "${_RESULT}" =~ "System Integrity Protection status: enabled (Apple Internal)." ]]; then
        # SIP enabled but it has Apple Internal magic -- can install
        _STATUS=0
    elif [[ "${_RESULT}" =~ "System Integrity Protection status: enabled (Custom Configuration)." ]]; then
        if [[ "${_RESULT}" =~ "Filesystem Protections: disabled" ]]; then
            # SIP enabled but it is custom configured and its filesystem projection is disabled, can install
            _STATUS=0
        elif [[ "${_RESULT}" =~ "Apple Internal: enabled" ]]; then
            # SIP enabled but it has Apple Internal magic -- can install
            _STATUS=0
        elif [[ "${_RESULT}" =~ "Filesystem Protections: enabled" ]]; then
            _STATUS=2
        fi
    fi
    return "${_STATUS}"
}


if [ ! $1 ]; then
   echo "You need to pass a valid version of ANI"
   exit 255
fi
VERSION=$1

echo "Downloading AppleNetInstall-$VERSION..."
curl https://ani.apple.com/Downloads/AppleNetInstall-$VERSION.dmg -o /tmp/AppleNetInstall-$VERSION.dmg || fail

echo "\nMounting disk image..."
hdiutil attach -quiet -mountpoint /Volumes/AppleNetInstall-$VERSION /tmp/AppleNetInstall-$VERSION.dmg || fail

echo "Terminating any running copies of Apple Net Install..."
pkill -9 -f Apple\ Net\ Install
launchctl remove com.apple.bni.applenetinstalld

checkSIP
STATUS=$?

if [ $STATUS -eq 0 ]; then
   DEST="/AppleInternal/Applications"
   echo "Installing to $DEST..."
   sudo rm -rf $DEST/Apple\ Net\ Install.app
   sudo ditto "/Volumes/AppleNetInstall-$VERSION/Apple Net Install.app" "$DEST/Apple Net Install.app" || fail
   sudo chown -R $USER:admin $DEST/Apple\ Net\ Install.app
   echo "Done."
   open $DEST/Apple\ Net\ Install.app
else
   DEST="/Applications"
   echo "Installing to $DEST..."
   rm -rf /Applications/Apple\ Net\ Install.app
   ditto "/Volumes/AppleNetInstall-$VERSION/Apple Net Install.app" "$DEST/Apple Net Install.app" || fail
   echo "Done."
   open $DEST/Apple\ Net\ Install.app
fi

exit 0
