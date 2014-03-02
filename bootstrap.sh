#!/bin/sh
CORE_TOP=`pwd`
export CORE_TOP

CURLBIN=`which curl`
if ! test -n "CURLBIN"; then
    display_error "Error: curl is required. Add it to 'PATH'"
    exit 1
fi

DATAFILE=UnicodeData.txt
DATAURL=http://www.unicode.org/Public/UNIDATA/$DATAFILE
TARGET=$CORE_TOP/priv/$DATAFILE

if ! test -f $TARGET; then
    echo "==> Fetch unicode data"
    $CURLBIN --progress-bar $DATAURL -o $TARGET
fi
