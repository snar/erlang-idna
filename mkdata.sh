#!/bin/sh
set -e

SCRIPT=$(readlink $0 || true)
if [ -z $SCRIPT ]; then
    SCRIPT=$0
fi;
SCRIPT_DIR="$(cd `dirname "$SCRIPT"` && pwd -P)"


CURL_BIN=`which curl`
if ! test -n "CURLBIN"; then
    echo "Error: curl is required. Add it to 'PATH'"
    exit 1
fi

GEN_DIR=idna.d
DATA_FILE=UnicodeData.txt
DATA_URL=http://www.unicode.org/Public/UNIDATA/UnicodeData.txt
INCLUDE_FILE=$GEN_DIR/idna_unicode_data.hrl
MODULE_SRC=$GEN_DIR/idna_unicode_data.erl

# fetch data file

if [ ! -e "$DATA_FILE" ]; then
    $CURL_BIN -o $DATA_FILE $DATA_URL
fi

mkdir -p $GEN_DIR
cat <<EOF > $INCLUDE_FILE
-define(BY_CODE, #{
EOF
cat $DATA_FILE \
    | awk 'BEGIN{FS=";"}{if($1!=""){ printf("\"%s\" => {\"%s\",\"%s\",\"%s\"}\n", $1, $4, $6, $14) }};' \
    | sort \
    | uniq -w 25 \
    | awk '{print t $0;}; {t = ","} ' \
    >> $INCLUDE_FILE
echo "})." >> $INCLUDE_FILE


cat <<EOF >> $INCLUDE_FILE
-define(BY_KEY, #{
EOF
cat $DATA_FILE \
    | awk 'BEGIN{FS=";"}{if($6!=""){ printf("\"%s\" => \"%s\"\n", $6, $1) }};' \
    | sort \
    | uniq -w 25 \
    | awk '{print t $0;}; {t = ","} ' \
    >> $INCLUDE_FILE
echo "})." >> $INCLUDE_FILE

cat <<EOF > $MODULE_SRC
-module(idna_unicode_data).

-export([lookup/1]).
-export([decomposition/1]).

-include("idna_unicode_data.hrl").

lookup(Codepoint) ->
	maps:get(Codepoint, ?BY_CODE, false).

decomposition(Key) ->
	maps:get(Key, ?BY_KEY, false).
EOF

mkdir -p priv
erlc -I $INCLUDE_FILE -o priv/ $MODULE_SRC
