#!/usr/bin/env bash

function extract_images {
    pdfimages "$FILE_PATH" "$PREFIX"
}

function convert_rename_cleanup {
    convert * "${PREFIX}.png"
    rm *.ppm
}

function game_default {
    extract_images
    convert_and_rename
}

function game_palm-island {
    echo "Preparing Palm Island from "$FILE" ..."
    extract_images
    # remove background images
    for i in 01 03 05 07 11 13 15 17 19 21 23 25; do
        rm "${PREFIX}-0${i}.ppm"
    done
    convert_rename_cleanup
}

function print-usage {
    cat >&2 <<-EOF 
	Usage: $0 <file> [game]
	Puts files in directory "img". Fails if it exists.
	Some games that requires special actions, like cleaning up unused images.
	Example:
	$0 Palm-Island-Print-and-Play1.4.6.pdf palm-island
	Known games:
	- default: Generalt configuration. Just extract images and convert to png.
	- palm-island: Palm Island, based on Palm-Island-Print-and-Play1.4.6.pdf
	EOF
}    

if [ $# -ne 2 ]; then
    print-usage
    exit 1
fi

FILE="$1"
GAME=default
[ -n "$2" ] && GAME="$2"

FOLDER=img
PREFIX="img"

if [ ! -f "$FILE" ]; then
    echo "\"$FILE\" does not exist or is not a regular file!" >&2
    exit 3
fi

FILE_PATH=$(readlink -f "$FILE")

function game_exists {
    for g in default palm-island; do
        [ "$GAME" = "$g" ] && return 0
    done
    return 1
}

if ! game_exists; then
    print-usage
    exit 2
fi

mkdir "$FOLDER" && cd "$FOLDER" || exit 4

game_${GAME}
